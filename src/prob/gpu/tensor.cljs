(ns prob.gpu.tensor
  "GPU-backed tensor runtime. Synchronous ops, async readback.
   All tensor operations return new Tensor records immediately;
   use to-clj or to-number for async readback to CPU."
  (:require [prob.gpu.device :as dev]
            [prob.gpu.shaders :as shaders]
            [promesa.core :as p]))

;; ---------------------------------------------------------------------------
;; Record
;; ---------------------------------------------------------------------------

(defrecord Tensor [buffer shape strides size dtype device])

(defn tensor? [x] (instance? Tensor x))

;; ---------------------------------------------------------------------------
;; Constants (lazy — GPUBufferUsage only exists after dev/init!)
;; ---------------------------------------------------------------------------

(defn- storage-usage []
  (bit-or js/GPUBufferUsage.STORAGE
          js/GPUBufferUsage.COPY_SRC
          js/GPUBufferUsage.COPY_DST))

(defn- staging-usage []
  (bit-or js/GPUBufferUsage.MAP_READ
          js/GPUBufferUsage.COPY_DST))

;; ---------------------------------------------------------------------------
;; Pipeline cache
;; ---------------------------------------------------------------------------

(defonce ^:private pipeline-cache (volatile! {}))

(defn- get-pipeline!
  "Get or create a compiled GPUComputePipeline, cached by shader source."
  [device shader-source]
  (if-let [p (get @pipeline-cache shader-source)]
    p
    (let [module   (.createShaderModule device #js {:code shader-source})
          pipeline (.createComputePipeline device
                     #js {:layout  "auto"
                          :compute #js {:module     module
                                        :entryPoint "main"}})]
      (vswap! pipeline-cache assoc shader-source pipeline)
      pipeline)))

;; ---------------------------------------------------------------------------
;; Buffer helpers
;; ---------------------------------------------------------------------------

(defn- create-storage-buffer!
  "Create a storage buffer of given byte size."
  [device byte-size]
  (.createBuffer device
    #js {:size  byte-size
         :usage (storage-usage)}))

(defn- create-staging-buffer!
  "Create a staging buffer for MAP_READ readback."
  [device byte-size]
  (.createBuffer device
    #js {:size  byte-size
         :usage (staging-usage)}))

(defn- copy-buffer-to-buffer!
  "Encode + submit a buffer-to-buffer copy."
  [device src dst byte-size]
  (let [encoder (.createCommandEncoder device)]
    (.copyBufferToBuffer encoder src 0 dst 0 byte-size)
    (.submit (.-queue device) #js [(.finish encoder)])))

;; ---------------------------------------------------------------------------
;; Shape utilities
;; ---------------------------------------------------------------------------

(defn- compute-strides
  "Row-major strides from shape vec."
  [shape-vec]
  (if (empty? shape-vec)
    []
    (let [n (count shape-vec)]
      (loop [i (dec n), acc (list 1), s 1]
        (if (zero? i)
          (vec acc)
          (let [new-s (* s (nth shape-vec i))]
            (recur (dec i) (cons new-s acc) new-s)))))))

(defn- infer-shape
  "Infer shape from nested vectors/sequences."
  [data]
  (if (number? data)
    []
    (let [data (vec data)]
      (if (empty? data)
        [0]
        (if (number? (first data))
          [(count data)]
          (into [(count data)] (infer-shape (first data))))))))

(defn- flatten-data
  "Flatten nested vectors to a flat sequence of numbers."
  [data]
  (if (number? data)
    [data]
    (if (and (sequential? data) (not-empty data) (number? (first data)))
      (vec data)
      (vec (mapcat flatten-data data)))))

(defn- reshape-clj
  "Nest a flat vector into the given shape."
  [flat-vec shape-vec]
  (if (empty? shape-vec)
    (first flat-vec)
    (if (= 1 (count shape-vec))
      flat-vec
      (let [sub-size (apply * (rest shape-vec))
            n        (first shape-vec)]
        (mapv (fn [i]
                (reshape-clj (subvec flat-vec (* i sub-size) (* (inc i) sub-size))
                             (vec (rest shape-vec))))
              (range n))))))

;; ---------------------------------------------------------------------------
;; Dispatch helpers
;; ---------------------------------------------------------------------------

(defn- workgroup-count [n]
  (js/Math.ceil (/ n 64)))

(defn- dispatch-unary!
  "Dispatch unary shader: 1 input + 1 output. Returns new Tensor."
  [shader-source t]
  (let [device (:device t)
        n      (:size t)
        bytes  (* 4 n)
        out    (create-storage-buffer! device bytes)
        pl     (get-pipeline! device shader-source)
        bg     (.createBindGroup device
                 #js {:layout  (.getBindGroupLayout pl 0)
                      :entries #js [#js {:binding 0 :resource #js {:buffer (:buffer t)}}
                                    #js {:binding 1 :resource #js {:buffer out}}]})
        enc    (.createCommandEncoder device)
        pass   (.beginComputePass enc)]
    (.setPipeline pass pl)
    (.setBindGroup pass 0 bg)
    (.dispatchWorkgroups pass (workgroup-count n))
    (.end pass)
    (.submit (.-queue device) #js [(.finish enc)])
    (->Tensor out (:shape t) (:strides t) n :f32 device)))

(defn- dispatch-binary!
  "Dispatch binary shader: 2 inputs + 1 output. Broadcasting via modulo."
  [shader-source a b]
  (let [device   (:device a)
        out-size (max (:size a) (:size b))
        out-shape (if (>= (:size a) (:size b)) (:shape a) (:shape b))
        out-strides (compute-strides out-shape)
        bytes    (* 4 out-size)
        out      (create-storage-buffer! device bytes)
        pl       (get-pipeline! device shader-source)
        bg       (.createBindGroup device
                   #js {:layout  (.getBindGroupLayout pl 0)
                        :entries #js [#js {:binding 0 :resource #js {:buffer (:buffer a)}}
                                      #js {:binding 1 :resource #js {:buffer (:buffer b)}}
                                      #js {:binding 2 :resource #js {:buffer out}}]})
        enc      (.createCommandEncoder device)
        pass     (.beginComputePass enc)]
    (.setPipeline pass pl)
    (.setBindGroup pass 0 bg)
    (.dispatchWorkgroups pass (workgroup-count out-size))
    (.end pass)
    (.submit (.-queue device) #js [(.finish enc)])
    (->Tensor out out-shape out-strides out-size :f32 device)))

(defn- dispatch-ternary!
  "Dispatch ternary shader: 3 inputs + 1 output (where)."
  [shader-source cond-t a b]
  (let [device   (:device a)
        out-size (max (:size cond-t) (max (:size a) (:size b)))
        out-shape (cond
                    (>= (:size a) (max (:size cond-t) (:size b))) (:shape a)
                    (>= (:size b) (max (:size cond-t) (:size a))) (:shape b)
                    :else (:shape cond-t))
        out-strides (compute-strides out-shape)
        bytes    (* 4 out-size)
        out      (create-storage-buffer! device bytes)
        pl       (get-pipeline! device shader-source)
        bg       (.createBindGroup device
                   #js {:layout  (.getBindGroupLayout pl 0)
                        :entries #js [#js {:binding 0 :resource #js {:buffer (:buffer cond-t)}}
                                      #js {:binding 1 :resource #js {:buffer (:buffer a)}}
                                      #js {:binding 2 :resource #js {:buffer (:buffer b)}}
                                      #js {:binding 3 :resource #js {:buffer out}}]})
        enc      (.createCommandEncoder device)
        pass     (.beginComputePass enc)]
    (.setPipeline pass pl)
    (.setBindGroup pass 0 bg)
    (.dispatchWorkgroups pass (workgroup-count out-size))
    (.end pass)
    (.submit (.-queue device) #js [(.finish enc)])
    (->Tensor out out-shape out-strides out-size :f32 device)))

(defn- dispatch-reduction!
  "Multi-pass sum reduction. Each pass reduces by factor 64.
   Returns scalar tensor (shape [], size 1)."
  [t]
  (let [device (:device t)]
    (loop [input-buf (:buffer t)
           n         (:size t)
           temps     []]
      (let [num-groups (workgroup-count n)
            out-bytes  (* 4 num-groups)
            out-buf    (create-storage-buffer! device out-bytes)
            pl         (get-pipeline! device shaders/sum-reduction-shader)
            bg         (.createBindGroup device
                         #js {:layout  (.getBindGroupLayout pl 0)
                              :entries #js [#js {:binding 0 :resource #js {:buffer input-buf}}
                                            #js {:binding 1 :resource #js {:buffer out-buf}}]})
            enc        (.createCommandEncoder device)
            pass       (.beginComputePass enc)]
        (.setPipeline pass pl)
        (.setBindGroup pass 0 bg)
        (.dispatchWorkgroups pass num-groups)
        (.end pass)
        (.submit (.-queue device) #js [(.finish enc)])
        (if (<= num-groups 1)
          (do
            ;; Destroy intermediate buffers (not the original input)
            (doseq [tmp temps] (.destroy tmp))
            (->Tensor out-buf [] [] 1 :f32 device))
          (recur out-buf num-groups (conj temps out-buf)))))))

;; ---------------------------------------------------------------------------
;; Creation
;; ---------------------------------------------------------------------------

(defn tensor
  "Create a tensor from flat or nested data.
   Requires GPU to be initialized (call dev/init! first)."
  [data]
  (let [{:keys [device]} (dev/ctx)
        shape   (infer-shape data)
        flat    (flatten-data data)
        size    (count flat)
        strides (compute-strides shape)
        arr     (js/Float32Array. (clj->js flat))
        buf     (create-storage-buffer! device (.-byteLength arr))]
    (.writeBuffer (.-queue device) buf 0 arr)
    (->Tensor buf shape strides size :f32 device)))

(defn scalar
  "Create a scalar tensor (shape [], size 1)."
  [x]
  (let [{:keys [device]} (dev/ctx)
        arr (js/Float32Array. #js [x])
        buf (create-storage-buffer! device 4)]
    (.writeBuffer (.-queue device) buf 0 arr)
    (->Tensor buf [] [] 1 :f32 device)))

(defn zeros
  "Create a zero-filled tensor of given shape."
  [shape-vec]
  (let [{:keys [device]} (dev/ctx)
        size    (apply * shape-vec)
        strides (compute-strides shape-vec)
        buf     (create-storage-buffer! device (* 4 size))]
    ;; WebGPU spec guarantees zero-initialization
    (->Tensor buf shape-vec strides size :f32 device)))

(defn full
  "Create a tensor filled with the given value."
  [shape-vec value]
  (let [{:keys [device]} (dev/ctx)
        size    (apply * shape-vec)
        strides (compute-strides shape-vec)
        arr     (js/Float32Array. size)
        _       (.fill arr value)
        buf     (create-storage-buffer! device (.-byteLength arr))]
    (.writeBuffer (.-queue device) buf 0 arr)
    (->Tensor buf shape-vec strides size :f32 device)))

(defn ones
  "Create a tensor filled with 1.0."
  [shape-vec]
  (full shape-vec 1.0))

;; ---------------------------------------------------------------------------
;; Arithmetic
;; ---------------------------------------------------------------------------

(defn add [a b] (dispatch-binary! shaders/add-shader a b))
(defn subtract [a b] (dispatch-binary! shaders/subtract-shader a b))
(defn multiply [a b] (dispatch-binary! shaders/multiply-shader a b))
(defn divide [a b] (dispatch-binary! shaders/divide-shader a b))

(defn negative [t] (dispatch-unary! shaders/negative-shader t))
(defn exp [t] (dispatch-unary! shaders/exp-shader t))
(defn log [t] (dispatch-unary! shaders/log-shader t))
(defn sqrt [t] (dispatch-unary! shaders/sqrt-shader t))
(defn square [t] (dispatch-unary! shaders/square-shader t))
(defn abs [t] (dispatch-unary! shaders/abs-shader t))

;; ---------------------------------------------------------------------------
;; Comparisons
;; ---------------------------------------------------------------------------

(defn greater [a b] (dispatch-binary! shaders/greater-shader a b))
(defn less [a b] (dispatch-binary! shaders/less-shader a b))
(defn greater-equal [a b] (dispatch-binary! shaders/greater-equal-shader a b))
(defn less-equal [a b] (dispatch-binary! shaders/less-equal-shader a b))

(defn where
  "Element-wise select: where cond > 0, take a, else b."
  [cond-t a b]
  (dispatch-ternary! shaders/where-shader cond-t a b))

;; ---------------------------------------------------------------------------
;; Reductions
;; ---------------------------------------------------------------------------

(defn sum
  "Sum all elements. Returns scalar tensor."
  [t]
  (if (= 1 (:size t))
    ;; Already scalar — copy to new buffer to avoid aliasing
    (let [device (:device t)
          buf    (create-storage-buffer! device 4)]
      (copy-buffer-to-buffer! device (:buffer t) buf 4)
      (->Tensor buf [] [] 1 :f32 device))
    (dispatch-reduction! t)))

(defn mean
  "Mean of all elements. Returns scalar tensor."
  [t]
  (divide (sum t) (scalar (:size t))))

;; ---------------------------------------------------------------------------
;; Shape
;; ---------------------------------------------------------------------------

(defn shape [t] (:shape t))
(defn ndim [t] (count (:shape t)))
(defn size [t] (:size t))

(defn reshape
  "Zero-cost reshape — same buffer, new shape metadata.
   Product of new shape must equal tensor size."
  [t new-shape]
  (let [new-size (apply * new-shape)]
    (when (not= new-size (:size t))
      (throw (ex-info "Reshape size mismatch"
                      {:current-size (:size t) :new-size new-size :new-shape new-shape})))
    (->Tensor (:buffer t) new-shape (compute-strides new-shape) (:size t) :f32 (:device t))))

;; ---------------------------------------------------------------------------
;; Cleanup
;; ---------------------------------------------------------------------------

(defn dispose!
  "Destroy the GPU buffer backing this tensor."
  [t]
  (.destroy (:buffer t))
  nil)

;; ---------------------------------------------------------------------------
;; RNG
;; ---------------------------------------------------------------------------

(defonce ^:private *rng-counter* (volatile! 0))

(defn set-rng-seed!
  "Reset RNG seed counter for reproducibility."
  [seed]
  (vreset! *rng-counter* seed))

(defn rand-uniform
  "Generate uniform random tensor in [0, 1)."
  [shape-vec]
  (let [{:keys [device]} (dev/ctx)
        size    (apply * shape-vec)
        strides (compute-strides shape-vec)
        ;; Generate unique seeds per element
        base    @*rng-counter*
        _       (vswap! *rng-counter* + size)
        seeds   (js/Uint32Array. size)
        _       (dotimes [i size]
                  (aset seeds i (+ base i)))
        seed-buf (create-storage-buffer! device (.-byteLength seeds))
        _       (.writeBuffer (.-queue device) seed-buf 0 seeds)
        out-buf  (create-storage-buffer! device (* 4 size))
        pl       (get-pipeline! device shaders/uniform-rng-shader)
        bg       (.createBindGroup device
                   #js {:layout  (.getBindGroupLayout pl 0)
                        :entries #js [#js {:binding 0 :resource #js {:buffer seed-buf}}
                                      #js {:binding 1 :resource #js {:buffer out-buf}}]})
        enc      (.createCommandEncoder device)
        pass     (.beginComputePass enc)]
    (.setPipeline pass pl)
    (.setBindGroup pass 0 bg)
    (.dispatchWorkgroups pass (workgroup-count size))
    (.end pass)
    (.submit (.-queue device) #js [(.finish enc)])
    (.destroy seed-buf)
    (->Tensor out-buf shape-vec strides size :f32 device)))

(defn randn
  "Generate standard normal random tensor (mean 0, std 1) via Box-Muller."
  [shape-vec]
  (let [{:keys [device]} (dev/ctx)
        size    (apply * shape-vec)
        strides (compute-strides shape-vec)
        ;; Need 2x uniforms for Box-Muller
        n-uniforms (* 2 size)
        uniform-t  (rand-uniform [n-uniforms])
        out-buf    (create-storage-buffer! device (* 4 size))
        pl         (get-pipeline! device shaders/normal-rng-shader)
        bg         (.createBindGroup device
                     #js {:layout  (.getBindGroupLayout pl 0)
                          :entries #js [#js {:binding 0 :resource #js {:buffer (:buffer uniform-t)}}
                                        #js {:binding 1 :resource #js {:buffer out-buf}}]})
        enc        (.createCommandEncoder device)
        pass       (.beginComputePass enc)]
    (.setPipeline pass pl)
    (.setBindGroup pass 0 bg)
    (.dispatchWorkgroups pass (workgroup-count size))
    (.end pass)
    (.submit (.-queue device) #js [(.finish enc)])
    (dispose! uniform-t)
    (->Tensor out-buf shape-vec strides size :f32 device)))

;; ---------------------------------------------------------------------------
;; Readback (async)
;; ---------------------------------------------------------------------------

(defn to-number
  "Read back a scalar tensor. Returns Promise<number>."
  [t]
  (let [device  (:device t)
        staging (create-staging-buffer! device 4)]
    (copy-buffer-to-buffer! device (:buffer t) staging 4)
    (p/let [_ (.mapAsync staging js/GPUMapMode.READ)]
      (let [result (aget (js/Float32Array. (.slice (.getMappedRange staging) 0)) 0)]
        (.unmap staging)
        (.destroy staging)
        result))))

(defn to-clj
  "Read back tensor data. Returns Promise<vector> (nested to match shape)."
  [t]
  (let [device   (:device t)
        bytes    (* 4 (:size t))
        staging  (create-staging-buffer! device bytes)]
    (copy-buffer-to-buffer! device (:buffer t) staging bytes)
    (p/let [_ (.mapAsync staging js/GPUMapMode.READ)]
      (let [flat (vec (js/Float32Array. (.slice (.getMappedRange staging) 0)))]
        (.unmap staging)
        (.destroy staging)
        (if (empty? (:shape t))
          (first flat)
          (reshape-clj flat (:shape t)))))))
