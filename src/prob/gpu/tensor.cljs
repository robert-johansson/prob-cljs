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

(defn- uniform-usage []
  (bit-or js/GPUBufferUsage.UNIFORM
          js/GPUBufferUsage.COPY_DST))

(defn- create-uniform-buffer!
  "Create a small uniform buffer from a vec of u32 values."
  [device values]
  (let [arr (js/Uint32Array. (clj->js values))
        buf (.createBuffer device
              #js {:size  (.-byteLength arr)
                   :usage (uniform-usage)})]
    (.writeBuffer (.-queue device) buf 0 arr)
    buf))

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
;; Matmul
;; ---------------------------------------------------------------------------

(defn matmul
  "Matrix multiply A[M,K] × B[K,N] → C[M,N].
   1D inputs: [K] treated as [1,K] (left) or [K,1] (right), result squeezed."
  [a b]
  (let [device (:device a)
        a-shape (:shape a)
        b-shape (:shape b)
        a-ndim  (count a-shape)
        b-ndim  (count b-shape)
        ;; Promote 1D to 2D
        squeeze-left  (= a-ndim 1)
        squeeze-right (= b-ndim 1)
        [M K-a] (cond
                  (= a-ndim 0) (throw (ex-info "matmul: scalar input not supported" {:shape a-shape}))
                  squeeze-left  [1 (first a-shape)]
                  :else         [(first a-shape) (second a-shape)])
        [K-b N]  (cond
                   (= b-ndim 0) (throw (ex-info "matmul: scalar input not supported" {:shape b-shape}))
                   squeeze-right [(first b-shape) 1]
                   :else         [(first b-shape) (second b-shape)])
        _       (when (not= K-a K-b)
                  (throw (ex-info "matmul: inner dimensions mismatch"
                                  {:a-shape a-shape :b-shape b-shape :K-a K-a :K-b K-b})))
        K       K-a
        out-size (* M N)
        out-buf  (create-storage-buffer! device (* 4 out-size))
        dims-buf (create-uniform-buffer! device [M K N 0])
        pl       (get-pipeline! device shaders/matmul-shader)
        bg       (.createBindGroup device
                   #js {:layout  (.getBindGroupLayout pl 0)
                        :entries #js [#js {:binding 0 :resource #js {:buffer (:buffer a)}}
                                      #js {:binding 1 :resource #js {:buffer (:buffer b)}}
                                      #js {:binding 2 :resource #js {:buffer out-buf}}
                                      #js {:binding 3 :resource #js {:buffer dims-buf}}]})
        enc      (.createCommandEncoder device)
        pass     (.beginComputePass enc)
        wg-x     (js/Math.ceil (/ N 16))
        wg-y     (js/Math.ceil (/ M 16))]
    (.setPipeline pass pl)
    (.setBindGroup pass 0 bg)
    (.dispatchWorkgroups pass wg-x wg-y)
    (.end pass)
    (.submit (.-queue device) #js [(.finish enc)])
    (.destroy dims-buf)
    ;; Determine output shape with squeeze
    (let [out-shape (cond
                      (and squeeze-left squeeze-right) []      ;; dot product → scalar
                      squeeze-left                     [N]     ;; [K]×[K,N] → [N]
                      squeeze-right                    [M]     ;; [M,K]×[K] → [M]
                      :else                            [M N])
          out-strides (compute-strides out-shape)]
      (->Tensor out-buf out-shape out-strides out-size :f32 device))))

;; ---------------------------------------------------------------------------
;; Transpose
;; ---------------------------------------------------------------------------

(defn transpose
  "Physical transpose of 2D tensor [rows,cols] → [cols,rows].
   Scalar/1D → returned as-is (no-op)."
  [t]
  (let [sh (:shape t)]
    (if (<= (count sh) 1)
      ;; Scalar or 1D — no-op, return copy
      (let [device (:device t)
            bytes  (* 4 (:size t))
            buf    (create-storage-buffer! device bytes)]
        (copy-buffer-to-buffer! device (:buffer t) buf bytes)
        (->Tensor buf sh (:strides t) (:size t) :f32 device))
      ;; 2D transpose via shader
      (let [device  (:device t)
            rows    (first sh)
            cols    (second sh)
            n       (:size t)
            out-buf (create-storage-buffer! device (* 4 n))
            dims-buf (create-uniform-buffer! device [rows cols 0 0])
            pl      (get-pipeline! device shaders/transpose-shader)
            bg      (.createBindGroup device
                      #js {:layout  (.getBindGroupLayout pl 0)
                           :entries #js [#js {:binding 0 :resource #js {:buffer (:buffer t)}}
                                         #js {:binding 1 :resource #js {:buffer out-buf}}
                                         #js {:binding 2 :resource #js {:buffer dims-buf}}]})
            enc     (.createCommandEncoder device)
            pass    (.beginComputePass enc)]
        (.setPipeline pass pl)
        (.setBindGroup pass 0 bg)
        (.dispatchWorkgroups pass (workgroup-count n))
        (.end pass)
        (.submit (.-queue device) #js [(.finish enc)])
        (.destroy dims-buf)
        (let [out-shape [cols rows]]
          (->Tensor out-buf out-shape (compute-strides out-shape) n :f32 device))))))

;; ---------------------------------------------------------------------------
;; Slice
;; ---------------------------------------------------------------------------

(defn slice
  "Extract sub-tensor along dimension `dim`, indices [start, end).
   Fast path: dim-0 slices use copyBufferToBuffer (contiguous)."
  [t dim start end]
  (let [sh     (:shape t)
        ndims  (count sh)
        _      (when (or (< dim 0) (>= dim ndims))
                 (throw (ex-info "slice: dim out of range" {:dim dim :ndims ndims})))
        dim-sz (nth sh dim)
        _      (when (or (< start 0) (> end dim-sz) (> start end))
                 (throw (ex-info "slice: invalid bounds" {:dim dim :start start :end end :dim-size dim-sz})))
        slice-len (- end start)
        device    (:device t)]
    (if (zero? dim)
      ;; Fast path: first-dim slice is a contiguous block
      (let [inner-size (apply * (rest sh))
            src-offset (* start inner-size 4)
            out-size   (* slice-len inner-size)
            out-bytes  (* 4 out-size)
            out-buf    (create-storage-buffer! device out-bytes)
            enc        (.createCommandEncoder device)]
        (.copyBufferToBuffer enc (:buffer t) src-offset out-buf 0 out-bytes)
        (.submit (.-queue device) #js [(.finish enc)])
        (let [out-shape (assoc sh 0 slice-len)]
          (->Tensor out-buf out-shape (compute-strides out-shape) out-size :f32 device)))
      ;; General case: shader for inner-dim slices
      (let [inner-size  (apply * (subvec sh (inc dim)))
            outer-size  (apply * (subvec sh 0 dim))
            out-size    (* outer-size slice-len inner-size)
            out-buf     (create-storage-buffer! device (* 4 out-size))
            params-buf  (create-uniform-buffer! device [start inner-size outer-size slice-len
                                                        dim-sz 0 0 0])
            pl          (get-pipeline! device shaders/slice-shader)
            bg          (.createBindGroup device
                          #js {:layout  (.getBindGroupLayout pl 0)
                               :entries #js [#js {:binding 0 :resource #js {:buffer (:buffer t)}}
                                             #js {:binding 1 :resource #js {:buffer out-buf}}
                                             #js {:binding 2 :resource #js {:buffer params-buf}}]})
            enc         (.createCommandEncoder device)
            pass        (.beginComputePass enc)]
        (.setPipeline pass pl)
        (.setBindGroup pass 0 bg)
        (.dispatchWorkgroups pass (workgroup-count out-size))
        (.end pass)
        (.submit (.-queue device) #js [(.finish enc)])
        (.destroy params-buf)
        (let [out-shape (assoc sh dim slice-len)]
          (->Tensor out-buf out-shape (compute-strides out-shape) out-size :f32 device))))))

;; ---------------------------------------------------------------------------
;; Concat
;; ---------------------------------------------------------------------------

(defn concat-tensors
  "Join tensors along existing dimension `dim`.
   Fast path: dim-0 concat uses sequential buffer copies."
  [dim tensors]
  (let [tensors (vec tensors)
        _       (when (< (count tensors) 2)
                  (throw (ex-info "concat-tensors: need at least 2 tensors" {})))
        device  (:device (first tensors))
        sh0     (:shape (first tensors))
        ndims   (count sh0)
        _       (when (or (< dim 0) (>= dim ndims))
                  (throw (ex-info "concat-tensors: dim out of range" {:dim dim :ndims ndims})))
        ;; Validate all shapes match except along concat dim
        _       (doseq [t (rest tensors)]
                  (let [sh (:shape t)]
                    (when (not= (count sh) ndims)
                      (throw (ex-info "concat-tensors: rank mismatch"
                                      {:expected ndims :got (count sh)})))
                    (doseq [d (range ndims)]
                      (when (and (not= d dim) (not= (nth sh d) (nth sh0 d)))
                        (throw (ex-info "concat-tensors: shape mismatch"
                                        {:dim d :expected (nth sh0 d) :got (nth sh d)}))))))
        ;; Compute output shape
        total-dim (reduce + (map #(nth (:shape %) dim) tensors))
        out-shape (assoc sh0 dim total-dim)
        out-size  (apply * out-shape)
        out-buf   (create-storage-buffer! device (* 4 out-size))]
    (if (zero? dim)
      ;; Fast path: dim-0 concat is sequential memory blocks
      (let [enc (.createCommandEncoder device)]
        (loop [ts tensors, offset 0]
          (when (seq ts)
            (let [t     (first ts)
                  bytes (* 4 (:size t))]
              (.copyBufferToBuffer enc (:buffer t) 0 out-buf offset bytes)
              (recur (rest ts) (+ offset bytes)))))
        (.submit (.-queue device) #js [(.finish enc)])
        (->Tensor out-buf out-shape (compute-strides out-shape) out-size :f32 device))
      ;; General case: one shader dispatch per input
      (let [inner-size (apply * (subvec sh0 (inc dim)))
            outer-size (apply * (subvec sh0 0 dim))
            dst-dim-size total-dim
            enc (.createCommandEncoder device)
            temp-bufs (volatile! [])]
        (loop [ts tensors, dim-offset 0]
          (when (seq ts)
            (let [t          (first ts)
                  src-dim-sz (nth (:shape t) dim)
                  src-total  (:size t)
                  params-buf (create-uniform-buffer! device [dim-offset src-total inner-size src-dim-sz
                                                            dst-dim-size outer-size 0 0])
                  pl         (get-pipeline! device shaders/concat-shader)
                  bg         (.createBindGroup device
                               #js {:layout  (.getBindGroupLayout pl 0)
                                    :entries #js [#js {:binding 0 :resource #js {:buffer (:buffer t)}}
                                                  #js {:binding 1 :resource #js {:buffer out-buf}}
                                                  #js {:binding 2 :resource #js {:buffer params-buf}}]})
                  pass       (.beginComputePass enc)]
              (vswap! temp-bufs conj params-buf)
              (.setPipeline pass pl)
              (.setBindGroup pass 0 bg)
              (.dispatchWorkgroups pass (workgroup-count src-total))
              (.end pass)
              (recur (rest ts) (+ dim-offset src-dim-sz)))))
        (.submit (.-queue device) #js [(.finish enc)])
        (doseq [buf @temp-bufs] (.destroy buf))
        (->Tensor out-buf out-shape (compute-strides out-shape) out-size :f32 device)))))

;; ---------------------------------------------------------------------------
;; Stack
;; ---------------------------------------------------------------------------

(defn stack
  "Stack tensors along a new dimension `dim`.
   Inserts a size-1 dim at `dim` in each tensor, then concatenates."
  [dim tensors]
  (let [tensors (vec tensors)
        ;; Insert size-1 dim at position `dim` in each tensor's shape
        reshaped (mapv (fn [t]
                         (let [sh (:shape t)
                               new-sh (vec (concat (subvec sh 0 dim) [1] (subvec sh dim)))]
                           (reshape t new-sh)))
                       tensors)]
    (concat-tensors dim reshaped)))

;; ---------------------------------------------------------------------------
;; Arange
;; ---------------------------------------------------------------------------

(defn arange
  "Create 1D tensor [0 1 2 ... n-1]."
  [n]
  (let [{:keys [device]} (dev/ctx)
        arr (js/Float32Array. n)
        _   (dotimes [i n] (aset arr i i))
        buf (create-storage-buffer! device (.-byteLength arr))]
    (.writeBuffer (.-queue device) buf 0 arr)
    (->Tensor buf [n] [1] n :f32 device)))

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
