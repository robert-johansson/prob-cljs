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

;; ---------------------------------------------------------------------------
;; Autograd tape infrastructure
;; ---------------------------------------------------------------------------

(def ^:dynamic *tape*
  "When non-nil (a volatile! holding a vector), ops record backward closures."
  nil)

(defrecord TrackedTensor [tensor id])

(defn tracked? [x] (instance? TrackedTensor x))

(defonce ^:private *id-counter* (volatile! 0))

(defn next-id!
  "Return a fresh integer ID for TrackedTensor nodes."
  []
  (let [id @*id-counter*] (vswap! *id-counter* inc) id))

(defn- unwrap
  "If x is a TrackedTensor, return its inner Tensor; else return x as-is."
  [x]
  (if (tracked? x) (:tensor x) x))

(defn tensor? [x]
  (or (instance? Tensor x) (tracked? x)))

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
;; Autograd gradient helpers
;; ---------------------------------------------------------------------------

(defn- dispatch-broadcast-scalar!
  "Broadcast a scalar tensor to fill a buffer of target-size."
  [scalar-t target-size target-shape]
  (let [device (:device scalar-t)
        bytes  (* 4 target-size)
        out    (create-storage-buffer! device bytes)
        pl     (get-pipeline! device shaders/broadcast-scalar-shader)
        bg     (.createBindGroup device
                 #js {:layout  (.getBindGroupLayout pl 0)
                      :entries #js [#js {:binding 0 :resource #js {:buffer (:buffer scalar-t)}}
                                    #js {:binding 1 :resource #js {:buffer out}}]})
        enc    (.createCommandEncoder device)
        pass   (.beginComputePass enc)]
    (.setPipeline pass pl)
    (.setBindGroup pass 0 bg)
    (.dispatchWorkgroups pass (workgroup-count target-size))
    (.end pass)
    (.submit (.-queue device) #js [(.finish enc)])
    (->Tensor out target-shape (compute-strides target-shape) target-size :f32 device)))

(defn- dispatch-reduction-to-scalar!
  "Sum all elements to a scalar. Wraps dispatch-reduction! for size>1, copies for size=1."
  [t]
  (if (= 1 (:size t))
    (let [device (:device t)
          buf    (create-storage-buffer! device 4)]
      (copy-buffer-to-buffer! device (:buffer t) buf 4)
      (->Tensor buf [] [] 1 :f32 device))
    (dispatch-reduction! t)))

(defn- reduce-grad
  "Sum gradient down if it was broadcast from a smaller input."
  [grad original-size]
  (if (= (:size grad) original-size)
    grad
    (if (= original-size 1)
      (dispatch-reduction-to-scalar! grad)
      (throw (ex-info "Partial broadcast grad not yet supported"
                      {:grad-size (:size grad) :target original-size})))))

(defn- record-tape! [out-id backward-fn]
  (vswap! *tape* conj {:output-id out-id :backward backward-fn}))

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
;; Arithmetic (with autograd tracking)
;; ---------------------------------------------------------------------------

(defn add [a b]
  (if *tape*
    (let [at (tracked? a) bt (tracked? b)
          a-raw (unwrap a) b-raw (unwrap b)
          out (dispatch-binary! shaders/add-shader a-raw b-raw)
          out-id (next-id!)
          a-id (when at (:id a)) b-id (when bt (:id b))
          a-sz (:size a-raw) b-sz (:size b-raw)]
      (record-tape! out-id
        (fn [g grads]
          (cond-> grads
            a-id (update a-id
                   (fn [ex]
                     (let [rd (reduce-grad g a-sz)]
                       (if ex (dispatch-binary! shaders/add-shader ex rd) rd))))
            b-id (update b-id
                   (fn [ex]
                     (let [rd (reduce-grad g b-sz)]
                       (if ex (dispatch-binary! shaders/add-shader ex rd) rd)))))))
      (->TrackedTensor out out-id))
    (dispatch-binary! shaders/add-shader a b)))

(defn subtract [a b]
  (if *tape*
    (let [at (tracked? a) bt (tracked? b)
          a-raw (unwrap a) b-raw (unwrap b)
          out (dispatch-binary! shaders/subtract-shader a-raw b-raw)
          out-id (next-id!)
          a-id (when at (:id a)) b-id (when bt (:id b))
          a-sz (:size a-raw) b-sz (:size b-raw)]
      (record-tape! out-id
        (fn [g grads]
          (cond-> grads
            a-id (update a-id
                   (fn [ex]
                     (let [rd (reduce-grad g a-sz)]
                       (if ex (dispatch-binary! shaders/add-shader ex rd) rd))))
            b-id (update b-id
                   (fn [ex]
                     (let [ng (dispatch-unary! shaders/negative-shader g)
                           rd (reduce-grad ng b-sz)]
                       (if ex (dispatch-binary! shaders/add-shader ex rd) rd)))))))
      (->TrackedTensor out out-id))
    (dispatch-binary! shaders/subtract-shader a b)))

(defn multiply [a b]
  (if *tape*
    (let [at (tracked? a) bt (tracked? b)
          a-raw (unwrap a) b-raw (unwrap b)
          out (dispatch-binary! shaders/multiply-shader a-raw b-raw)
          out-id (next-id!)
          a-id (when at (:id a)) b-id (when bt (:id b))
          a-sz (:size a-raw) b-sz (:size b-raw)]
      (record-tape! out-id
        (fn [g grads]
          (cond-> grads
            a-id (update a-id
                   (fn [ex]
                     (let [raw (dispatch-binary! shaders/multiply-shader g b-raw)
                           rd  (reduce-grad raw a-sz)]
                       (if ex (dispatch-binary! shaders/add-shader ex rd) rd))))
            b-id (update b-id
                   (fn [ex]
                     (let [raw (dispatch-binary! shaders/multiply-shader g a-raw)
                           rd  (reduce-grad raw b-sz)]
                       (if ex (dispatch-binary! shaders/add-shader ex rd) rd)))))))
      (->TrackedTensor out out-id))
    (dispatch-binary! shaders/multiply-shader a b)))

(defn divide [a b]
  (if *tape*
    (let [at (tracked? a) bt (tracked? b)
          a-raw (unwrap a) b-raw (unwrap b)
          out (dispatch-binary! shaders/divide-shader a-raw b-raw)
          out-id (next-id!)
          a-id (when at (:id a)) b-id (when bt (:id b))
          a-sz (:size a-raw) b-sz (:size b-raw)]
      (record-tape! out-id
        (fn [g grads]
          (cond-> grads
            ;; d(a/b)/da = 1/b => grad_a = g / b
            a-id (update a-id
                   (fn [ex]
                     (let [raw (dispatch-binary! shaders/divide-shader g b-raw)
                           rd  (reduce-grad raw a-sz)]
                       (if ex (dispatch-binary! shaders/add-shader ex rd) rd))))
            ;; d(a/b)/db = -a/b^2 => grad_b = -g * a / (b * b)
            b-id (update b-id
                   (fn [ex]
                     (let [b2  (dispatch-binary! shaders/multiply-shader b-raw b-raw)
                           ab2 (dispatch-binary! shaders/divide-shader a-raw b2)
                           gab (dispatch-binary! shaders/multiply-shader g ab2)
                           neg (dispatch-unary! shaders/negative-shader gab)
                           rd  (reduce-grad neg b-sz)]
                       (if ex (dispatch-binary! shaders/add-shader ex rd) rd)))))))
      (->TrackedTensor out out-id))
    (dispatch-binary! shaders/divide-shader a b)))

;; Unary ops with autograd tracking

(defn negative [t]
  (if *tape*
    (let [tr (tracked? t) t-raw (unwrap t)
          out (dispatch-unary! shaders/negative-shader t-raw)
          out-id (next-id!)
          t-id (when tr (:id t))]
      (when t-id
        (record-tape! out-id
          (fn [g grads]
            (update grads t-id
              (fn [ex]
                (let [ng (dispatch-unary! shaders/negative-shader g)]
                  (if ex (dispatch-binary! shaders/add-shader ex ng) ng)))))))
      (->TrackedTensor out out-id))
    (dispatch-unary! shaders/negative-shader t)))

(defn exp [t]
  (if *tape*
    (let [tr (tracked? t) t-raw (unwrap t)
          out (dispatch-unary! shaders/exp-shader t-raw)
          out-id (next-id!)
          t-id (when tr (:id t))]
      (when t-id
        (record-tape! out-id
          (fn [g grads]
            ;; d(exp(x))/dx = exp(x) = out
            (update grads t-id
              (fn [ex]
                (let [raw (dispatch-binary! shaders/multiply-shader g out)]
                  (if ex (dispatch-binary! shaders/add-shader ex raw) raw)))))))
      (->TrackedTensor out out-id))
    (dispatch-unary! shaders/exp-shader t)))

(defn log [t]
  (if *tape*
    (let [tr (tracked? t) t-raw (unwrap t)
          out (dispatch-unary! shaders/log-shader t-raw)
          out-id (next-id!)
          t-id (when tr (:id t))]
      (when t-id
        (record-tape! out-id
          (fn [g grads]
            ;; d(log(x))/dx = 1/x => grad = g / input
            (update grads t-id
              (fn [ex]
                (let [raw (dispatch-binary! shaders/divide-shader g t-raw)]
                  (if ex (dispatch-binary! shaders/add-shader ex raw) raw)))))))
      (->TrackedTensor out out-id))
    (dispatch-unary! shaders/log-shader t)))

(defn sqrt [t]
  (if *tape*
    (let [tr (tracked? t) t-raw (unwrap t)
          out (dispatch-unary! shaders/sqrt-shader t-raw)
          out-id (next-id!)
          t-id (when tr (:id t))]
      (when t-id
        (record-tape! out-id
          (fn [g grads]
            ;; d(sqrt(x))/dx = 1/(2*sqrt(x)) = 1/(2*out)
            (update grads t-id
              (fn [ex]
                (let [two  (->Tensor (:buffer (scalar 2.0)) [] [] 1 :f32 (:device out))
                      denom (dispatch-binary! shaders/multiply-shader two out)
                      raw   (dispatch-binary! shaders/divide-shader g denom)]
                  (if ex (dispatch-binary! shaders/add-shader ex raw) raw)))))))
      (->TrackedTensor out out-id))
    (dispatch-unary! shaders/sqrt-shader t)))

(defn square [t]
  (if *tape*
    (let [tr (tracked? t) t-raw (unwrap t)
          out (dispatch-unary! shaders/square-shader t-raw)
          out-id (next-id!)
          t-id (when tr (:id t))]
      (when t-id
        (record-tape! out-id
          (fn [g grads]
            ;; d(x^2)/dx = 2x => grad = g * 2 * input
            (update grads t-id
              (fn [ex]
                (let [two (scalar 2.0)
                      dx  (dispatch-binary! shaders/multiply-shader two t-raw)
                      raw (dispatch-binary! shaders/multiply-shader g dx)]
                  (if ex (dispatch-binary! shaders/add-shader ex raw) raw)))))))
      (->TrackedTensor out out-id))
    (dispatch-unary! shaders/square-shader t)))

(defn abs [t]
  (if *tape*
    (let [tr (tracked? t) t-raw (unwrap t)
          out (dispatch-unary! shaders/abs-shader t-raw)
          out-id (next-id!)
          t-id (when tr (:id t))]
      (when t-id
        (record-tape! out-id
          (fn [g grads]
            ;; d|x|/dx = sign(x) = where(x > 0, 1, -1)
            ;; grad = where(x > 0, g, -g)
            (update grads t-id
              (fn [ex]
                (let [z   (->Tensor (:buffer (zeros [1])) [1] [1] 1 :f32 (:device t-raw))
                      cmp (dispatch-binary! shaders/greater-shader t-raw z)
                      ng  (dispatch-unary! shaders/negative-shader g)
                      raw (dispatch-ternary! shaders/where-shader cmp g ng)]
                  (if ex (dispatch-binary! shaders/add-shader ex raw) raw)))))))
      (->TrackedTensor out out-id))
    (dispatch-unary! shaders/abs-shader t)))

;; ---------------------------------------------------------------------------
;; Comparisons (non-differentiable — unwrap only)
;; ---------------------------------------------------------------------------

(defn greater [a b] (dispatch-binary! shaders/greater-shader (unwrap a) (unwrap b)))
(defn less [a b] (dispatch-binary! shaders/less-shader (unwrap a) (unwrap b)))
(defn greater-equal [a b] (dispatch-binary! shaders/greater-equal-shader (unwrap a) (unwrap b)))
(defn less-equal [a b] (dispatch-binary! shaders/less-equal-shader (unwrap a) (unwrap b)))

(defn where
  "Element-wise select: where cond > 0, take a, else b."
  [cond-t a b]
  (if *tape*
    (let [at (tracked? a) bt (tracked? b)
          c-raw (unwrap cond-t) a-raw (unwrap a) b-raw (unwrap b)
          out (dispatch-ternary! shaders/where-shader c-raw a-raw b-raw)
          out-id (next-id!)
          a-id (when at (:id a)) b-id (when bt (:id b))
          a-sz (:size a-raw) b-sz (:size b-raw)]
      (record-tape! out-id
        (fn [g grads]
          (let [z (zeros (if (empty? (:shape g)) [1] (:shape g)))]
            (cond-> grads
              a-id (update a-id
                     (fn [ex]
                       (let [raw (dispatch-ternary! shaders/where-shader c-raw g z)
                             rd  (reduce-grad raw a-sz)]
                         (if ex (dispatch-binary! shaders/add-shader ex rd) rd))))
              b-id (update b-id
                     (fn [ex]
                       (let [raw (dispatch-ternary! shaders/where-shader c-raw z g)
                             rd  (reduce-grad raw b-sz)]
                         (if ex (dispatch-binary! shaders/add-shader ex rd) rd))))))))
      (->TrackedTensor out out-id))
    (dispatch-ternary! shaders/where-shader cond-t a b)))

;; ---------------------------------------------------------------------------
;; Reductions (with autograd tracking)
;; ---------------------------------------------------------------------------

(defn sum
  "Sum all elements. Returns scalar tensor."
  [t]
  (if *tape*
    (let [tr (tracked? t) t-raw (unwrap t)
          out (if (= 1 (:size t-raw))
                (let [device (:device t-raw)
                      buf    (create-storage-buffer! device 4)]
                  (copy-buffer-to-buffer! device (:buffer t-raw) buf 4)
                  (->Tensor buf [] [] 1 :f32 device))
                (dispatch-reduction! t-raw))
          out-id (next-id!)
          t-id (when tr (:id t))
          t-sz (:size t-raw)
          t-sh (:shape t-raw)]
      (when t-id
        (record-tape! out-id
          (fn [g grads]
            ;; d(sum(x))/dx_i = 1 for all i => grad = broadcast(g, input_shape)
            (update grads t-id
              (fn [ex]
                (let [raw (if (= t-sz 1)
                            ;; Scalar input: grad passes through as-is
                            (let [device (:device g)
                                  buf (create-storage-buffer! device 4)]
                              (copy-buffer-to-buffer! device (:buffer g) buf 4)
                              (->Tensor buf [] [] 1 :f32 device))
                            (dispatch-broadcast-scalar! g t-sz t-sh))]
                  (if ex (dispatch-binary! shaders/add-shader ex raw) raw)))))))
      (->TrackedTensor out out-id))
    (if (= 1 (:size t))
      (let [device (:device t)
            buf    (create-storage-buffer! device 4)]
        (copy-buffer-to-buffer! device (:buffer t) buf 4)
        (->Tensor buf [] [] 1 :f32 device))
      (dispatch-reduction! t))))

(defn mean
  "Mean of all elements. Returns scalar tensor."
  [t]
  ;; mean = sum / n — both tracked, chain rule handles it
  (let [raw (unwrap t)]
    (divide (sum t) (scalar (:size raw)))))

;; ---------------------------------------------------------------------------
;; Shape (with TrackedTensor support)
;; ---------------------------------------------------------------------------

(defn shape [t] (:shape (unwrap t)))
(defn ndim [t] (count (:shape (unwrap t))))
(defn size [t] (:size (unwrap t)))

(defn reshape
  "Zero-cost reshape — same buffer, new shape metadata.
   Product of new shape must equal tensor size."
  [t new-shape]
  (if *tape*
    (let [tr (tracked? t) t-raw (unwrap t)
          new-size (apply * new-shape)
          _  (when (not= new-size (:size t-raw))
               (throw (ex-info "Reshape size mismatch"
                               {:current-size (:size t-raw) :new-size new-size :new-shape new-shape})))
          out (->Tensor (:buffer t-raw) new-shape (compute-strides new-shape) (:size t-raw) :f32 (:device t-raw))
          out-id (next-id!)
          t-id (when tr (:id t))
          orig-shape (:shape t-raw)]
      (when t-id
        (record-tape! out-id
          (fn [g grads]
            ;; Reshape gradient back to original shape
            (update grads t-id
              (fn [ex]
                (let [raw (->Tensor (:buffer g) orig-shape (compute-strides orig-shape) (:size g) :f32 (:device g))]
                  (if ex (dispatch-binary! shaders/add-shader ex raw) raw)))))))
      (->TrackedTensor out out-id))
    (let [new-size (apply * new-shape)]
      (when (not= new-size (:size t))
        (throw (ex-info "Reshape size mismatch"
                        {:current-size (:size t) :new-size new-size :new-shape new-shape})))
      (->Tensor (:buffer t) new-shape (compute-strides new-shape) (:size t) :f32 (:device t)))))

;; ---------------------------------------------------------------------------
;; Matmul
;; ---------------------------------------------------------------------------

(defn- dispatch-matmul!
  "Raw matmul dispatch: A[M,K] × B[K,N] → C[M,N]. Always 2D."
  [a-raw b-raw M K N]
  (let [device   (:device a-raw)
        out-size (* M N)
        out-buf  (create-storage-buffer! device (* 4 out-size))
        dims-buf (create-uniform-buffer! device [M K N 0])
        pl       (get-pipeline! device shaders/matmul-shader)
        bg       (.createBindGroup device
                   #js {:layout  (.getBindGroupLayout pl 0)
                        :entries #js [#js {:binding 0 :resource #js {:buffer (:buffer a-raw)}}
                                      #js {:binding 1 :resource #js {:buffer (:buffer b-raw)}}
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
    (->Tensor out-buf [M N] (compute-strides [M N]) out-size :f32 device)))

(defn- dispatch-transpose!
  "Raw 2D transpose: [rows,cols] → [cols,rows]."
  [t-raw]
  (let [sh     (:shape t-raw)
        device (:device t-raw)
        rows   (first sh)
        cols   (second sh)
        n      (:size t-raw)
        out-buf (create-storage-buffer! device (* 4 n))
        dims-buf (create-uniform-buffer! device [rows cols 0 0])
        pl      (get-pipeline! device shaders/transpose-shader)
        bg      (.createBindGroup device
                  #js {:layout  (.getBindGroupLayout pl 0)
                       :entries #js [#js {:binding 0 :resource #js {:buffer (:buffer t-raw)}}
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
      (->Tensor out-buf out-shape (compute-strides out-shape) n :f32 device))))

(defn- matmul-parse-shapes
  "Parse shapes for matmul, returning [M K N squeeze-left squeeze-right]."
  [a-shape b-shape]
  (let [a-ndim (count a-shape)
        b-ndim (count b-shape)
        squeeze-left  (= a-ndim 1)
        squeeze-right (= b-ndim 1)
        [M K-a] (cond
                  (= a-ndim 0) (throw (ex-info "matmul: scalar input not supported" {:shape a-shape}))
                  squeeze-left  [1 (first a-shape)]
                  :else         [(first a-shape) (second a-shape)])
        [K-b N]  (cond
                   (= b-ndim 0) (throw (ex-info "matmul: scalar input not supported" {:shape b-shape}))
                   squeeze-right [(first b-shape) 1]
                   :else         [(first b-shape) (second b-shape)])]
    (when (not= K-a K-b)
      (throw (ex-info "matmul: inner dimensions mismatch"
                      {:a-shape a-shape :b-shape b-shape :K-a K-a :K-b K-b})))
    [M K-a N squeeze-left squeeze-right]))

(defn- matmul-squeeze-output
  "Apply squeeze to matmul output based on input dims."
  [out-2d squeeze-left squeeze-right M N]
  (let [out-shape (cond
                    (and squeeze-left squeeze-right) []
                    squeeze-left                     [N]
                    squeeze-right                    [M]
                    :else                            [M N])]
    (if (= out-shape [M N])
      out-2d
      (->Tensor (:buffer out-2d) out-shape (compute-strides out-shape) (:size out-2d) :f32 (:device out-2d)))))

(defn matmul
  "Matrix multiply A[M,K] × B[K,N] → C[M,N].
   1D inputs: [K] treated as [1,K] (left) or [K,1] (right), result squeezed."
  [a b]
  (if *tape*
    (let [at (tracked? a) bt (tracked? b)
          a-raw (unwrap a) b-raw (unwrap b)
          a-shape (:shape a-raw) b-shape (:shape b-raw)
          [M K N squeeze-left squeeze-right] (matmul-parse-shapes a-shape b-shape)
          out-2d (dispatch-matmul! a-raw b-raw M K N)
          out (matmul-squeeze-output out-2d squeeze-left squeeze-right M N)
          out-id (next-id!)
          a-id (when at (:id a)) b-id (when bt (:id b))]
      (record-tape! out-id
        (fn [g grads]
          ;; g might be squeezed — unsqueeze to [M,N] for matmul backward
          (let [g-2d (cond
                       (and squeeze-left squeeze-right)
                       ;; scalar grad → [1,1]
                       (->Tensor (:buffer g) [1 1] [1 1] 1 :f32 (:device g))
                       squeeze-left
                       ;; [N] → [1,N]
                       (->Tensor (:buffer g) [1 N] [N 1] (:size g) :f32 (:device g))
                       squeeze-right
                       ;; [M] → [M,1]
                       (->Tensor (:buffer g) [M 1] [1 1] (:size g) :f32 (:device g))
                       :else g)]
            (cond-> grads
              ;; grad_A = g @ B^T  → [M,N] × [N,K] → [M,K]
              a-id (update a-id
                     (fn [ex]
                       (let [bt-raw (dispatch-transpose! b-raw)
                             raw-2d (dispatch-matmul! g-2d bt-raw M N K)
                             raw (if squeeze-left
                                   ;; squeeze [1,K] → [K]
                                   (->Tensor (:buffer raw-2d) [K] [1] K :f32 (:device raw-2d))
                                   raw-2d)]
                         (if ex (dispatch-binary! shaders/add-shader ex raw) raw))))
              ;; grad_B = A^T @ g  → [K,M] × [M,N] → [K,N]
              b-id (update b-id
                     (fn [ex]
                       (let [at-raw (dispatch-transpose! a-raw)
                             raw-2d (dispatch-matmul! at-raw g-2d K M N)
                             raw (if squeeze-right
                                   ;; squeeze [K,1] → [K]
                                   (->Tensor (:buffer raw-2d) [K] [1] K :f32 (:device raw-2d))
                                   raw-2d)]
                         (if ex (dispatch-binary! shaders/add-shader ex raw) raw))))))))
      (->TrackedTensor out out-id))
    ;; Non-tracked path
    (let [a-shape (:shape a) b-shape (:shape b)
          [M K N squeeze-left squeeze-right] (matmul-parse-shapes a-shape b-shape)
          out-2d (dispatch-matmul! a b M K N)]
      (matmul-squeeze-output out-2d squeeze-left squeeze-right M N))))

;; ---------------------------------------------------------------------------
;; Transpose
;; ---------------------------------------------------------------------------

(defn- copy-tensor-raw!
  "Copy a raw tensor to a new buffer."
  [t-raw]
  (let [device (:device t-raw)
        bytes  (* 4 (:size t-raw))
        buf    (create-storage-buffer! device bytes)]
    (copy-buffer-to-buffer! device (:buffer t-raw) buf bytes)
    (->Tensor buf (:shape t-raw) (:strides t-raw) (:size t-raw) :f32 device)))

(defn transpose
  "Physical transpose of 2D tensor [rows,cols] → [cols,rows].
   Scalar/1D → returned as-is (no-op)."
  [t]
  (if *tape*
    (let [tr (tracked? t) t-raw (unwrap t)
          sh (:shape t-raw)
          out (if (<= (count sh) 1)
                (copy-tensor-raw! t-raw)
                (dispatch-transpose! t-raw))
          out-id (next-id!)
          t-id (when tr (:id t))]
      (when t-id
        (record-tape! out-id
          (fn [g grads]
            ;; d(transpose(X))/dX = transpose(g)
            (update grads t-id
              (fn [ex]
                (let [raw (if (<= (count (:shape g)) 1)
                            (copy-tensor-raw! g)
                            (dispatch-transpose! g))]
                  (if ex (dispatch-binary! shaders/add-shader ex raw) raw)))))))
      (->TrackedTensor out out-id))
    ;; Non-tracked path
    (let [sh (:shape t)]
      (if (<= (count sh) 1)
        (copy-tensor-raw! t)
        (dispatch-transpose! t)))))

;; ---------------------------------------------------------------------------
;; Slice
;; ---------------------------------------------------------------------------

(defn slice
  "Extract sub-tensor along dimension `dim`, indices [start, end).
   Fast path: dim-0 slices use copyBufferToBuffer (contiguous)."
  [t dim start end]
  (let [t      (unwrap t)
        sh     (:shape t)
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
  (let [tensors (vec (map unwrap tensors))
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
  (let [tensors (vec (map unwrap tensors))
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
  (.destroy (:buffer (unwrap t)))
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
  (let [t       (unwrap t)
        device  (:device t)
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
  (let [t        (unwrap t)
        device   (:device t)
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
