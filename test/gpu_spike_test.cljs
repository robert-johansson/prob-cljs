(ns gpu-spike-test
  "Spike test: validate WebGPU compute from ClojureScript.
   Tests init, unary shader, binary shader, readback latency, and larger arrays."
  (:require [promesa.core :as p]
            [prob.gpu.device :as dev]))

(def ^:private passed (volatile! 0))
(def ^:private failed (volatile! 0))

(defn- pass [name]
  (vswap! passed inc)
  (println (str "  PASS: " name)))

(defn- fail [name msg]
  (vswap! failed inc)
  (println (str "  FAIL: " name " — " msg)))

(defn- approx= [a b tol]
  (< (js/Math.abs (- a b)) tol))

(defn- arr-approx= [xs ys tol]
  (and (= (count xs) (count ys))
       (every? true? (map #(approx= %1 %2 tol) xs ys))))

;; ---------------------------------------------------------------------------
;; WGSL shaders
;; ---------------------------------------------------------------------------

(def ^:private double-shader
  "@group(0) @binding(0) var<storage, read> input: array<f32>;
   @group(0) @binding(1) var<storage, read_write> output: array<f32>;
   @compute @workgroup_size(64)
   fn main(@builtin(global_invocation_id) gid: vec3<u32>) {
     let idx = gid.x;
     if (idx >= arrayLength(&output)) { return; }
     output[idx] = input[idx] * 2.0;
   }")

(def ^:private add-shader
  "@group(0) @binding(0) var<storage, read> a: array<f32>;
   @group(0) @binding(1) var<storage, read> b: array<f32>;
   @group(0) @binding(2) var<storage, read_write> result: array<f32>;
   @compute @workgroup_size(64)
   fn main(@builtin(global_invocation_id) gid: vec3<u32>) {
     let idx = gid.x;
     if (idx >= arrayLength(&result)) { return; }
     result[idx] = a[idx] + b[idx];
   }")

;; ---------------------------------------------------------------------------
;; Helpers: buffer creation, pipeline dispatch, readback
;; ---------------------------------------------------------------------------

(defn- create-buffer-with-data
  "Create a STORAGE | COPY_SRC | COPY_DST buffer and write f32 data into it."
  [device data]
  (let [arr  (js/Float32Array. (clj->js data))
        buf  (.createBuffer device
               #js {:size            (.-byteLength arr)
                    :usage           (bit-or js/GPUBufferUsage.STORAGE
                                             js/GPUBufferUsage.COPY_SRC
                                             js/GPUBufferUsage.COPY_DST)
                    :mappedAtCreation false})]
    (.writeBuffer (.-queue device) buf 0 arr)
    buf))

(defn- create-output-buffer
  "Create a STORAGE | COPY_SRC buffer for shader output."
  [device byte-size]
  (.createBuffer device
    #js {:size  byte-size
         :usage (bit-or js/GPUBufferUsage.STORAGE
                        js/GPUBufferUsage.COPY_SRC)}))

(defn- create-staging-buffer
  "Create a MAP_READ | COPY_DST buffer for readback."
  [device byte-size]
  (.createBuffer device
    #js {:size  byte-size
         :usage (bit-or js/GPUBufferUsage.MAP_READ
                        js/GPUBufferUsage.COPY_DST)}))

(defn- readback
  "Copy GPU buffer to staging, mapAsync, return js/Float32Array.
   Returns a Promise<Float32Array>."
  [device buf byte-size]
  (let [staging (create-staging-buffer device byte-size)
        encoder (.createCommandEncoder device)]
    (.copyBufferToBuffer encoder buf 0 staging 0 byte-size)
    (.submit (.-queue device) #js [(.finish encoder)])
    (p/let [_ (.mapAsync staging js/GPUMapMode.READ)]
      (let [result (js/Float32Array. (.slice (.getMappedRange staging) 0))]
        (.unmap staging)
        (.destroy staging)
        result))))

(defn- dispatch-unary!
  "Dispatch a unary compute shader (1 input, 1 output). Returns Promise<vec>."
  [device shader-source input-data]
  (let [n         (count input-data)
        byte-size (* 4 n)
        in-buf    (create-buffer-with-data device input-data)
        out-buf   (create-output-buffer device byte-size)
        module    (.createShaderModule device #js {:code shader-source})
        pipeline  (.createComputePipeline device
                    #js {:layout  "auto"
                         :compute #js {:module     module
                                       :entryPoint "main"}})
        bg        (.createBindGroup device
                    #js {:layout  (.getBindGroupLayout pipeline 0)
                         :entries #js [#js {:binding 0 :resource #js {:buffer in-buf}}
                                       #js {:binding 1 :resource #js {:buffer out-buf}}]})
        encoder   (.createCommandEncoder device)
        cpass     (.beginComputePass encoder)]
    (.setPipeline cpass pipeline)
    (.setBindGroup cpass 0 bg)
    (.dispatchWorkgroups cpass (js/Math.ceil (/ n 64)))
    (.end cpass)
    (.submit (.-queue device) #js [(.finish encoder)])
    (p/let [arr (readback device out-buf byte-size)]
      (.destroy in-buf)
      (.destroy out-buf)
      (vec arr))))

(defn- dispatch-binary!
  "Dispatch a binary compute shader (2 inputs, 1 output). Returns Promise<vec>."
  [device shader-source data-a data-b]
  (let [n         (count data-a)
        byte-size (* 4 n)
        buf-a     (create-buffer-with-data device data-a)
        buf-b     (create-buffer-with-data device data-b)
        out-buf   (create-output-buffer device byte-size)
        module    (.createShaderModule device #js {:code shader-source})
        pipeline  (.createComputePipeline device
                    #js {:layout  "auto"
                         :compute #js {:module     module
                                       :entryPoint "main"}})
        bg        (.createBindGroup device
                    #js {:layout  (.getBindGroupLayout pipeline 0)
                         :entries #js [#js {:binding 0 :resource #js {:buffer buf-a}}
                                       #js {:binding 1 :resource #js {:buffer buf-b}}
                                       #js {:binding 2 :resource #js {:buffer out-buf}}]})
        encoder   (.createCommandEncoder device)
        cpass     (.beginComputePass encoder)]
    (.setPipeline cpass pipeline)
    (.setBindGroup cpass 0 bg)
    (.dispatchWorkgroups cpass (js/Math.ceil (/ n 64)))
    (.end cpass)
    (.submit (.-queue device) #js [(.finish encoder)])
    (p/let [arr (readback device out-buf byte-size)]
      (.destroy buf-a)
      (.destroy buf-b)
      (.destroy out-buf)
      (vec arr))))

;; ---------------------------------------------------------------------------
;; Tests
;; ---------------------------------------------------------------------------

(defn- test-1-init
  "Test 1: WebGPU initialization."
  []
  (p/let [ctx (dev/init!)]
    (if (and (:adapter ctx) (:device ctx) (:queue ctx))
      (do (pass "WebGPU init")
          ctx)
      (do (fail "WebGPU init" "missing adapter, device, or queue")
          ctx))))

(defn- test-2-unary
  "Test 2: Unary shader — double [1 2 3 4]."
  [ctx]
  (let [device (:device ctx)]
    (p/let [result (dispatch-unary! device double-shader [1 2 3 4])]
      (if (arr-approx= result [2 4 6 8] 0.001)
        (pass (str "unary shader (double): " result))
        (fail "unary shader (double)" (str "expected [2 4 6 8], got " result))))))

(defn- test-3-binary
  "Test 3: Binary shader — [1 2 3] + [10 20 30]."
  [ctx]
  (let [device (:device ctx)]
    (p/let [result (dispatch-binary! device add-shader [1 2 3] [10 20 30])]
      (if (arr-approx= result [11 22 33] 0.001)
        (pass (str "binary shader (add): " result))
        (fail "binary shader (add)" (str "expected [11 22 33], got " result))))))

(defn- test-4-latency
  "Test 4: Readback latency benchmark — 100 single-f32 mapAsync calls."
  [ctx]
  (let [device (:device ctx)
        n      100]
    (p/let [buf (create-buffer-with-data device [42.0])]
      (p/loop [i 0, times (transient [])]
        (if (>= i n)
          (let [ts    (persistent! times)
                total (apply + ts)
                mn    (apply min ts)
                mx    (apply max ts)
                avg   (/ total (count ts))]
            (.destroy buf)
            (pass (str "readback latency (" n "x): "
                       "mean=" (.toFixed avg 2) "ms, "
                       "min=" (.toFixed mn 2) "ms, "
                       "max=" (.toFixed mx 2) "ms"))
            {:mean avg :min mn :max mx})
          (let [t0 (js/performance.now)]
            (p/let [arr (readback device buf 4)]
              (let [dt (- (js/performance.now) t0)]
                (p/recur (inc i) (conj! times dt))))))))))

(defn- test-5-large
  "Test 5: Larger array — 1024 elements doubled."
  [ctx]
  (let [device   (:device ctx)
        input    (vec (range 1024))
        expected (mapv #(* 2.0 %) input)]
    (p/let [result (dispatch-unary! device double-shader input)]
      (if (arr-approx= result expected 0.001)
        (pass "large array (1024 elements doubled)")
        (fail "large array" (str "mismatch at first diff"))))))

;; ---------------------------------------------------------------------------
;; Main
;; ---------------------------------------------------------------------------

(println "\n=== WebGPU Spike Test ===\n")

(p/let [ctx     (test-1-init)
        _       (test-2-unary ctx)
        _       (test-3-binary ctx)
        latency (test-4-latency ctx)
        _       (test-5-large ctx)]
  (println (str "\n" @passed " passed, " @failed " failed"))
  (when (pos? @failed)
    (js/process.exit 1)))
