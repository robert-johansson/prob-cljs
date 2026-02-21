# TODO: WebGPU GPU-Accelerated Inference for prob-cljs

Universal GPU acceleration for probabilistic programming — same ClojureScript
codebase runs in nbb (terminal) and Scittle (browser), on any GPU.

See `docs/webgpu-path.md` for full architecture rationale.

---

## Phase 0: Spike — Validate the Toolchain

**Goal:** Prove we can dispatch a WGSL compute shader and read back results
from both nbb and Scittle using the same ClojureScript code.

### 0.1 Environment detection module

Create `src/prob/gpu/device.cljs` — the one place that handles browser vs Node.js.

```clojure
(ns prob.gpu.device
  "WebGPU device initialization.
   Detects browser (navigator.gpu) vs Node.js (webgpu npm package)
   and provides a unified GPU device via Promesa."
  (:require [promesa.core :as p]))

(defn- request-gpu
  "Return a GPU object, auto-detecting environment."
  []
  (if (and (exists? js/navigator) (.-gpu js/navigator))
    js/navigator.gpu
    (let [webgpu (js/require "webgpu")]
      (.create webgpu #js []))))

(defn init!
  "Initialize WebGPU. Returns Promise<{:device :adapter :queue}>."
  []
  (p/let [gpu     (request-gpu)
          adapter (.requestAdapter gpu)
          device  (.requestDevice adapter)]
    {:gpu     gpu
     :adapter adapter
     :device  device
     :queue   (.-queue device)}))
```

- [x] Create `src/prob/gpu/device.cljs`
- [x] Test `init!` from nbb with `npm install webgpu`
- [ ] Test `init!` from a browser page with Scittle

### 0.2 Minimal compute shader round-trip

Write a WGSL shader that doubles an array of floats. Dispatch it, read back,
verify the result. This validates: shader compilation, buffer creation,
dispatch, staging buffer, `mapAsync` readback, Promesa integration.

```clojure
;; test/gpu_spike_test.cljs
(ns gpu-spike-test
  (:require [promesa.core :as p]
            [prob.gpu.device :as dev]))

(def double-shader
  "@group(0) @binding(0) var<storage, read> input: array<f32>;
   @group(0) @binding(1) var<storage, read_write> output: array<f32>;

   @compute @workgroup_size(64)
   fn main(@builtin(global_invocation_id) gid: vec3<u32>) {
     let idx = gid.x;
     if (idx >= arrayLength(&output)) { return; }
     output[idx] = input[idx] * 2.0;
   }")

(p/let [ctx (dev/init!)]
  ;; ... create buffers, pipeline, dispatch, readback ...
  ;; verify [2 4 6 8] from input [1 2 3 4]
  (println "PASS: spike test"))
```

- [x] Write spike test with inline WGSL
- [x] Run from nbb: `nbb -cp src:test test/gpu_spike_test.cljs`
- [ ] Run from browser: create `test/gpu_spike.html` with Scittle
- [x] Verify identical results in both environments

### 0.3 Measure readback latency

Time a single `mapAsync` for a 4-byte (one f32) staging buffer.
This determines whether GPU-side accept/reject is necessary or just nice-to-have.

- [x] Benchmark `mapAsync` latency (target: < 3ms)
- [ ] Document results in `docs/webgpu-path.md`

---

## Phase 1: Tensor Runtime

**Goal:** A `prob.gpu.tensor` namespace with GPU-backed tensors, synchronous
ops, and async readback. No autograd yet.

### 1.1 Tensor record

Create `src/prob/gpu/tensor.cljs`. A tensor is a lightweight record wrapping
a GPU buffer with CPU-side shape metadata.

```clojure
(ns prob.gpu.tensor
  "GPU-backed tensors via WebGPU.
   All arithmetic operations are synchronous (encode GPU commands).
   Only to-number and to-clj are async (readback via mapAsync)."
  (:require [promesa.core :as p]))

(defrecord Tensor [buffer shape strides size dtype device])

(defn tensor?
  "Returns true if x is a GPU tensor."
  [x]
  (instance? Tensor x))
```

Fields:
- `buffer` — `GPUBuffer` reference
- `shape` — ClojureScript vector `[3 4]`
- `strides` — ClojureScript vector `[4 1]` (row-major)
- `size` — total element count (product of shape)
- `dtype` — keyword, always `:f32` for now
- `device` — back-reference to the WebGPU device context

- [x] Define `Tensor` record
- [x] Implement `shape`, `ndim`, `size` as plain functions on the record
- [x] Implement `tensor?` predicate

### 1.2 Buffer pool

Avoid allocation churn by reusing GPU buffers. Pool is keyed by byte size,
rounded up to the next power of two.

```clojure
(defn- next-power-of-two [n]
  (loop [p 256] ;; minimum 256 bytes
    (if (>= p n) p (recur (* 2 p)))))

(defonce ^:private buffer-pool
  (volatile! {})) ;; {size -> [available-buffer ...]}

(defn- acquire-buffer! [device byte-size usage]
  (let [alloc-size (next-power-of-two byte-size)
        pool       @buffer-pool
        available  (get pool alloc-size [])]
    (if (seq available)
      (do (vswap! buffer-pool update alloc-size subvec 1)
          (first available))
      (.createBuffer device
        #js {:size  alloc-size
             :usage usage}))))

(defn- release-buffer! [buf byte-size]
  (let [alloc-size (next-power-of-two byte-size)]
    (vswap! buffer-pool update alloc-size (fnil conj []) buf)))
```

- [ ] ~~Implement buffer pool~~ (deferred — direct allocation for now)
- [ ] ~~`acquire-buffer!` with power-of-two size classes~~ (deferred)
- [ ] ~~`release-buffer!` returns buffer to pool~~ (deferred)
- [x] `dispose!` explicitly destroys a tensor's buffer

### 1.3 Array creation

```clojure
(defn tensor
  "Create a GPU tensor from a ClojureScript sequential."
  [data]
  (let [flat (flatten data)     ;; TODO: handle nested vecs for shape
        arr  (js/Float32Array. (clj->js flat))
        buf  (acquire-buffer! *device* (.-byteLength arr) storage-usage)]
    (.writeBuffer (.-queue *device*) buf 0 arr)
    (->Tensor buf [(count flat)] [(1)] (count flat) :f32 *device*)))

(defn scalar
  "Create a GPU scalar (0-dimensional tensor)."
  [x]
  (let [arr (js/Float32Array. #js [x])
        buf (acquire-buffer! *device* 4 storage-usage)]
    (.writeBuffer (.-queue *device*) buf 0 arr)
    (->Tensor buf [] [] 1 :f32 *device*)))

(defn zeros
  "Create a zero-filled GPU tensor with the given shape."
  [shape]
  (let [n   (apply * shape)
        buf (acquire-buffer! *device* (* 4 n) storage-usage)]
    ;; GPU buffers are zero-initialized by spec
    (->Tensor buf shape (compute-strides shape) n :f32 *device*)))
```

- [x] `tensor` — from ClojureScript vector/seq, infer shape from nesting
- [x] `scalar` — single f32 value, shape `[]`
- [x] `zeros` — zero-filled tensor with given shape
- [x] `ones` — one-filled tensor
- [x] `full` — filled with a constant
- [x] Handle nested vectors: `(tensor [[1 2] [3 4]])` → shape `[2 2]`
- [x] `compute-strides` helper for row-major strides

### 1.4 WGSL shader strings

Store shaders as `def ^:private` string constants in a dedicated namespace.
Parameterize the binary op shader with string interpolation.

```clojure
(ns prob.gpu.shaders
  "WGSL compute shader source strings.")

(def ^:private binary-op-template
  "@group(0) @binding(0) var<storage, read> a: array<f32>;
   @group(0) @binding(1) var<storage, read> b: array<f32>;
   @group(0) @binding(2) var<storage, read_write> result: array<f32>;

   @compute @workgroup_size(64)
   fn main(@builtin(global_invocation_id) gid: vec3<u32>) {
     let idx = gid.x;
     if (idx >= arrayLength(&result)) { return; }
     result[idx] = a[idx] OP b[idx];
   }")

(defn binary-shader [op-str]
  (clojure.string/replace binary-op-template "OP" op-str))

(def add-shader      (binary-shader "+"))
(def subtract-shader (binary-shader "-"))
(def multiply-shader (binary-shader "*"))
(def divide-shader   (binary-shader "/"))
```

- [x] Create `src/prob/gpu/shaders.cljs`
- [x] Binary op template with string substitution
- [x] Unary op template (exp, log, neg, sqrt, square, abs)
- [x] Comparison ops (greater, less, greater-equal, less-equal)
- [x] `where` (conditional select)
- [x] Reduction shader (tree reduction with workgroup barrier)
- [x] PCG RNG shader (uniform)
- [x] Box-Muller shader (normal from uniform)
- [x] Tiled matmul shader
- [x] Broadcast shader (for grad of sum)
- [x] Transpose shader (for grad of matmul)

### 1.5 Pipeline cache

Cache compiled `GPUComputePipeline` objects by shader source string.
Creating pipelines is expensive (~50ms); reuse is free.

```clojure
(defonce ^:private pipeline-cache
  (volatile! {})) ;; {shader-source -> GPUComputePipeline}

(defn- get-pipeline! [device shader-source]
  (or (get @pipeline-cache shader-source)
      (let [module   (.createShaderModule device #js {:code shader-source})
            pipeline (.createComputePipeline device
                       #js {:layout  "auto"
                            :compute #js {:module     module
                                          :entryPoint "main"}})]
        (vswap! pipeline-cache assoc shader-source pipeline)
        pipeline)))
```

- [x] Pipeline cache with `volatile!`
- [x] `get-pipeline!` — create-or-reuse by shader source
- [x] Bind group creation helper

### 1.6 Op dispatch

Generic dispatch function that handles buffer binding, workgroup calculation,
command encoding, and submission.

```clojure
(defn- dispatch-binary!
  "Dispatch a binary element-wise shader. Returns a new Tensor."
  [shader-source a b]
  (let [device   (:device a)
        pipeline (get-pipeline! device shader-source)
        out-buf  (acquire-buffer! device (* 4 (:size a)) result-usage)
        bg       (.createBindGroup device
                   #js {:layout  (.getBindGroupLayout pipeline 0)
                        :entries #js [#js {:binding 0 :resource #js {:buffer (:buffer a)}}
                                      #js {:binding 1 :resource #js {:buffer (:buffer b)}}
                                      #js {:binding 2 :resource #js {:buffer out-buf}}]})
        encoder  (.createCommandEncoder device)
        pass     (.beginComputePass encoder)]
    (.setPipeline pass pipeline)
    (.setBindGroup pass 0 bg)
    (.dispatchWorkgroups pass (js/Math.ceil (/ (:size a) 64)))
    (.end pass)
    (.submit (.-queue device) #js [(.finish encoder)])
    (->Tensor out-buf (:shape a) (:strides a) (:size a) :f32 device)))
```

- [x] `dispatch-binary!` — generic binary op dispatch
- [x] `dispatch-unary!` — generic unary op dispatch
- [x] `dispatch-reduction!` — multi-pass tree reduction
- [x] Handle broadcasting for mismatched shapes
- [x] Handle scalar-tensor binary ops (broadcast scalar)

### 1.7 Arithmetic and math ops

Thin wrappers that call `dispatch-binary!` / `dispatch-unary!`:

```clojure
(defn add      [a b] (dispatch-binary! shaders/add-shader a b))
(defn subtract [a b] (dispatch-binary! shaders/subtract-shader a b))
(defn multiply [a b] (dispatch-binary! shaders/multiply-shader a b))
(defn divide   [a b] (dispatch-binary! shaders/divide-shader a b))
(defn negative [a]   (dispatch-unary!  shaders/negative-shader a))
(defn exp      [a]   (dispatch-unary!  shaders/exp-shader a))
(defn log      [a]   (dispatch-unary!  shaders/log-shader a))
(defn sqrt     [a]   (dispatch-unary!  shaders/sqrt-shader a))
(defn square   [a]   (dispatch-unary!  shaders/square-shader a))
```

- [x] Element-wise arithmetic: `add`, `subtract`, `multiply`, `divide`
- [x] Element-wise unary: `negative`, `exp`, `log`, `sqrt`, `square`, `abs`
- [x] Comparison: `greater`, `less`, `greater-equal`, `less-equal`
- [x] Conditional: `where`
- [x] Scalar broadcasting: `(add (scalar 2) (tensor [1 2 3]))` works

### 1.8 Reductions

Tree reduction shader dispatched in multiple passes for arrays larger than
one workgroup. Final result is a scalar tensor (shape `[]`).

```clojure
(defn sum
  "Sum all elements. Returns a scalar tensor."
  [a]
  (dispatch-reduction! shaders/sum-shader a))

(defn mean
  "Mean of all elements. Returns a scalar tensor."
  [a]
  (divide (sum a) (scalar (:size a))))

(defn std
  "Standard deviation. Returns a scalar tensor."
  [a]
  (let [mu   (mean a)
        diff (subtract a mu)]
    (sqrt (mean (square diff)))))
```

- [x] Sum reduction (multi-pass for large arrays)
- [x] `mean` via `sum` / `scalar(n)`
- [ ] `std` via `mean`, `square`, `sqrt`

### 1.9 Shape operations

CPU-only metadata manipulation. No GPU work unless data movement is needed.

```clojure
(defn reshape
  "Reshape tensor. Same buffer, new shape metadata."
  [a new-shape]
  (assert (= (apply * new-shape) (:size a))
          "reshape: total size must match")
  (->Tensor (:buffer a) new-shape (compute-strides new-shape)
            (:size a) :f32 (:device a)))

(defn slice
  "Slice along first axis. Returns a view (shared buffer, offset)."
  [a start end]
  ;; ...offset into same buffer, adjust shape/strides...
  )
```

- [x] `reshape` — zero-cost view with new shape
- [x] `slice` — extract sub-tensor along any dimension
- [x] `flatten` — reshape to `[n]`
- [x] `concat` — allocate new buffer, copy segments
- [x] `stack` — add a new axis and concat

### 1.10 Random number generation

GPU-side PCG RNG. State is a GPU buffer of uint32 seeds, one per thread.

```clojure
(defn rand-uniform
  "GPU-side uniform random in [0, 1). Returns a tensor."
  [shape]
  ;; Dispatch PCG shader, returns f32 tensor
  )

(defn randn
  "GPU-side standard normal via Box-Muller. Returns a tensor."
  [shape]
  ;; Dispatch Box-Muller shader over uniform pairs
  )
```

- [x] PCG state management (seed buffer, per-dispatch increment)
- [x] `rand-uniform` — dispatch PCG shader
- [x] `randn` — dispatch Box-Muller over uniform pairs
- [x] Seeding: deterministic seeds from a ClojureScript-side counter

### 1.11 Async readback

The only async functions in the tensor API:

```clojure
(defn to-number
  "Read a scalar tensor back to a JS number. Returns Promise<number>."
  [a]
  (assert (= (:size a) 1) "to-number: tensor must be scalar")
  (p/let [staging (create-staging-buffer! (:device a) 4)]
    (copy-buffer-to-buffer! (:device a) (:buffer a) staging 4)
    (p/let [_ (.mapAsync staging js/GPUMapMode.READ)]
      (let [result (aget (js/Float32Array. (.getMappedRange staging)) 0)]
        (.unmap staging)
        (release-buffer! staging 4)
        result))))

(defn to-clj
  "Read a tensor to a ClojureScript vector. Returns Promise<vector>."
  [a]
  (p/let [staging (create-staging-buffer! (:device a) (* 4 (:size a)))]
    (copy-buffer-to-buffer! (:device a) (:buffer a) staging (* 4 (:size a)))
    (p/let [_ (.mapAsync staging js/GPUMapMode.READ)]
      (let [arr  (js/Float32Array. (.slice (.getMappedRange staging) 0))
            data (vec (js->clj arr))]
        (.unmap staging)
        (release-buffer! staging (* 4 (:size a)))
        ;; reshape into nested vectors matching tensor shape
        (reshape-clj data (:shape a))))))
```

- [x] `to-number` — scalar tensor → `Promise<number>`
- [x] `to-clj` — tensor → `Promise<vector>` (nested to match shape)
- [x] `reshape-clj` helper to nest flat vec into shape `[2 3]` → `[[...] [...]]`
- [ ] Staging buffer reuse via pool

### 1.12 Device context management

The tensor ops need access to the GPU device. Use a module-level volatile
(matching the codebase's `volatile!` pattern for mutable singletons).

```clojure
(defonce ^:private *ctx* (volatile! nil))

(defn set-context!
  "Set the active GPU context. Called by init!."
  [ctx]
  (vreset! *ctx* ctx))

(defn- ctx
  "Get the active GPU context. Throws if not initialized."
  []
  (or @*ctx*
      (throw (ex-info "GPU not initialized. Call (prob.gpu.device/init!) first."
                      {:type ::not-initialized}))))
```

- [x] Module-level `volatile!` for device context
- [x] `init!` sets context, returns Promise
- [x] All ops read context via `(ctx)` helper
- [x] Clear error message if GPU not initialized

### 1.13 Phase 1 tests

Mirror the structure of `test/mlx_smoke_test.cljs`:

```clojure
(ns gpu-tensor-test
  (:require [promesa.core :as p]
            [prob.gpu.tensor :as t]
            [prob.gpu.device :as dev]))

(defn pass [name] (println (str "  PASS: " name)))
(defn fail [name msg] (println (str "  FAIL: " name " - " msg)))
(defn approx= [a b tol] (< (js/Math.abs (- a b)) tol))

(p/let [_ (dev/init!)]
  ;; Test 1: Array creation and readback
  (p/let [v (t/to-clj (t/tensor [1 2 3 4 5]))]
    (if (= v [1 2 3 4 5])
      (pass "tensor creation + readback")
      (fail "tensor creation" (str v))))

  ;; Test 2: Arithmetic
  (p/let [v (t/to-clj (t/add (t/tensor [1 2 3]) (t/tensor [10 20 30])))]
    (if (= v [11 22 33])
      (pass "add")
      (fail "add" (str v))))

  ;; ... more tests ...
  )
```

- [x] Creation + readback round-trip
- [x] Each arithmetic op (add, sub, mul, div)
- [x] Each unary op (exp, log, neg, sqrt, square)
- [x] Reductions (sum, mean)
- [x] Comparison + where
- [x] Shape ops (reshape, slice, concat, stack, transpose, matmul, arange)
- [x] Random (randn, rand-uniform) — verify shape and rough statistics
- [x] Scalar broadcasting
- [x] Run all tests from nbb
- [ ] Run all tests from browser (create test HTML page)

---

## Phase 2: Autograd

**Goal:** Tape-based reverse-mode automatic differentiation. `grad` and
`value-and-grad` that work through all tensor ops.

### 2.1 Tape and tracked tensors

Tracked tensors wrap regular tensors with a reference to the autograd tape.
The tape is a `volatile!` holding a persistent vector of op records.

```clojure
(ns prob.gpu.autograd
  "Reverse-mode automatic differentiation over GPU tensors."
  (:require [prob.gpu.tensor :as t]))

(defrecord TrackedTensor [tensor grad-fn tape id requires-grad])

(defn- track
  "Wrap a tensor for gradient tracking."
  ([tensor tape]
   (track tensor tape nil true))
  ([tensor tape grad-fn requires-grad]
   (let [id (str (gensym "t"))]
     (->TrackedTensor tensor grad-fn tape id requires-grad))))

(defn- record-op!
  "Record an operation on the tape."
  [tape op inputs output]
  (vswap! tape conj {:op op :inputs inputs :output output}))
```

- [x] `TrackedTensor` record
- [x] `track` helper to wrap tensors
- [x] `record-op!` appends to tape

### 2.2 Tracked arithmetic ops

Each tracked op:
1. Extracts the raw tensors
2. Dispatches the forward GPU kernel (reuses Phase 1 ops)
3. Records the backward rule on the tape
4. Returns a new `TrackedTensor`

```clojure
(defn tracked-add [a b]
  (let [tape    (:tape a)
        raw-out (t/add (:tensor a) (:tensor b))
        out     (track raw-out tape)]
    (record-op! tape
      {:backward (fn [grad-out]
                   ;; d(a+b)/da = 1, d(a+b)/db = 1
                   {(:id a) grad-out
                    (:id b) grad-out})}
      [a b] out)
    out))

(defn tracked-multiply [a b]
  (let [tape    (:tape a)
        raw-out (t/multiply (:tensor a) (:tensor b))
        out     (track raw-out tape)]
    (record-op! tape
      {:backward (fn [grad-out]
                   ;; d(a*b)/da = b, d(a*b)/db = a
                   {(:id a) (t/multiply grad-out (:tensor b))
                    (:id b) (t/multiply grad-out (:tensor a))})}
      [a b] out)
    out))
```

- [x] `tracked-add` with backward: identity
- [x] `tracked-subtract` with backward: identity, negate
- [x] `tracked-multiply` with backward: swap inputs
- [x] `tracked-divide` with backward: quotient rule
- [x] `tracked-negative` with backward: negate
- [x] `tracked-exp` with backward: multiply by output
- [x] `tracked-log` with backward: reciprocal
- [x] `tracked-sqrt` with backward: `1 / (2 * sqrt(x))`
- [x] `tracked-square` with backward: `2 * x`
- [x] `tracked-sum` with backward: broadcast scalar grad
- [x] `tracked-matmul` with backward: transposed matmuls

### 2.3 Backward pass

Walk the tape in reverse, accumulate gradients, dispatch GPU kernels:

```clojure
(defn- backward!
  "Run the backward pass. Populates gradient buffers on tracked tensors."
  [loss tape]
  (let [grad-map (volatile! {(:id loss) (t/ones (:shape (:tensor loss)))})]
    (doseq [entry (rseq @tape)]
      (let [{:keys [backward]} (:op entry)
            out-id             (:id (:output entry))
            grad-out           (get @grad-map out-id)]
        (when grad-out
          (let [input-grads (backward (:tensor grad-out))]
            (doseq [[id g] input-grads]
              (vswap! grad-map update id
                      (fn [existing]
                        (if existing (t/add existing g) g))))))))))
```

- [x] Reverse tape traversal
- [x] Gradient accumulation (handles fan-out correctly)
- [x] Seed gradient with ones for loss tensor
- [ ] Handle `stop-gradient` (skip in backward)

### 2.4 Public API: `grad` and `value-and-grad`

```clojure
(defn grad
  "Returns a function that computes the gradient of f w.r.t. its input."
  [f]
  (fn [params]
    (let [tape    (volatile! [])
          tracked (track params tape)
          loss    (f tracked)]
      (backward! loss tape)
      (get @(:grad-map loss) (:id tracked)))))

(defn value-and-grad
  "Returns a function that computes [value, gradient]."
  [f]
  (fn [params]
    (let [tape    (volatile! [])
          tracked (track params tape)
          loss    (f tracked)]
      (backward! loss tape)
      [(:tensor loss)
       (get @(:grad-map loss) (:id tracked))])))
```

- [x] `grad` — returns gradient function
- [x] `value-and-grad` — returns `[value gradient]`
- [x] Make tracked ops transparent: detect tracked vs raw tensor, dispatch accordingly
- [ ] Nested `grad` calls (second derivative) — not required for Phase 2

### 2.5 Transparent dispatch

The tensor module's public API should auto-detect tracked tensors so the
user's model function works unchanged with or without autograd:

```clojure
;; In prob.gpu.tensor:
(defn add [a b]
  (if (or (instance? TrackedTensor a) (instance? TrackedTensor b))
    (autograd/tracked-add a b)
    (dispatch-binary! shaders/add-shader a b)))
```

- [x] Every op in `prob.gpu.tensor` checks for TrackedTensor
- [x] Autograd module only needs to be loaded if `grad` is called
- [x] Model functions work identically with raw tensors and tracked tensors

### 2.6 Phase 2 tests

Verify gradients against finite differences:

```clojure
(defn finite-diff-grad
  "Numerical gradient via central differences."
  [f x epsilon]
  (p/let [x-val (t/to-clj x)]
    (p/let [grads (p/all
                    (for [i (range (count x-val))]
                      (let [x+ (update x-val i + epsilon)
                            x- (update x-val i - epsilon)]
                        (p/let [f+ (t/to-number (f (t/tensor x+)))
                                f- (t/to-number (f (t/tensor x-)))]
                          (/ (- f+ f-) (* 2 epsilon))))))]
      (vec grads))))
```

- [x] `f(x) = x^2`, gradient at x=3 should be 6
- [x] `f(x) = sum(x^2)` for vector x, gradient should be `2x`
- [x] `f(x) = exp(x)`, gradient should be `exp(x)`
- [x] `f(x) = log(x)`, gradient should be `1/x`
- [x] `f(x) = sum(a * x)` where a is constant, gradient should be `a`
- [x] Composition: `f(x) = sum(exp(x * 2))`, verify chain rule
- [x] Finite difference comparison for all ops within tolerance 1e-3
- [ ] Run from nbb and browser

---

## Phase 3: HMC

**Goal:** Hamiltonian Monte Carlo with GPU-side accept/reject. Returns
plain ClojureScript data via a single Promesa call.

### 3.1 Leapfrog integrator

All synchronous GPU ops — no readback within the trajectory:

```clojure
(ns prob.gpu.inference
  "GPU-accelerated inference: HMC, NUTS, VI.
   All functions return Promises that resolve to plain ClojureScript data."
  (:require [promesa.core :as p]
            [prob.gpu.tensor :as t]
            [prob.gpu.autograd :as ad]))

(defn- leapfrog-step
  "One leapfrog step. All GPU ops, no readback."
  [grad-fn q p step-size]
  (let [g       (grad-fn q)
        p-half  (t/add p (t/multiply (t/scalar (* 0.5 step-size)) g))
        q-new   (t/add q (t/multiply (t/scalar step-size) p-half))
        g-new   (grad-fn q-new)
        p-new   (t/add p-half (t/multiply (t/scalar (* 0.5 step-size)) g-new))]
    [q-new p-new]))

(defn- leapfrog
  "Full leapfrog trajectory. Returns [q' p']."
  [grad-fn q p step-size n-steps]
  (loop [i 0, q q, p p]
    (if (>= i n-steps)
      [q p]
      (let [[q' p'] (leapfrog-step grad-fn q p step-size)]
        (recur (inc i) q' p')))))
```

- [x] `leapfrog-step` — half-step momentum, full-step position, half-step momentum
- [x] `leapfrog` — loop of leapfrog steps (synchronous, no readback)

### 3.2 HMC with GPU-side accept/reject

```clojure
(defn- hamiltonian
  "H(q, p) = -log_density(q) + 0.5 * sum(p^2). Returns scalar tensor."
  [log-density q p]
  (t/add (t/negative (log-density q))
         (t/multiply (t/scalar 0.5) (t/sum (t/square p)))))

(defn- hmc-step
  "One HMC step. GPU-side accept/reject, no readback."
  [log-density grad-fn q step-size n-leapfrog]
  (let [p           (t/randn (t/shape q))
        current-H   (hamiltonian log-density q p)
        [q' p']     (leapfrog grad-fn q p step-size n-leapfrog)
        proposed-H  (hamiltonian log-density q' p')
        log-alpha   (t/subtract current-H proposed-H)
        u           (t/log (t/rand-uniform [1]))
        accept?     (t/greater log-alpha u)
        next-q      (t/where accept? q' q)]
    next-q))
```

- [x] `hamiltonian` — kinetic + potential energy as GPU tensor
- [x] `hmc-step` — propose + accept/reject entirely on GPU
- [x] No `mapAsync` calls within the step

### 3.3 Sample collection loop

Synchronous loop collecting samples on GPU. Single bulk readback at end:

```clojure
(defn hmc
  "Hamiltonian Monte Carlo. Returns Promise<{:samples :acceptance-rate}>."
  [opts log-density init-params]
  (let [{:keys [samples step-size leapfrog-steps burn on-progress]
         :or   {burn 0}} opts
        grad-fn  (t/grad log-density)
        total    (+ samples burn)]
    (p/let [_ (dev/ensure-init!)]
      (let [all-samples (loop [i 0, q init-params, acc (transient [])]
                          (if (>= i total)
                            (persistent! acc)
                            (let [q' (hmc-step log-density grad-fn q
                                               step-size leapfrog-steps)]
                              (when (and on-progress (zero? (mod i 100)))
                                (on-progress {:iteration i}))
                              (recur (inc i) q'
                                     (if (>= i burn)
                                       (conj! acc q')
                                       acc)))))]
        ;; Single bulk readback: GPU tensors → ClojureScript vectors
        (p/let [clj-samples (p/all (mapv t/to-clj all-samples))]
          {:samples         clj-samples
           :acceptance-rate nil})))))  ;; TODO: track on GPU
```

- [x] `hmc` main entry point
- [x] Burn-in: discard first `burn` samples
- [x] Progress callback every N iterations
- [x] Bulk readback via `p/all` + `t/to-clj`
- [x] Return plain ClojureScript map
- [ ] Dispose intermediate GPU tensors to prevent OOM
- [x] Track acceptance rate on GPU (count via where + sum)

### 3.4 Phase 3 tests

```clojure
;; Test: 1D Gaussian N(3, 1), posterior mean should be ~3
(p/let [result (infer/hmc {:samples 500 :step-size 0.1
                            :leapfrog-steps 10 :burn 100}
                           (fn [x] (t/multiply (t/scalar -0.5)
                                               (t/square (t/subtract x (t/scalar 3.0)))))
                           (t/scalar 0.0))]
  (let [m (prob/mean (:samples result))]
    (if (approx= m 3.0 0.3)
      (pass (str "HMC 1D: mean=" m))
      (fail "HMC 1D" (str "mean=" m)))))
```

- [x] 1D Gaussian: recover mean ~3.0
- [x] 2D Gaussian: recover mean `[2, -1]`
- [ ] Bayesian linear regression: recover `w ~2, b ~1`
- [x] Acceptance rate > 0.5 for well-tuned step size
- [ ] Run from nbb and browser
- [ ] Compare results to MLX version (statistical equivalence)

---

## Phase 4: NUTS

**Goal:** No-U-Turn Sampler with adaptive trajectory length.

### 4.1 Tree building

NUTS builds a binary tree and checks the U-turn criterion. The U-turn check
requires reading back a scalar, so this uses `p/loop`:

```clojure
(defn- check-u-turn
  "U-turn criterion. Returns scalar tensor (GPU-side boolean)."
  [q-minus q-plus p-minus p-plus]
  (let [dq (t/subtract q-plus q-minus)]
    (t/greater (t/minimum (t/sum (t/multiply dq p-plus))
                          (t/sum (t/multiply dq p-minus)))
               (t/scalar 0))))
```

- [ ] `build-tree` — recursive doubling with `p/let` at U-turn check
- [ ] U-turn criterion on GPU, single readback per tree level
- [ ] Multinomial sampling for trajectory selection
- [ ] `nuts` main entry point matching HMC signature

### 4.2 Phase 4 tests

- [ ] 1D Gaussian: recover mean ~5.0
- [ ] 2D Gaussian: recover mean `[2, -1]`
- [ ] Compare trajectory lengths to expected values
- [ ] Run from nbb and browser

---

## Phase 5: Variational Inference (ADVI)

**Goal:** Mean-field Gaussian ADVI with Adam optimizer.

### 5.1 Adam optimizer

```clojure
(defn- adam-step
  "One Adam update. All GPU ops, no readback."
  [{:keys [m v t]} params grad
   {:keys [learning-rate beta1 beta2 epsilon]}]
  (let [t'  (inc t)
        m'  (t/add (t/multiply (t/scalar beta1) m)
                   (t/multiply (t/scalar (- 1 beta1)) grad))
        v'  (t/add (t/multiply (t/scalar beta2) v)
                   (t/multiply (t/scalar (- 1 beta2)) (t/square grad)))
        m-hat (t/divide m' (t/scalar (- 1 (js/Math.pow beta1 t'))))
        v-hat (t/divide v' (t/scalar (- 1 (js/Math.pow beta2 t'))))
        step  (t/divide (t/multiply (t/scalar learning-rate) m-hat)
                        (t/add (t/sqrt v-hat) (t/scalar epsilon)))]
    {:state {:m m' :v v' :t t'}
     :params (t/subtract params step)}))
```

- [ ] Adam optimizer (GPU-side, no readback)
- [ ] ELBO estimation via reparameterization trick
- [ ] `vi` main entry point
- [ ] Returns `{:mu :sigma :elbo-history :sample-fn}`
- [ ] Optimization loop via `p/loop`

### 5.2 Phase 5 tests

- [ ] 1D Gaussian: mu converges to 3.0, sigma to 1.0
- [ ] 2D Gaussian: mu converges to `[2, -1]`, sigma to `[1, 1]`
- [ ] ELBO increases over training
- [ ] Sample function produces correct distribution
- [ ] Run from nbb and browser

---

## Phase 6: SCI Registration + Scittle Plugin

**Goal:** Make all GPU namespaces available in `<script type="application/x-scittle">`.

### 6.1 SCI namespace registration

Following the pattern in `src/prob/sci.cljs`:

```clojure
(ns prob.gpu.sci
  "SCI configuration for GPU tensor and inference namespaces."
  (:require [sci.core :as sci]
            [prob.gpu.tensor :as t]
            [prob.gpu.inference :as infer]
            [prob.gpu.device :as dev]))

(def tensor-ns (sci/create-ns 'prob.gpu.tensor nil))
(def infer-ns  (sci/create-ns 'prob.gpu.inference nil))
(def device-ns (sci/create-ns 'prob.gpu.device nil))

(def tensor-namespace
  {'tensor         (sci/copy-var t/tensor tensor-ns)
   'scalar         (sci/copy-var t/scalar tensor-ns)
   'zeros          (sci/copy-var t/zeros tensor-ns)
   'add            (sci/copy-var t/add tensor-ns)
   'subtract       (sci/copy-var t/subtract tensor-ns)
   'multiply       (sci/copy-var t/multiply tensor-ns)
   ;; ... all tensor ops ...
   'grad           (sci/copy-var t/grad tensor-ns)
   'value-and-grad (sci/copy-var t/value-and-grad tensor-ns)
   'to-number      (sci/copy-var t/to-number tensor-ns)
   'to-clj         (sci/copy-var t/to-clj tensor-ns)})

(def inference-namespace
  {'hmc  (sci/copy-var infer/hmc infer-ns)
   'nuts (sci/copy-var infer/nuts infer-ns)
   'vi   (sci/copy-var infer/vi infer-ns)})

(def device-namespace
  {'init! (sci/copy-var dev/init! device-ns)})

(def config
  {:namespaces {'prob.gpu.tensor    tensor-namespace
                'prob.gpu.inference inference-namespace
                'prob.gpu.device    device-namespace}})
```

- [ ] Create `src/prob/gpu/sci.cljs`
- [ ] Register every public var from `prob.gpu.tensor`
- [ ] Register every public var from `prob.gpu.inference`
- [ ] Register `prob.gpu.device/init!`

### 6.2 Scittle plugin entry point

```clojure
(ns scittle.prob-gpu
  {:no-doc true}
  (:require [prob.gpu.sci :refer [config]]
            [scittle.core :as scittle]))

(defn init []
  (scittle/register-plugin! ::prob-gpu config))
```

- [ ] Create `src/scittle/prob_gpu.cljs`
- [ ] Add build target to `scittle/shadow-cljs.edn`

### 6.3 shadow-cljs build configuration

```edn
;; In scittle/shadow-cljs.edn, add to :builds :plugin :modules:
:scittle.prob-gpu
{:entries [scittle.prob-gpu]
 :init-fn scittle.prob-gpu/init
 :depends-on #{:scittle}}
```

- [ ] Add `:scittle.prob-gpu` module to shadow-cljs config
- [ ] `npm run build` produces `scittle.prob-gpu.js`
- [ ] Verify plugin loads alongside existing `scittle.prob.js`

---

## Phase 7: Integration Tests + Demo

**Goal:** End-to-end verification in both runtimes. Interactive browser demo.

### 7.1 Unified test suite

One test file that runs in both nbb and browser:

```clojure
(ns gpu-test
  (:require [promesa.core :as p]
            [prob.gpu.tensor :as t]
            [prob.gpu.inference :as infer]
            [prob.gpu.device :as dev]
            [prob.core :as prob]))

(defn approx= [a b tol] (< (js/Math.abs (- a b)) tol))

(p/let [_ (dev/init!)]
  (println "\n=== GPU Tensor Tests ===")
  ;; ... tensor tests ...

  (println "\n=== Autograd Tests ===")
  ;; ... gradient tests ...

  (println "\n=== HMC Tests ===")
  ;; ... HMC tests ...

  (println "\n=== NUTS Tests ===")
  ;; ... NUTS tests ...

  (println "\n=== VI Tests ===")
  ;; ... VI tests ...

  (println "\n=== All GPU tests complete ==="))
```

- [ ] Write `test/gpu_test.cljs`
- [ ] Run via `nbb -cp src:test test/gpu_test.cljs`
- [ ] Create `test/gpu_test.html` for browser testing
- [ ] All tests pass in Chrome, Safari, Firefox
- [ ] All tests pass in nbb

### 7.2 Browser demo page

Interactive demo at `docs/webgpu/index.html`:

- [ ] GPU capability detection (show "WebGPU not supported" if missing)
- [ ] Bayesian linear regression example
- [ ] Progress bar during sampling
- [ ] Display posterior samples as histogram (using canvas or SVG)
- [ ] Show acceptance rate and timing
- [ ] Link from `docs/index.html`

### 7.3 nbb example scripts

- [ ] `examples/gpu/gaussian.cljs` — 1D Gaussian inference
- [ ] `examples/gpu/linear-regression.cljs` — Bayesian linear regression
- [ ] `examples/gpu/comparison.cljs` — compare CPU vs GPU timing

---

## File Inventory

### New files to create

```
src/prob/gpu/
  device.cljs          ~50 lines   WebGPU init, environment detection
  shaders.cljs         ~200 lines  WGSL shader source strings
  tensor.cljs          ~500 lines  Tensor record, ops, buffer pool, readback
  autograd.cljs        ~300 lines  Tape, tracked tensors, backward pass
  inference.cljs       ~400 lines  HMC, NUTS, VI
  sci.cljs             ~100 lines  SCI namespace registration

src/scittle/
  prob_gpu.cljs        ~10 lines   Scittle plugin entry point

test/
  gpu_spike_test.cljs  ~50 lines   Phase 0 toolchain validation
  gpu_tensor_test.cljs ~200 lines  Phase 1 tensor ops
  gpu_autograd_test.cljs ~150 lines Phase 2 gradient verification
  gpu_inference_test.cljs ~200 lines Phase 3-5 inference tests
  gpu_test.html        ~30 lines   Browser test harness

docs/webgpu/
  index.html           ~100 lines  Interactive browser demo

examples/gpu/
  gaussian.cljs        ~30 lines   Basic example
  linear-regression.cljs ~50 lines Bayesian regression example
```

### Files to modify

```
scittle/shadow-cljs.edn   Add :scittle.prob-gpu build module
scittle/package.json       Add webgpu dependency (for dev/testing only)
docs/index.html            Link to WebGPU demo
GAPS.md                    Update GPU coverage status
```

### Total new code: ~1550 lines ClojureScript + ~360 lines WGSL

---

## Style Guide (match existing codebase)

- **Namespace docstrings**: multi-line, describe purpose and key exports
- **Private helpers**: `defn-` for functions, `def ^:private` for values
- **State**: `volatile!` with persistent maps (never atoms)
- **Errors**: `ex-info` with namespaced keyword maps `{:type ::error-kind}`
- **Impossible values**: `##-Inf` for log-probabilities
- **JS interop**: `(.-prop obj)`, `(.method obj args)`, `clj->js`/`js->clj` at boundaries
- **Collections in loops**: `(transient [])` + `conj!` + `persistent!`
- **Loop pattern**: `(loop [i 0, state init, acc (transient [])] ...)`
- **Async**: Promesa `p/let`, `p/loop`/`p/recur`, `p/all` — never raw `.then`
- **Naming**: `hyphenated-lowercase`, predicates end in `?`, mutating fns end in `!`
- **Section separators**: `;;` with `---` lines between major sections
- **Docstrings**: one-line summary, optional second paragraph, no `@param`
- **Adding public API**: add to source module + `sci.cljs` registration

---

## Definition of Done

Each phase is complete when:

1. All checkboxes for that phase are ticked
2. Tests pass from both nbb and browser
3. No regressions in existing `prob.core` tests
4. Code follows the style guide above
5. No raw `.then` — all async uses Promesa
