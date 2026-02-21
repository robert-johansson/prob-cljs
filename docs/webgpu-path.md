# WebGPU Path: Universal GPU-Accelerated prob-cljs

## The Problem

prob-cljs has GPU-accelerated gradient-based inference (HMC, NUTS, VI) via `prob.mlx`, but it only works on Apple Silicon with nbb. The browser deployment (Scittle) is entirely CPU-bound, and nbb on Linux/Windows has no GPU acceleration at all.

WebGPU changes this. It has shipped in all major browsers (Chrome 113+, Safari 26+, Firefox 141+), and Google's Dawn engine is available as a Node.js package (`webgpu` on npm), giving us GPU compute in both runtimes.

## Platform Coverage

**Current state** — GPU acceleration is Apple Silicon only:

| Platform | nbb (terminal) | Browser (Scittle) |
|---|---|---|
| macOS Apple Silicon | MLX (Metal) | CPU only |
| macOS Intel | CPU only | CPU only |
| Linux + NVIDIA/AMD | CPU only | CPU only |
| Windows + NVIDIA/AMD | CPU only | CPU only |

**With a WebGPU tensor engine** — GPU acceleration everywhere:

| Platform | nbb (terminal) | Browser (Scittle) |
|---|---|---|
| macOS Apple Silicon | MLX (Metal) **or** WebGPU (Metal) | **WebGPU (Metal)** |
| macOS Intel | **WebGPU (Metal)** | **WebGPU (Metal)** |
| Linux + NVIDIA/AMD | **WebGPU (Vulkan)** | **WebGPU (Vulkan)** |
| Windows + NVIDIA/AMD | **WebGPU (D3D12)** | **WebGPU (D3D12)** |

One codebase, one set of WGSL shaders, runs in browser and terminal, on any GPU.

### How: the `webgpu` npm package

The npm package [`webgpu`](https://www.npmjs.com/package/webgpu) (v0.3.8, last updated September 2025) ships Google's Dawn engine as a prebuilt Node.js addon. No compilation needed.

| Property | Value |
|---|---|
| Install | `npm install webgpu` (2 seconds, zero build step) |
| Platforms | macOS universal, Linux x64, Windows x64 (prebuilt binaries) |
| Backend | Dawn (same WebGPU implementation as Chrome) |
| Size | ~54 MB (all platform binaries bundled) |
| Node.js | >= 18 |

Verified working from nbb on this machine.

The API difference between browser and Node.js is trivial:

```clojure
(defn get-gpu []
  (if (exists? js/navigator.gpu)
    js/navigator.gpu                          ;; browser
    (let [webgpu (js/require "webgpu")]
      (.create webgpu #js []))))              ;; Node.js / nbb
```

After `requestDevice()`, every WebGPU call is identical across environments. The WGSL shaders, buffer management, compute pipelines — all the same code.

### MLX vs WebGPU on Apple Silicon

On Apple Silicon, both MLX and WebGPU target the same Metal GPU. MLX will likely be faster due to unified memory (zero-copy CPU/GPU) and a mature runtime. WebGPU goes through Dawn's Metal backend with explicit buffer copies. For users who have MLX set up, it remains the best choice on Apple Silicon. WebGPU is the universal fallback — and the only option on every other platform.

## Why Not Wrap an Existing Library?

We evaluated every WebGPU tensor/compute library in the ecosystem:

| Library | Status | Why not |
|---|---|---|
| **webgpu-torch** | Abandoned (last commit July 2023, last npm release May 2023) | No maintainer response to issues, basic autograd bugs unfixed |
| **surfgrad** | Dormant (last commit May 2025) | Only 5 operations (matmul, mul, add, exp, log) |
| **TensorFlow.js WebGPU** | Slowing (Google shifting focus) | Massive bundle, inference-only, no custom autograd |
| **ONNX Runtime Web** | Active but wrong shape | Model inference runtime, not a tensor library |
| **gpu.js** | No WebGPU support | WebGL only, maintenance mode |
| **TypeGPU** | Active (Software Mansion, 2.1k stars) | Lower-level WebGPU toolkit — no tensors, no autograd; type-safety value lost from ClojureScript; 72 KB gzipped for ~100 lines of saved boilerplate; compute API marked `~unstable` |

There is no maintained, lightweight WebGPU tensor+autograd library. The pattern is consistent: someone builds a PyTorch-lite proof of concept, publishes a blog post, and moves on within months.

**Conclusion: write targeted WGSL compute shaders directly.** No dependencies beyond Promesa (already bundled) and `webgpu` (nbb only). Full control. The WebGPU API is a W3C standard — stable ground.

## Async Strategy: Promesa

### Why Promesa?

WebGPU is inherently async (GPU readback requires `buffer.mapAsync`). ClojureScript's `core.async` won't work — go blocks require compile-time macro transformation that SCI/Scittle can't perform.

**Promesa** is the answer. It's already built into both target runtimes:

| Runtime | Promesa version | How to use |
|---|---|---|
| **nbb** | v11.0.678 (bundled since v0.0.36) | `(require '[promesa.core :as p])` |
| **Scittle** | Built-in plugin on CDN (~14 KB gzipped) | Add `<script src="scittle.promesa.js">` |

Zero additional dependencies. Full SCI macro support including `p/let`, `p/do`, `p/loop`, `p/recur`.

### What Promesa gives us

**`p/let`** — sequential async bindings that look like synchronous code:

```clojure
;; Without Promesa (nested callback pyramid):
(.then (infer/hmc opts log-density init)
  (fn [result]
    (.then (t/to-clj (:samples result))
      (fn [samples]
        (println "mean:" (prob/mean samples))))))

;; With Promesa (flat, readable, nearly synchronous):
(p/let [result  (infer/hmc opts log-density init)
        samples (t/to-clj (:samples result))]
  (println "mean:" (prob/mean samples)))
```

**`p/loop` / `p/recur`** — async iteration that looks exactly like ClojureScript's `loop/recur`:

```clojure
;; NUTS sampling loop (needs async readback for U-turn check)
(p/loop [step 0, state init-state, samples []]
  (if (>= step n-samples)
    {:samples samples :state state}
    (p/let [next (nuts-step! state log-density opts)]
      (p/recur (inc step)
               (:state next)
               (conj samples (:params next))))))
```

This eliminates the need for a custom Promise trampoline. Promesa gives us proper async `loop/recur` for free.

**`p/->`** — async threading:

```clojure
(p/-> (infer/hmc opts log-density init)
      :samples
      prob/mean
      (fn [m] (println "posterior mean:" m)))
```

## Architecture: Eager Tensors, Hidden Async

### The key insight

In probabilistic programming, the user writes a **model** (log-density function) and calls an **inference algorithm**. The inference algorithm runs the model thousands of times internally. The user just wants samples back. **The system owns the entire execution loop, so it can own all the async.**

```
USER (synchronous)                INFERENCE ENGINE (async internally)

(defn log-density [params]   -->  called 1000x inside HMC loop
  (t/sum (t/square ...)))        each call encodes GPU commands (sync)
                                  accept/reject on GPU via t/where (sync)
                                  bulk readback all samples at end (async)

(p/let [result                <-- plain ClojureScript map
        (infer/hmc opts f x0)]
  (println (mean (:samples result))))
```

**One `p/let` binding. That's the entire async surface the user sees.** This is the same whether running in a browser or in nbb.

### Why this works

WebGPU command encoding is synchronous. You build a command buffer, submit it, and the GPU runs asynchronously in the background. JavaScript doesn't block. The only async operation is reading data *back* from GPU (`buffer.mapAsync`).

This means tensor operations are naturally synchronous — they just enqueue GPU work. The user's model function is plain ClojureScript with no async contamination:

```clojure
;; Synchronous. No promises, no callbacks, no channels.
;; Works identically in nbb and Scittle.
(defn log-density [params]
  (let [diff (t/subtract params (t/tensor [2.0 -1.0]))]
    (t/multiply (t/scalar -0.5)
                (t/sum (t/multiply diff diff)))))
```

### GPU-side accept/reject

The biggest design win over the MLX approach: **do accept/reject entirely on GPU**, eliminating per-iteration readbacks.

Instead of reading back the acceptance scalar to make a CPU decision:

```clojure
;; BAD: reads back a scalar every iteration (~1-3ms each)
(p/let [la (t/to-number log-alpha)]    ;; async readback
  (if (> la (Math/log (Math/random)))  ;; CPU decision
    proposed current))
```

Use `t/where` to make the decision on GPU:

```clojure
;; GOOD: everything stays on GPU, zero readbacks during sampling
(let [log-alpha  (t/subtract proposed-H current-H)
      u          (t/log (t/rand-uniform [1]))
      accept?    (t/greater log-alpha u)           ;; GPU comparison
      next-state (t/where accept? proposed current)] ;; GPU conditional
  next-state)
```

For fixed-trajectory HMC, this means **zero readbacks during the entire sampling loop**. Run 1000 iterations, store all samples on GPU, bulk-read once at the end. One async call total.

NUTS still needs one readback per iteration (for the U-turn criterion), handled cleanly by `p/loop`:

```clojure
(p/loop [step 0, state init, tree-stats []]
  (if (>= step n-samples)
    (p/let [samples (t/to-clj (t/stack (map :params tree-stats)))]
      {:samples samples})
    (p/let [u-turn? (t/to-number (check-u-turn ...))]  ;; one readback per tree
      (p/recur (inc step) next-state (conj tree-stats result)))))
```

## API Design: Breaking MLX Compatibility

The WebGPU version should **not** mirror the MLX API. Promesa and GPU-side control flow enable a cleaner design that works uniformly across browser and terminal.

### Tensor operations (all synchronous)

```clojure
(ns prob.gpu.tensor)

;; Creation — returns GPU tensor handles, no promises
(t/tensor [1 2 3])          ;; from ClojureScript data
(t/scalar 3.0)              ;; scalar
(t/zeros [3])               ;; zeros
(t/randn [3])               ;; GPU-side random normal

;; Arithmetic — synchronous, returns GPU tensors
(t/add a b)
(t/subtract a b)
(t/multiply a b)
(t/divide a b)
(t/negative a)

;; Math — synchronous
(t/exp a)
(t/log a)
(t/sqrt a)
(t/square a)

;; Reductions — return GPU scalar tensors, NOT numbers
(t/sum a)
(t/mean a)

;; Comparison + conditional — GPU-side, no readback
(t/greater a b)
(t/less a b)
(t/where cond then else)

;; Shape — CPU metadata only
(t/shape a)
(t/reshape a [2 3])
(t/slice a 0 3)

;; Autograd
(t/grad f)                  ;; returns gradient function
(t/value-and-grad f)        ;; returns [value, gradient] function

;; THE async boundary — ONLY these return Promises
(t/to-number scalar-tensor) ;; Promise<number>
(t/to-clj tensor)           ;; Promise<vector>
```

### Inference (returns Promises, consumed via `p/let`)

```clojure
(require '[promesa.core :as p])
(require '[prob.gpu.tensor :as t])
(require '[prob.gpu.inference :as infer])

;; User defines model as a plain synchronous function
(defn log-density [params]
  (let [x params]
    (t/multiply (t/scalar -0.5)
                (t/square (t/subtract x (t/scalar 3.0))))))

;; HMC — one p/let, plain ClojureScript data back
(p/let [result (infer/hmc {:samples      1000
                            :step-size    0.1
                            :leapfrog-steps 10
                            :burn         200}
                           log-density
                           (t/scalar 0.0))]
  ;; result is a plain ClojureScript map:
  ;; {:samples [[-0.3] [1.2] ...], :acceptance-rate 0.74}
  (println "mean:" (prob/mean (:samples result)))
  (println "acceptance rate:" (:acceptance-rate result)))

;; NUTS
(p/let [result (infer/nuts {:samples 500 :step-size 0.1 :max-depth 5}
                            log-density
                            (t/zeros [2]))]
  (println "mean:" (prob/mean (:samples result))))

;; VI
(p/let [result (infer/vi {:iterations 1000 :learning-rate 0.01}
                          log-density
                          (t/zeros [2]))]
  (println "mu:" (:mu result) "sigma:" (:sigma result)))
```

### Progress callbacks

```clojure
(p/let [result (infer/hmc {:samples      1000
                            :step-size    0.1
                            :leapfrog-steps 10
                            :on-progress  (fn [{:keys [iteration acceptance-rate]}]
                                            (update-progress-bar! iteration))}
                           log-density
                           (t/scalar 0.0))]
  (show-results! result))
```

### Chaining inference steps

Promesa makes multi-step workflows natural:

```clojure
;; Run inference, then use results in another model
(p/let [;; Step 1: infer parameters
        result    (infer/hmc hmc-opts model-1 init-1)
        post-mean (prob/mean (:samples result))

        ;; Step 2: use inferred parameters in second model
        result-2  (infer/hmc hmc-opts (model-2 post-mean) init-2)]
  (println "final:" (:samples result-2)))
```

### What the user never touches

- GPU buffers, `mapAsync`, command encoders, staging buffers
- Bind groups, pipeline objects, WGSL shaders
- Promise chains, `.then`, `.catch` (Promesa handles this)
- Runtime detection (browser vs Node.js — handled internally)

The entire WebGPU machinery is hidden behind tensor operations and `p/let`.

## Backend Abstraction via Protocols

ClojureScript protocols enable clean CPU/GPU portability:

```clojure
(defprotocol ITensorBackend
  (tensor [this data])
  (add [this a b])
  (multiply [this a b])
  (grad [this f])
  (run-hmc [this opts log-density init])
  ...)
```

The same model code works with different backends:

```clojure
;; CPU backend (for testing, fallback, environments without WebGPU)
(def cpu (cpu-backend))

;; WebGPU backend (browser or nbb — auto-detects)
(p/let [gpu (webgpu-backend)]  ;; async init (one-time)
  ;; Same model, same inference call, different backend
  (p/let [result (run-hmc gpu opts log-density init)]
    (println result)))
```

This is a better design than MLX, where the backend is hardcoded via namespace choice. With protocols, the backend is a value — composable, testable, swappable.

## WGSL Shaders

### What we need

**~14 WGSL compute shaders total (~360 lines):**

| Shader | Lines | Covers |
|---|---|---|
| Element-wise binary | ~30 | add, subtract, multiply, divide, where |
| Element-wise unary | ~20 | exp, log, negative, sqrt, square |
| Comparison | ~15 | greater-equal, less-equal |
| Tree reduction | ~40 | sum (reused for mean, std) |
| Tiled matmul | ~60 | matmul |
| RNG uniform | ~25 | PCG generator |
| RNG normal | ~30 | PCG + Box-Muller |
| Broadcast | ~15 | grad of sum |
| Transpose | ~15 | grad of matmul |
| Cholesky | ~80 | multivariate normal (deferrable) |
| Triangular solve | ~50 | multivariate normal (deferrable) |
| Diag extraction | ~10 | multivariate normal (deferrable) |
| lgamma approx | ~30 | distribution log-pdfs |
| erf approx | ~30 | Gaussian CDF |

WGSL has built-in `exp`, `log`, `sqrt`, `pow`, `abs`, `min`, `max`, `sin`, `cos`, `tanh`, `fma`. Missing: `lgamma`, `erf`, `digamma` (need polynomial approximations).

The same shaders run unchanged in browser (native WebGPU) and nbb (Dawn via `webgpu` npm package). WGSL is part of the W3C WebGPU spec — there are no platform-specific shader variants.

### What the runtime needs

| Component | Lines | Notes |
|---|---|---|
| Device init + environment detection | ~50 | Browser vs Node.js, adapter selection |
| Buffer management, pipeline cache | ~150 | WebGPU boilerplate, do once |
| Tensor class (shape, strides, buffer ref) | ~150 | CPU-side metadata |
| Op dispatch (bind groups, dispatch, staging) | ~200 | Repetitive but mechanical |
| Autograd tape + backward rules | ~300 | The hard part |
| Async readback via Promesa | ~50 | `p/let` over `mapAsync` |
| Shape ops (reshape, slice, concat, stack) | ~100 | CPU-only, no shaders |

**Total runtime: ~1000-1200 lines of ClojureScript.**

### The autograd tape

CPU-side tape, same architecture as PyTorch:

1. **Forward pass**: each tensor op dispatches a GPU kernel and records the operation on a CPU-side tape
2. **Backward pass**: walk the tape in reverse, dispatching gradient kernels on GPU
3. **Gradients stay on GPU** — only read back when explicitly requested

```clojure
(defn grad [f]
  (fn [params]
    (let [tape    (volatile! [])
          tracked (track params tape)       ;; wrap tensor to record ops
          result  (f tracked)               ;; user's function runs, tape fills
          _       (backward! result tape)]  ;; reverse pass on GPU
      (gradient tracked))))                 ;; extract gradient tensor
```

Each forward op needs a backward rule. Most reuse existing shaders:

| Forward op | Backward rule | New shader needed? |
|---|---|---|
| `add(a, b)` | `grad_a = grad_out`, `grad_b = grad_out` | No (copy) |
| `multiply(a, b)` | `grad_a = grad_out * b` | No (reuses binary mul) |
| `exp(a)` | `grad_a = grad_out * exp(a)` | No (reuses mul, exp) |
| `log(a)` | `grad_a = grad_out / a` | No (reuses binary div) |
| `sum(a)` | `grad_a = broadcast(grad_out)` | Yes (broadcast) |
| `matmul(A, B)` | `grad_A = grad_out @ B^T` | Yes (transpose) |
| `negative(a)` | `grad_a = -grad_out` | No (reuses unary neg) |
| `sqrt(a)` | `grad_a = grad_out / (2 * sqrt(a))` | No (reuses existing ops) |
| `square(a)` | `grad_a = grad_out * 2 * a` | No |
| `divide(a, b)` | `grad_a = grad_out / b` | No |
| `where(c, a, b)` | `grad_a = where(c, grad_out, 0)` | No (reuses where) |

## Comparison with MLX

| | MLX (current) | WebGPU (proposed) |
|---|---|---|
| Hardware | Apple Silicon only | Any GPU (Intel, AMD, NVIDIA, Apple) |
| Runtimes | nbb only | **nbb + browser** |
| Platforms | macOS only | **macOS, Linux, Windows** |
| Async model | Synchronous (blocks during inference) | Promesa `p/let` (non-blocking) |
| Progress | Callback during blocking loop | Natural — update UI between iterations |
| Accept/reject | CPU readback per iteration | GPU-side via `where` — zero readbacks for HMC |
| Iteration loops | `loop/recur` | `p/loop`/`p/recur` (async-aware) |
| Backend | Hardcoded to MLX namespace | Protocol-based — swappable, testable |
| User model code | Same | Same |
| Dependencies | @frost-beta/mlx (native addon, macOS only) | `webgpu` npm (nbb) / none (browser) |
| Precision | Multiple dtypes | float32 only |

MLX remains the best option on Apple Silicon (unified memory, zero-copy, mature runtime). WebGPU is the universal option that works everywhere else — and the only path to GPU in the browser.

## Examples

### nbb (terminal)

```clojure
#!/usr/bin/env nbb
(ns my-inference
  (:require [promesa.core :as p]
            [prob.gpu.tensor :as t]
            [prob.gpu.inference :as infer]
            [prob.core :as prob]))

;; Model: 1D Gaussian, recover mean ~ 3.0
(defn log-density [params]
  (t/multiply (t/scalar -0.5)
              (t/square (t/subtract params (t/scalar 3.0)))))

(p/let [result (infer/hmc {:samples 1000 :step-size 0.1
                            :leapfrog-steps 10 :burn 200}
                           log-density
                           (t/scalar 0.0))]
  (println "posterior mean:" (prob/mean (:samples result)))
  (println "acceptance rate:" (:acceptance-rate result)))
```

```bash
# Runs on macOS, Linux, or Windows — any machine with a GPU
nbb -cp src my-inference.cljs
```

### Browser (Scittle)

```html
<!DOCTYPE html>
<html>
<head>
  <script src="https://cdn.jsdelivr.net/npm/scittle@0.8.31/dist/scittle.js"></script>
  <script src="https://cdn.jsdelivr.net/npm/scittle@0.8.31/dist/scittle.promesa.js"></script>
  <script src="scittle.prob.js"></script>
  <script src="scittle.prob-gpu.js"></script>
</head>
<body>
  <div id="output">Initializing GPU...</div>
  <script type="application/x-scittle">
    (require '[promesa.core :as p])
    (require '[prob.gpu.tensor :as t])
    (require '[prob.gpu.inference :as infer])
    (require '[prob.core :as prob])

    ;; Model: Bayesian linear regression
    ;; y = wx + b + noise, recover w ~ 2, b ~ 1
    (def xs (t/tensor [1 2 3 4 5]))
    (def ys (t/tensor [3.1 4.9 7.2 8.8 11.1]))

    (defn log-density [params]
      (let [w         (t/slice params 0 1)
            b         (t/slice params 1 2)
            y-hat     (t/add (t/multiply w xs) b)
            residuals (t/subtract ys y-hat)
            obs-lp    (t/multiply (t/scalar -0.5)
                        (t/sum (t/square (t/divide residuals (t/scalar 0.5)))))
            prior-lp  (t/multiply (t/scalar -0.5)
                        (t/sum (t/divide (t/square params) (t/scalar 100))))]
        (t/add obs-lp prior-lp)))

    (p/let [result (infer/hmc {:samples        1000
                               :step-size      0.01
                               :leapfrog-steps 20
                               :burn           200
                               :on-progress    (fn [{:keys [iteration]}]
                                                 (set! (.-textContent
                                                         (js/document.getElementById "output"))
                                                       (str "Sampling: " iteration "/1000")))}
                              log-density
                              (t/zeros [2]))]
      (let [samples (:samples result)
            w-mean  (prob/mean (map first samples))
            b-mean  (prob/mean (map second samples))]
        (set! (.-textContent (js/document.getElementById "output"))
              (str "w = " (.toFixed w-mean 2) " (expected ~2)\n"
                   "b = " (.toFixed b-mean 2) " (expected ~1)\n"
                   "acceptance rate: " (.toFixed (:acceptance-rate result) 2)))))
  </script>
</body>
</html>
```

**Same model function, same inference call, same `p/let` pattern.** The only difference is how the page is set up.

## Risks

| Risk | Impact | Mitigation |
|---|---|---|
| Autograd correctness | Wrong gradients = wrong posteriors | Numerical gradient checking against finite differences |
| Reduction precision | Float32 accumulation errors on large arrays | Kahan summation in reduction shader |
| Pipeline creation latency | First dispatch slow (~50-100ms) | Cache compiled pipelines by shader+layout |
| Buffer allocation churn | GC pressure, OOM | Buffer pooling with size classes |
| Cross-browser WGSL differences | Works in Chrome, breaks in Safari | Test early in Safari; stick to WGSL core spec |
| Cholesky on GPU for small matrices | Slower than CPU for d < ~20 | Run linalg on CPU, only use GPU for element-wise/matmul |
| Scalar readback latency (NUTS) | ~1-3ms per mapAsync | `p/loop` handles cleanly; one read per tree is acceptable |
| Dawn Node.js gaps | No Linux ARM64 or Windows ARM64 prebuilt binary | Falls back to CPU inference on those platforms |

## Effort Estimate

| Scope | Lines | Time |
|---|---|---|
| Core tensor engine (no linalg, no MVN) | ~1500 | 4-6 weeks |
| Add Cholesky/triangular solve for MVN | +500 | +2 weeks |
| Robust cross-browser + nbb testing | — | +2-3 weeks |

## Implementation Phases

1. **Spike** (half day): Dispatch a WGSL compute shader from both nbb and a browser page. Element-wise multiply, read back result via `p/let`. Validates the toolchain on both runtimes.

2. **Core tensor ops** (2 weeks): Element-wise arithmetic/math, reductions, matmul, RNG. Environment detection (browser vs Node.js). Buffer pooling and pipeline caching. Test against known values in both runtimes.

3. **Autograd** (2 weeks): Tape-based reverse-mode AD. Backward rules for all forward ops. Verify gradients with finite differences against CPU computation.

4. **HMC** (1 week): Leapfrog integration, GPU-side accept/reject via `where`, `p/loop` for the sampling loop. Test on 1D/2D Gaussians, Bayesian linear regression. Verify identical results in nbb and browser.

5. **NUTS + VI** (1 week): Port from MLX versions. NUTS uses `p/loop` with per-iteration readback for U-turn check. VI uses `p/loop` for the Adam optimization loop.

6. **Scittle plugin + demo** (1 week): Wire into browser. Demo page with interactive Bayesian model. nbb example scripts.

### What we'd defer

- Multiple dtypes (float32 is sufficient for probabilistic programming)
- `vmap` (broadcasting suffices)
- Linalg beyond matmul (add Cholesky when multivariate normal is needed)
- Kernel fusion (record + replay model evaluation — optimization for later)
- Deno / Bun support (WebGPU exists in both but is less mature)

## Phase 0 Spike Results (2026-02-21)

Validated the full WebGPU toolchain from ClojureScript via nbb.

### nbb (Dawn via `webgpu` npm package)

All 5 tests pass:

| Test | Result |
|---|---|
| WebGPU init (`device.cljs` env detection + `init!`) | PASS |
| Unary shader (double `[1 2 3 4]` → `[2 4 6 8]`) | PASS |
| Binary shader (add `[1 2 3] + [10 20 30]` → `[11 22 33]`) | PASS |
| Readback latency (100x single f32 `mapAsync`) | PASS |
| Large array (1024 elements doubled) | PASS |

### Readback latency

| Metric | Value |
|---|---|
| Mean | 0.20 ms |
| Min | 0.18 ms |
| Max | 0.28 ms |

Well under the 3ms threshold. GPU-side accept/reject is still valuable for eliminating cumulative overhead across thousands of HMC iterations, but individual readback latency is not a bottleneck. NUTS per-iteration readback (~0.2ms) is negligible.

### Browser (Safari iPadOS 26.3, iPad Pro M4)

All 5 tests pass via Scittle + Promesa:

| Test | Result |
|---|---|
| WebGPU init (via `navigator.gpu`) | PASS |
| Unary shader (double) | PASS |
| Binary shader (add) | PASS |
| Readback latency (100x single f32) | PASS — mean=0.33ms, min=0.00ms, max=1.00ms |
| Large array (1024 elements) | PASS |

**Secure context required:** WebGPU requires HTTPS (or localhost). Serving over plain HTTP on a LAN IP hides `navigator.gpu`. This is a W3C spec requirement, not Safari-specific. For development, use `npx http-server -S` with a self-signed cert.

### SCI/Scittle compatibility

SCI's `exists?` does **not** work for checking `js/navigator.gpu` — it checks for var bindings, not JS property chains. Use `(.-gpu js/navigator)` or pass detection results from plain JS via `window.__webgpu_available`. The `device.cljs` module's browser path uses `(.-gpu js/navigator)` for the property check.

All other constructs work in SCI: `defonce`, `volatile!`, `vreset!`, `if-let`, `p/let`, `p/resolved`, `p/loop`, `p/recur`, `ex-info`, `bit-or`, `.createBuffer`, `.createComputePipeline`, `.mapAsync`, `js/Float32Array`, `js/GPUBufferUsage.STORAGE`, `clj->js`.

### Confidence level

**High confidence to proceed to Phase 1.** The toolchain works cleanly. WGSL shaders compile, dispatch correctly, and readback is fast. Promesa `p/let` and `p/loop`/`p/recur` handle all async naturally.
