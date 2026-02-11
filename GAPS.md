# prob-cljs Gap Analysis

Systematic comparison of prob-cljs against [webchurch](https://github.com/probmods/webchurch) and [Anglican](https://probprog.github.io/anglican/) to catalog missing features and plan the path to a research-grade probabilistic programming library.

**webchurch** is a trace-based PPL — relatively simple, with single-site MH and trace-based enumeration.
**Anglican** is a research-grade Clojure PPL with 15+ inference algorithms, CPS compilation, particle methods, and variational inference.

---

## ERPs / Distributions

### Existing distributions

| Distribution | webchurch | Anglican | prob-cljs | Status |
|---|---|---|---|---|
| flip (Bernoulli) | sampling + scoring + proposal + enumerable | full dist object | full: sample, observe, enumerate, drift | **Done** |
| gaussian | sampling + scoring + drift proposal | full dist + gradient | full: sample, observe, drift proposal | **Done** |
| uniform (continuous) | sampling + scoring | full dist | full: sample, observe | **Done** |
| uniform-draw | sampling + scoring + enumerable | — (use categorical) | full: sample, observe, enumerate | **Done** |
| multinomial | sampling + scoring + proposal + enumerable | — (use categorical) | full: sample, observe, enumerate | **Done** |
| beta | sampling + scoring | full dist + gradient | full: sample, observe, drift proposal | **Done** |
| gamma | sampling + scoring | full dist + gradient | full: sample, observe | **Done** |
| dirichlet | sampling + scoring | full dist + gradient | full: sample, observe | **Done** |
| exponential | sampling + scoring | full dist + gradient | full: sample, observe | **Done** |
| random-integer | sampling + scoring + enumerable | — (use uniform-discrete) | full: sample, observe, enumerate | **Done** |
| sample-discrete | sampling + scoring | — (use discrete/categorical) | full: sample, observe, enumerate | **Done** |
| binomial | full ERP with scoring | full dist | full: sample, observe, enumerate | **Done** |
| poisson | full ERP with scoring | full dist | full: sample, observe (not enumerable) | **Done** |
| categorical | — | labeled weighted discrete | full: sample, observe, enumerate | **Done** |

### Missing distributions

| Distribution | webchurch | Anglican | prob-cljs | Priority |
|---|---|---|---|---|
| **uniform-discrete** | — | `(uniform-discrete min max)` | not implemented | Medium |
| **chi-squared** | — | `(chi-squared nu)` | not implemented | Medium |
| **student-t** | — | `(student-t nu)` + location-scale | not implemented | Medium |
| **laplace** | — | `(laplace loc scale)` | not implemented | Medium |
| **multivariate normal** | — | `(mvn mean cov)` with Cholesky | not implemented | Low (needs linear algebra) |
| **multivariate-t** | — | `(multivariate-t nu mu sigma)` | not implemented | Low (needs linear algebra) |
| **wishart** | — | `(wishart n V)` with Bartlett decomp | not implemented | Low (needs linear algebra) |

### Distribution protocol

prob-cljs implements a full distribution protocol matching Anglican's approach:

- `IDistribution`: `sample*` (forward sampling) + `observe*` (log-probability scoring)
- `IEnumerable`: `enumerate*` returns vector of support values for discrete distributions
- `IProposable`: `propose*` returns `[new-value fwd-lp rev-lp]` for drift proposals in MH

All existing distributions implement `IDistribution`. Discrete distributions implement `IEnumerable`. Gaussian and Beta implement `IProposable` with drift kernels (Gaussian: symmetric random walk; Beta: kappa=10 reparameterized proposal).

---

## Architecture: CPS / Checkpoint Interrupts

**Anglican-specific.** Not present in webchurch or prob-cljs.

Anglican's core innovation: every program is CPS-transformed at macro-expansion time. When execution hits `sample` or `observe`, it returns an **interrupt record** containing the distribution, the continuation, and the current state. The inference algorithm inspects this record, decides what value to use, and resumes by calling the continuation.

This is fundamentally more powerful than:
- webchurch's trace-replay approach (re-run entire program with changed values)
- prob-cljs's trace-replay approach (same as webchurch, with persistent data structures)

**Why it matters:** CPS interrupts enable SMC (particles pause at observe points for resampling), variational inference (proposals learned per-checkpoint), and efficient MH (re-execute from a specific checkpoint, not from scratch).

**Options for prob-cljs:** Full CPS transformation is not the only path. JavaScript generators (`function*`/`yield`) or async/await could provide the same interrupt semantics more naturally in a JS-hosted environment. The key requirement is: execution must be **interruptible at sample/observe points** and **resumable with a chosen value**.

---

## `observe` as First-Class Primitive

prob-cljs now has `observe`:

```clojure
(observe dist value)  ;; adds (observe* dist value) to accumulated log-weight via factor
```

This works in both rejection mode (probabilistic rejection) and MH mode (exact score accumulation).

---

## Inference

### Current state

| Capability | webchurch | Anglican | prob-cljs | Status |
|---|---|---|---|---|
| Rejection sampling | Trace-based, uses logprob | Supported | Exception-based rejection | **Done** |
| MH / MCMC | Real single-site trace MH | LMH, RMH, ALMH | Real single-site trace MH with drift proposals | **Done** |
| Exact enumeration | Trace-based, iterates all ERP combinations | Not emphasized | Trace-based, odometer over all combinations | **Done** |
| `condition` | Sets flag on trace | Via observe | Throws exception (rejection-style) | **Done** |
| `factor` | Adds to trace logprob (exact) | Via observe log-weight | Exact in MH mode, probabilistic in rejection mode | **Done** |
| `observe` | — | Via observe log-weight | Delegates to factor via dist protocol | **Done** |

### prob-cljs MH implementation

1. Initializes a trace via traced rejection sampling
2. Each step selects one random choice address uniformly at random
3. Checks if the distribution supports `IProposable` — if so, uses drift proposal; otherwise resamples from prior
4. Re-executes the program with trace replay (persistent hash-map for structural sharing)
5. MH acceptance: `log-accept = (new-score - old-score) + log(old-size/new-size) + proposal-correction`
6. For drift proposals, correction includes prior log-prob ratio + proposal asymmetry: `(new-lp - old-lp) + (rev-lp - fwd-lp)`

### prob-cljs enumeration implementation

1. **Discovery pass**: execute thunk once with `:enum-discovery true` to find all choice points and their domains
2. **Iteration**: odometer over all combinations (product of domain sizes)
3. **Replay**: for each combo, build trace and replay via `make-replay-state`
4. **Collection**: `{:value return-val :log-prob (sum-choice-log-probs + score)}`; skip rejections
5. **Normalization**: `log-sum-exp` for numerical stability; aggregate duplicate return values
6. **Guards**: error if >1M combos or non-enumerable ERP encountered

### Anglican inference algorithms (all missing from prob-cljs)

| Algorithm | Type | Description |
|---|---|---|
| **Importance Sampling** | Basic | Sample from prior, weight by likelihood. Simplest weighted inference. |
| **ALMH** | MCMC | Adaptive LMH with UCB bandit scheduling for faster mixing. |
| **SMC** | Particle | Sequential Monte Carlo with systematic resampling. Requires interruptible execution. |
| **Particle Gibbs** | PMCMC | Iterated conditional SMC with retained particle. |
| **PGAS** | PMCMC | Particle Gibbs with ancestor sampling. |
| **PIMH** | PMCMC | Particle Independent MH (sweep-level acceptance). |
| **IPMCMC** | PMCMC | Interacting Particle MCMC. |
| **Particle Cascade** | Adaptive | Thread-parallel dynamic particle allocation. |
| **BBVB** | Variational | Black-Box Variational Bayes with gradient learning. Requires gradient infrastructure. |
| **Simulated Annealing** | Optimization | MAP estimation via annealing. |
| **BAMC** | Adaptive | Bayesian Adaptive Monte Carlo. |
| **AIS** | Annealing | Annealed Importance Sampling. |

### webchurch-specific algorithms (also missing)

| Algorithm | Description |
|---|---|
| **LARJ** | Locally Annealed Reversible Jump MCMC for trans-dimensional models |
| **Suwa-Todo** | Irreversible MCMC kernel for discrete variables |
| **Marginalization** | Nested inference as ERP via `marginalize()` |

---

## Trace / Address System

| Capability | webchurch | Anglican | prob-cljs | Status |
|---|---|---|---|---|
| Execution traces | `RandomExecutionTrace` with variable database | CPS checkpoint records | Persistent hash-map trace, `volatile!` state | **Done** |
| Structural addresses | Call-site ID stack + loop counter | Stable gensym per sample site | Sequential integer counter per execution | **Done** (simpler scheme) |
| Trace replay | Re-execute program with changed variables | CPS continuation replay | Re-execute with `make-replay-state`, old trace guides values | **Done** |
| Variable records | name, ERP, params, value, logprob, structural?, conditioned? | dist, value, log-prob, continuation | addr, value, log-prob, erp-id, fwd-lp, rev-lp | **Done** |

### Design: Scoped volatiles with persistent data

prob-cljs uses `volatile!` (not `atom`) for trace state — single-writer controlled mutation within inference boundaries. The trace itself is a persistent (immutable) hash-map, getting structural sharing benefits (~O(log32 n) per step). Volatiles never escape inference function boundaries; outside inference, ERPs are pure sampling functions.

---

## Random Processes (Stochastic Processes as First-Class Objects)

**Anglican-specific.** Not present in webchurch (except DPmem).

Anglican has a `random-process` protocol with `produce` (get current distribution) and `absorb` (update with observation):

| Process | Description |
|---|---|
| **CRP** | Chinese Restaurant Process -- nonparametric clustering |
| **Dirichlet Process** | DP with arbitrary base measure |
| **Gaussian Process** | GP regression with CPS-compatible mean/kernel functions |
| **Dirichlet-discrete** | Conjugate Dirichlet-discrete process |
| **MVN-NIW** | Multivariate Normal with Normal-Inverse-Wishart conjugate prior |

prob-cljs has no random process abstraction.

---

## Expressive Modeling Features

### `predict` -- Labeled output collection

**Anglican-specific.** prob-cljs uses the return value of the query thunk as the sole output.

### `store`/`retrieve` -- Per-particle key-value store

**Anglican-specific.** Per-particle state accessible from within the probabilistic program.

### `conditional` -- Queries as distributions

Anglican wraps an entire query into a distribution object that can be `sample`d, enabling **nested inference**. prob-cljs has `conditional-fn` which creates a reusable sampler from query parameters, but the result is a function, not a distribution object (no `observe*`).

### Per-particle memoization

prob-cljs's `mem` is now **trace-aware**: during inference, the cache is stored in `:mem-cache` of the trace state (keyed by `[fn-id args]`), so MH can roll back memoized values on rejection. Outside inference, `mem` uses a closure-local `volatile!`.

---

## Memoization

| Capability | webchurch | Anglican | prob-cljs | Status |
|---|---|---|---|---|
| `mem` | Deterministic memoization | Per-particle memoization | Trace-aware memoization (cache in trace state) | **Done** |
| `DPmem` | Dirichlet Process memoization (CRP) | Via random process protocol | not implemented | **Missing** |

---

## Gradient Infrastructure

**Anglican-specific.** Not present in webchurch or prob-cljs.

Anglican's `DistGradient` protocol provides `grad-log` and `grad-step` for 9 distribution types, enabling BBVB to automatically learn variational proposal distributions.

Required for variational inference (BBVB). Not needed for MCMC or SMC.

---

## Mathematical Special Functions

| Function | Description | Needed for | In prob-cljs? |
|---|---|---|---|
| `log-sum-exp` | Numerically stable log-space addition | Enumeration, importance sampling | **Yes** |
| `log-gamma-fn` | Log-Gamma function | Beta, Gamma, Dirichlet, Binomial log-probs | **Yes** |
| `digamma` | Digamma (psi) function | Gradient of Beta, Gamma, Dirichlet | **Yes** |
| `erf` | Error function | Gaussian CDF | **Yes** |
| `log-mv-gamma-fn` | Multivariate log-Gamma | Wishart log-prob | No |
| `cholesky` | Cholesky decomposition | Multivariate Normal sampling + scoring | No |
| `inverse` / `det` | Matrix inverse, determinant | Wishart, MVN | No |

---

## Weighted Sample Processing

**Anglican-specific.** Anglican's `stat.cljc` provides functions for working with weighted samples:
- `empirical-distribution` -- weighted samples to probability map
- `empirical-mean/variance/std/skew/kurtosis` -- weighted statistics
- `empirical-expectation` -- E[f(x)] under weighted samples
- KL divergence, L2 distance, Kolmogorov-Smirnov distance

prob-cljs has no weighted sample utilities. These become important once inference produces weighted (non-rejection) samples.

---

## Builtins

| Category | webchurch | Anglican | prob-cljs | Status |
|---|---|---|---|---|
| List ops | Full suite | Clojure standard | Full suite | **Done** |
| Math | Full suite | Clojure standard | Full suite | **Done** |
| Comparison | Full suite | Clojure standard | Full suite | **Done** |
| String ops | Full suite | Clojure standard | Full suite | **Done** |
| Set ops | union, intersection, difference | Clojure standard | Same | **Done** |
| `curry` / `uncurry` | Implemented | — | not implemented | Minor gap |
| `read-file` / `read-csv` / `write-csv` | Implemented | — | not implemented | Minor (users have `js/require "fs"`) |
| `set-seed` | PRNG seeding | Via runtime config | not implemented | **Missing** |
| Visualization | Full Vega-based viz suite | — (external tools) | SVG-based: hist, density, scatter, barplot | **Done** (browser only) |

---

## Priority Roadmap (remaining work)

### Phase 1: Better Inference

1. **Importance sampling** -- Sample from prior, weight by observe log-probs. Simplest weighted inference.

2. **SMC / Particle Filtering** -- Multiple particles, resample at observe points. Requires interruptible execution (CPS or generators).

3. **Adaptive MH (ALMH)** -- UCB bandit scheduling for faster mixing.

### Phase 2: Interruptible Execution

4. **Interruptible execution** -- Options:
   - **(a) CPS macros** (Anglican's approach) -- most powerful, most complex
   - **(b) Generator-based** -- use JS `function*`/`yield` if ClojureScript can interop

   Needed for SMC and particle methods.

### Phase 3: Missing Distributions

5. **New distributions:**
   - Uniform-discrete, Chi-squared, Student-t, Laplace (medium priority)
   - Multivariate Normal (low priority -- requires Cholesky / linear algebra)

### Phase 4: Expressive Modeling

6. **`DPmem`** -- Dirichlet Process memoization for nonparametric Bayesian models.

7. **Random processes** -- `produce`/`absorb` protocol for CRP, DP, GP.

8. **`conditional`** -- Wrap query output as a full distribution for nested inference with `observe*`.

9. **`predict`** -- Labeled output collection alongside return value.

10. **PRNG seeding** -- Reproducible sampling.

### Phase 5: Advanced Algorithms

11. **Particle Gibbs / PGAS** -- Iterated conditional SMC.

12. **BBVB** -- Black-box variational inference. Requires gradient infrastructure.

13. **Gradient protocol** -- `grad-log` and `grad-step` for distributions.

### Phase 6: Ecosystem

14. **Weighted sample utilities** -- `empirical-distribution`, weighted mean/variance, KL divergence.

15. **Matrix operations** -- Cholesky, inverse, determinant (pure ClojureScript or optional dep).

16. **Multivariate distributions** -- MVN, Wishart, multivariate-t (require matrix ops).

---

## prob-cljs Advantages to Preserve

Anglican is powerful but heavy (Apache Commons Math, core.matrix, vectorz, claypoole, timbre). prob-cljs should stay:

- **Zero-dependency core** -- all distribution math in pure ClojureScript
- **nbb-native** -- no JVM, no compilation step, just `nbb -cp src script.cljs`
- **Browser-ready via Scittle** -- same code runs in `<script type="application/x-scittle">`
- **JS-interop-first** -- easy to use with React, D3, Planck.js, Web APIs
- **Simple mental model** -- you're writing ClojureScript that happens to be probabilistic
