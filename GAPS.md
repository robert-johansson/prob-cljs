# prob-cljs Gap Analysis

Systematic comparison of prob-cljs against [webchurch](https://github.com/probmods/webchurch), [WebPPL](https://github.com/probmods/webppl), and [Anglican](https://probprog.github.io/anglican/) to catalog missing features and plan the path to a research-grade probabilistic programming library.

**webchurch** is a trace-based PPL — relatively simple, with single-site MH and trace-based enumeration. The predecessor to WebPPL.
**WebPPL** is webchurch's successor — a compiler-based PPL with CPS transforms, 31 distributions, 10 inference algorithms, variational inference, and tensor/neural-net support. Used by ProbMods v2.
**Anglican** is a research-grade Clojure PPL with 15+ inference algorithms, CPS compilation, particle methods, and variational inference.

---

## ERPs / Distributions

### Existing distributions

| Distribution | webchurch | WebPPL | Anglican | prob-cljs | Status |
|---|---|---|---|---|---|
| flip (Bernoulli) | sampling + scoring + proposal + enumerable | `Bernoulli({p})` | full dist object | full: sample, observe, enumerate, drift | **Done** |
| gaussian | sampling + scoring + drift proposal | `Gaussian({mu, sigma})` + drift + reparam | full dist + gradient | full: sample, observe, drift proposal | **Done** |
| uniform (continuous) | sampling + scoring | `Uniform({a, b})` | full dist | full: sample, observe | **Done** |
| uniform-draw | sampling + scoring + enumerable | `Categorical` (equivalent) | — (use categorical) | full: sample, observe, enumerate | **Done** |
| multinomial | sampling + scoring + proposal + enumerable | `Multinomial({ps, n})` | — (use categorical) | full: sample, observe, enumerate | **Done** |
| beta | sampling + scoring | `Beta({a, b})` + reparam | full dist + gradient | full: sample, observe, drift proposal | **Done** |
| gamma | sampling + scoring | `Gamma({shape, scale})` | full dist + gradient | full: sample, observe | **Done** |
| dirichlet | sampling + scoring | `Dirichlet({alpha})` | full dist + gradient | full: sample, observe | **Done** |
| exponential | sampling + scoring | `Exponential({a})` | full dist + gradient | full: sample, observe | **Done** |
| random-integer | sampling + scoring + enumerable | `RandomInteger({n})` | — (use uniform-discrete) | full: sample, observe, enumerate | **Done** |
| sample-discrete | sampling + scoring | `Discrete({ps})` | — (use discrete/categorical) | full: sample, observe, enumerate | **Done** |
| binomial | full ERP with scoring | `Binomial({p, n})` | full dist | full: sample, observe, enumerate | **Done** |
| poisson | full ERP with scoring | `Poisson({mu})` | full dist | full: sample, observe (not enumerable) | **Done** |
| categorical | — | `Categorical({ps, vs})` | labeled weighted discrete | full: sample, observe, enumerate | **Done** |

### Missing distributions

| Distribution | webchurch | WebPPL | Anglican | prob-cljs | Priority |
|---|---|---|---|---|---|
| **delta** | — | `Delta({v})` | — | not implemented | High (trivial; useful for MAP, mixture components) |
| **cauchy** | — | `Cauchy({location, scale})` | — | not implemented | High (easy: inverse-transform sampling) |
| **laplace** | — | `Laplace({location, scale})` | `(laplace loc scale)` | not implemented | High (easy: inverse-transform sampling) |
| **log-normal** | — | `LogNormal({mu, sigma})` | — | not implemented | High (easy: `exp(gaussian(mu, sigma))`) |
| **student-t** | — | `StudentT({df, location, scale})` | `(student-t nu)` + location-scale | not implemented | High (medium: ratio of Gaussian and Gamma) |
| **mixture** | — | `Mixture({dists, ps})` | — | not implemented | High (medium: weighted combination of dists) |
| **uniform-discrete** | — | — | `(uniform-discrete min max)` | not implemented | Medium |
| **logit-normal** | — | `LogitNormal({mu, sigma, a, b})` | — | not implemented | Medium (sigmoid transform of Gaussian) |
| **KDE** | — | `KDE({data, width})` | — | not implemented | Medium (Gaussian kernel, Silverman bandwidth) |
| **chi-squared** | — | — | `(chi-squared nu)` | not implemented | Medium |
| **multivariate-bernoulli** | — | `MultivariateBernoulli({ps})` | — | not implemented | Low (vector of independent Bernoulli) |
| **diag-cov-gaussian** | — | `DiagCovGaussian({mu, sigma})` | — | not implemented | Low (needs vector abstraction) |
| **multivariate normal** | — | `MultivariateGaussian({mu, cov})` | `(mvn mean cov)` with Cholesky | not implemented | Low (needs linear algebra) |
| **logistic-normal** | — | `LogisticNormal({mu, sigma})` | — | not implemented | Low (softmax of multivariate Gaussian) |
| **multivariate-t** | — | — | `(multivariate-t nu mu sigma)` | not implemented | Low (needs linear algebra) |
| **wishart** | — | — | `(wishart n V)` with Bartlett decomp | not implemented | Low (needs linear algebra) |

### Distribution protocol

prob-cljs implements a full distribution protocol matching Anglican's approach:

- `IDistribution`: `sample*` (forward sampling) + `observe*` (log-probability scoring)
- `IEnumerable`: `enumerate*` returns vector of support values for discrete distributions
- `IProposable`: `propose*` returns `[new-value fwd-lp rev-lp]` for drift proposals in MH

All existing distributions implement `IDistribution`. Discrete distributions implement `IEnumerable`. Gaussian and Beta implement `IProposable` with continuous drift kernels (Gaussian: symmetric random walk; Beta: kappa=10 reparameterized proposal). Bernoulli, Multinomial, Categorical, UniformDraw, and RandomInteger implement `IProposable` with smart discrete proposals (deterministic toggle for Bernoulli, exclude-current-value for others). `Marginal` record wraps inner inference output as a full distribution (`sample*`, `observe*`, `enumerate*`).

### Distribution utilities (missing)

| Capability | WebPPL | Anglican | prob-cljs | Priority |
|---|---|---|---|---|
| `entropy` on discrete dists | Yes | — | not implemented | Medium (easy: `-sum(p * log(p))`) |
| `isContinuous` / `isDiscrete` flag | Yes | Via type check | not implemented | Low |
| Distribution serialization (JSON round-trip) | Yes | — | not implemented | Low |
| Reparameterization (`base`/`transform`) | Yes (for variational inference) | — | not implemented | Low (only useful with VI) |

---

## Architecture: CPS / Checkpoint Interrupts

Present in both **WebPPL** and **Anglican**. Not present in webchurch or prob-cljs.

WebPPL compiles `.wppl` programs through a pipeline of AST transforms: **CPS** (continuation-passing style), **naming** (address assignment per call site), **store** (threading global state), and **trampoline** (stack overflow prevention). When execution hits `sample` or `factor`, it yields control to the inference coroutine, which decides how to proceed.

Anglican uses the same approach at macro-expansion time: CPS transformation that returns interrupt records at `sample`/`observe` points, with the inference algorithm inspecting and resuming continuations.

This is fundamentally more powerful than:
- webchurch's trace-replay approach (re-run entire program with changed values)
- prob-cljs's trace-replay approach (same as webchurch, with persistent data structures)

**Why it matters:** CPS interrupts enable SMC (particles pause at observe points for resampling), variational inference (proposals learned per-checkpoint), IncrementalMH (re-execute from a specific checkpoint, not from scratch), and HMC (need AD through the program).

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

| Capability | webchurch | WebPPL | Anglican | prob-cljs | Status |
|---|---|---|---|---|---|
| Rejection sampling | Trace-based, uses logprob | Incremental rejection with `maxScore` bound | Supported | Exception-based rejection | **Done** |
| MH / MCMC | Real single-site trace MH | Single-site MH with drift kernels, burn-in, lag | LMH, RMH, ALMH | Real single-site trace MH with drift proposals | **Done** |
| MH scored | — | Via callbacks | Weighted samples | `mh-query-scored-fn` returns `{:value :score}` maps | **Done** |
| MAP | — | `onlyMAP` flag on MCMC | Simulated annealing | `map-query-fn` returns highest-scoring MH sample | **Done** |
| Exact enumeration | Trace-based, iterates all ERP combinations | Depth-first, breadth-first, likelyFirst (priority queue) | Not emphasized | Trace-based, odometer over all combinations | **Done** |
| Importance sampling | — | Built into rejection with weights | Weighted prior sampling | N runs with score normalization, aggregates duplicates | **Done** |
| `condition` | Sets flag on trace | `factor(bool ? 0 : -Inf)` | Via observe | Throws exception (rejection-style) | **Done** |
| `condition-equal` | Soft conditioning via enumerate | — | — | Enumerate thunk, factor log(P(return=value)) | **Done** |
| `factor` | Adds to trace logprob (exact) | Adds to trace logprob | Via observe log-weight | Exact in MH mode, probabilistic in rejection mode | **Done** |
| `observe` | — | Equivalent to factor(dist.score(val)) | Via observe log-weight | Delegates to factor via dist protocol | **Done** |
| Forward sampling | — | `method: 'forward'` (ignores factors) | — | not implemented | Easy |
| MCMC burn-in / thinning | — | `burn`, `lag` options on MCMC | — | not implemented | Easy |
| MCMC callbacks / hooks | — | `callbacks` array on MCMC | — | not implemented | Easy |
| Unified inference entry | — | `Infer(options, model)` dispatches to all methods | — | Separate macros per method | Medium |
| Incremental rejection | — | Reject at factor statements with `maxScore` bound | — | not implemented | Medium |

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

### Missing inference algorithms

| Algorithm | Source | Type | Description | Feasibility |
|---|---|---|---|---|
| **Forward sampling** | WebPPL | Sampling | Run model ignoring factors, collect prior samples | Easy (no new infrastructure) |
| **AIS** | WebPPL, Anglican | Annealing | Annealed Importance Sampling with temperature schedule | Medium (uses existing MH kernel) |
| **ALMH** | Anglican | MCMC | Adaptive LMH with UCB bandit scheduling for faster mixing | Medium |
| **Kernel composition** | WebPPL | MCMC | `sequence`, `repeat` combinators for MCMC kernels | Medium |
| **Enumeration strategies** | WebPPL | Exact | `likelyFirst` (priority queue), `breadthFirst`, time/tree-size limits | Medium |
| **SMC** | WebPPL, Anglican | Particle | Sequential Monte Carlo with systematic resampling | Hard (requires interruptible execution) |
| **IncrementalMH** | WebPPL | MCMC | C3 algorithm with adaptive caching, incremental re-execution | Hard (requires CPS + caching transforms) |
| **PMCMC / Particle Gibbs** | WebPPL, Anglican | PMCMC | Conditional SMC with retained particle | Hard (requires SMC) |
| **PGAS** | Anglican | PMCMC | Particle Gibbs with ancestor sampling | Hard (requires SMC) |
| **HMC** | WebPPL | MCMC | Hamiltonian Monte Carlo for continuous variables | Hard (requires automatic differentiation) |
| **Variational (ELBO)** | WebPPL, Anglican | Optimization | Guide programs with gradient-based ELBO optimization | Hard (requires AD + tensor ops) |
| **LARJ** | webchurch | MCMC | Locally Annealed Reversible Jump MCMC for trans-dimensional models | Hard |
| ~~**Marginalization**~~ | webchurch | Nested | ~~Nested inference as ERP via `marginalize()`~~ | **Done** via `marginal-dist` |

---

## Trace / Address System

| Capability | webchurch | WebPPL | Anglican | prob-cljs | Status |
|---|---|---|---|---|---|
| Execution traces | `RandomExecutionTrace` with variable database | CPS coroutine with trace object | CPS checkpoint records | Persistent hash-map trace, `volatile!` state | **Done** |
| Structural addresses | Call-site ID stack + loop counter | AST naming transform assigns unique address literals per call site | Stable gensym per sample site | Sequential integer counter per execution | **Done** (simpler scheme) |
| Trace replay | Re-execute program with changed variables | CPS continuation replay | CPS continuation replay | Re-execute with `make-replay-state`, old trace guides values | **Done** |
| Variable records | name, ERP, params, value, logprob, structural?, conditioned? | address, dist, value, score, guide | dist, value, log-prob, continuation | addr, value, log-prob, erp-id, fwd-lp, rev-lp | **Done** |

### Design: Scoped volatiles with persistent data

prob-cljs uses `volatile!` (not `atom`) for trace state — single-writer controlled mutation within inference boundaries. The trace itself is a persistent (immutable) hash-map, getting structural sharing benefits (~O(log32 n) per step). Volatiles never escape inference function boundaries; outside inference, ERPs are pure sampling functions.

---

## Random Processes (Stochastic Processes as First-Class Objects)

**Anglican-specific.** Not present in webchurch or prob-cljs. WebPPL has `DPmem` (via header) but no general process protocol.

Anglican has a `random-process` protocol with `produce` (get current distribution) and `absorb` (update with observation):

| Process | Description |
|---|---|
| **CRP** | Chinese Restaurant Process -- nonparametric clustering |
| **Dirichlet Process** | DP with arbitrary base measure |
| **Gaussian Process** | GP regression with CPS-compatible mean/kernel functions |
| **Dirichlet-discrete** | Conjugate Dirichlet-discrete process |
| **MVN-NIW** | Multivariate Normal with Normal-Inverse-Wishart conjugate prior |

prob-cljs has no random process abstraction (but `DPmem` covers the main nonparametric use case).

---

## Expressive Modeling Features

### Unified inference entry point

**WebPPL** has `Infer(options, model)` which dispatches to any inference method based on `options.method`. prob-cljs uses separate macros (`rejection-query`, `mh-query`, `enumeration-query`, `importance-query`) and corresponding `-fn` functions. A unified `infer` function dispatching on an options map would be more ergonomic.

### `predict` -- Labeled output collection

**Anglican-specific.** prob-cljs uses the return value of the query thunk as the sole output. WebPPL similarly uses the return value.

### `store`/`retrieve` -- Per-particle key-value store

**Anglican-specific.** Per-particle state accessible from within the probabilistic program.

### `mapData` -- Conditional independence

**WebPPL-specific.** `mapData({data: [...]}, fn)` annotates conditional independence, enabling data sub-sampling for scalable variational inference. Only useful if VI is implemented.

### `conditional` / `marginal-dist` -- Queries as distributions

Anglican wraps an entire query into a distribution object that can be `sample`d, enabling **nested inference**. WebPPL does the same via `Infer` returning a `Marginal` distribution. prob-cljs now has `marginal-dist` which wraps inner inference output as a full distribution object implementing `IDistribution` (`sample*`, `observe*`) and `IEnumerable` (`enumerate*`). Inner inference runs isolated from any outer trace. Additionally, `conditional-fn` creates a reusable sampler function from query parameters.

### `cache` -- LRU-cached deterministic functions

**WebPPL-specific.** `cache(fn, maxSize)` provides bounded deterministic memoization. Easy to add.

### Per-particle memoization

prob-cljs's `mem` is now **trace-aware**: during inference, the cache is stored in `:mem-cache` of the trace state (keyed by `[fn-id args]`), so MH can roll back memoized values on rejection. Outside inference, `mem` uses a closure-local `volatile!`. WebPPL's `mem` uses JSON serialization for cache keys; prob-cljs's approach is equivalent but uses structural equality.

---

## Memoization

| Capability | webchurch | WebPPL | Anglican | prob-cljs | Status |
|---|---|---|---|---|---|
| `mem` | Deterministic memoization | JSON-keyed memoization | Per-particle memoization | Trace-aware memoization (cache in trace state) | **Done** |
| `DPmem` | Dirichlet Process memoization (CRP) | `DPmem` in header (beta-bernoulli / dirichlet-discrete) | Via random process protocol | CRP table selection, trace-aware, returns single function | **Done** |
| `cache` (LRU) | — | `cache(fn, maxSize)` | — | not implemented | Easy |

---

## Gradient Infrastructure

Present in both **WebPPL** and **Anglican**. Not present in webchurch or prob-cljs.

**WebPPL** uses the `adnn` library for automatic differentiation, enabling HMC and variational inference (ELBO optimization with reparameterization gradients or score function estimator). Many continuous distributions implement `base()`/`transform()` for the reparameterization trick.

**Anglican's** `DistGradient` protocol provides `grad-log` and `grad-step` for 9 distribution types, enabling BBVB to automatically learn variational proposal distributions.

Required for: HMC, variational inference (BBVB/ELBO). Not needed for MCMC or SMC.

**Feasibility for prob-cljs:** Full AD in pure ClojureScript is a large effort. Simpler alternatives:
- Score function estimator (REINFORCE) for VI without reparameterization (high variance but feasible)
- Finite-difference gradients (slow but zero-dep)
- A ClojureScript AD library if one emerges

---

## Mathematical Special Functions

| Function | Description | Needed for | webchurch | WebPPL | Anglican | prob-cljs |
|---|---|---|---|---|---|---|
| `log-sum-exp` | Numerically stable log-space addition | Enumeration, importance sampling | — | Yes | Yes | **Yes** |
| `log-gamma-fn` | Log-Gamma function | Beta, Gamma, Dirichlet, Binomial log-probs | — | Yes | Yes | **Yes** |
| `digamma` | Digamma (psi) function | Gradient of Beta, Gamma, Dirichlet | — | Yes | Yes | **Yes** |
| `erf` | Error function | Gaussian CDF | — | — | — | **Yes** |
| `trigamma` | Trigamma function | Second derivative of log-Gamma | — | Yes | — | No |
| `log-mv-gamma-fn` | Multivariate log-Gamma | Wishart log-prob | — | — | Yes | No |
| `cholesky` | Cholesky decomposition | Multivariate Normal sampling + scoring | — | Via `adnn` | Yes | No |
| `inverse` / `det` | Matrix inverse, determinant | Wishart, MVN | — | Via `adnn` | Yes | No |

---

## Statistical Functions

| Function | WebPPL | Anglican | prob-cljs | Status |
|---|---|---|---|---|
| `mean` | Yes | Yes | Yes | **Done** |
| `variance` | Yes | Yes | Yes | **Done** |
| `weighted-mean` | — | `empirical-mean` | Yes | **Done** |
| `weighted-variance` | — | `empirical-variance` | Yes | **Done** |
| `expectation` | Yes | Yes | Yes | **Done** |
| `empirical-distribution` | — | Yes | Yes | **Done** |
| `sd` (standard deviation) | Yes | — | not implemented | Easy (trivial: `sqrt(variance)`) |
| `skew` | Yes | Yes | not implemented | Low priority |
| `kurtosis` | Yes | Yes | not implemented | Low priority |
| `mode` | Yes | — | not implemented | Low priority |
| `kde` (function) | Yes (Epanechnikov kernel) | — | not implemented | Medium |
| KL divergence | — | Yes | not implemented | Low priority |
| L2 distance | — | Yes | not implemented | Low priority |
| Kolmogorov-Smirnov distance | — | Yes | not implemented | Low priority |

---

## Visualization

| Chart Type | webchurch | WebPPL (`webppl-viz`) | prob-cljs | Status |
|---|---|---|---|---|
| Histogram | — | `viz.hist` with `numBins` | `hist` | **Done** |
| Density plot | — | `viz.density` with bounds | `density` | **Done** |
| Bar chart | — | `viz.bar` with grouped data | `barplot` | **Done** |
| Scatter plot | — | `viz.scatter` with groupBy | `scatter` | **Done** |
| Line chart | — | `viz.line` with groupBy, stroke | `lineplot` (single/multi-series) | **Done** |
| Table | — | `viz.table` with log/top options | `table` | **Done** |
| Auto-detect | — | `viz.auto` (analyzes data dimensionality, dispatches to best chart) | not implemented | Medium |
| Heat map | — | `viz.heatMap` (2D joint distributions) | not implemented | Medium |
| Marginals | — | `viz.marginals` (each marginal of joint dist separately) | not implemented | Medium |
| Parallel coordinates | — | `viz.parallelCoordinates` (high-dimensional data) | not implemented | Low priority |
| Display text | — | `display(val)` | `display` | **Done** |

WebPPL's `viz.auto` is notably sophisticated: it classifies variables as categorical (`c`), integer (`i`), or real (`r`), then dispatches to specialized renderers for combinations like `cc`, `cr`, `rr`, `ccc`, `ccr`, `crr`, etc.

WebPPL uses Vega/Vega-Lite (heavy npm dep). prob-cljs uses hand-rolled SVG/HTML (zero deps, browser-only).

---

## Builtins

| Category | webchurch | WebPPL | Anglican | prob-cljs | Status |
|---|---|---|---|---|---|
| List ops | Full suite | Full suite in `header.wppl` | Clojure standard | Full suite + ClojureScript standard lib | **Done** |
| Math | Full suite | Full suite | Clojure standard | Full suite | **Done** |
| Comparison | Full suite | JS standard | Clojure standard | Full suite | **Done** |
| String ops | Full suite | JS standard | Clojure standard | Full suite | **Done** |
| Set ops | union, intersection, difference | — (use lodash) | Clojure standard | Same | **Done** |
| `curry` / `uncurry` | Implemented | — | — | not implemented | Minor gap |
| `read-file` / `read-csv` / `write-csv` | Implemented | Via packages | — | not implemented | Minor (users have `js/require "fs"`) |
| `set-seed` | PRNG seeding | Via `seedrandom` | Via runtime config | `set-seed!` with xoshiro128** PRNG | **Done** |
| Visualization | — | Full Vega-based viz suite (`webppl-viz`) | — (external tools) | SVG-based: hist, density, scatter, barplot, lineplot, table | **Done** (browser only) |
| Tensor ops | — | Full via `adnn` tensors: `Vector`, `Matrix`, `T.add/mul/dot/transpose/softmax/...` | Via `core.matrix` | not implemented | Low (only needed for multivariate dists / VI) |
| Neural net layers | — | `affine`, `rnn`, `gru`, `lstm`, `sigmoid`, `relu`, `softmax`, `stack` | — | not implemented | Out of scope |

The effective gap for builtins is small because ClojureScript's standard library (`map`, `filter`, `reduce`, `sort`, `group-by`, etc.) covers most of what WebPPL reimplements in `header.wppl`.

---

## Extensibility

| Capability | webchurch | WebPPL | Anglican | prob-cljs |
|---|---|---|---|---|
| Package system | — | `--require pkg` with JS/wppl headers via `package.json` | Clojure deps | Scittle plugin system (`register-plugin!`) |
| Custom distributions | — | Extend `Distribution` prototype with `sample()`/`score()` | Implement protocol via `deftype` | Implement `IDistribution` protocol via `defrecord` |
| Custom inference | — | Write a coroutine (requires CPS internals) | Write inference backend | Write a `-fn` function following `mh-query-fn` pattern |
| Browser deployment | — | Browserify bundle | JVM only | Scittle CDN (no build step) |

---

## Priority Roadmap (remaining work)

### Phase 1: Easy Wins (pure ClojureScript, no new infrastructure)

1. ~~**Importance sampling**~~ -- **Done.** `importance-query-fn` / `importance-query` macro.

2. **MCMC burn-in and thinning** -- Add `burn` and `lag` options to `mh-query-fn`. WebPPL has these; they're standard MCMC practice.

3. **Forward sampling** -- Run model ignoring factors, collect prior samples. Useful for prior predictive checks.

4. **New easy distributions:**
   - **Delta** -- trivial: always returns `v`, score is `0` or `-Inf`. Useful for MAP, mixture components.
   - **Cauchy** -- inverse-transform: `location + scale * tan(pi * (u - 0.5))`
   - **Laplace** -- inverse-transform: `location - scale * sign(u - 0.5) * ln(1 - 2|u - 0.5|)`
   - **LogNormal** -- `exp(gaussian(mu, sigma))`, log-prob is Gaussian log-prob minus `x`
   - **StudentT** -- ratio of Gaussian and chi-squared (via Gamma)

5. **Mixture distribution** -- `(mixture-dist dists probs)`: weighted combination of component distributions. Sample by choosing component, then sampling from it. Score by `log-sum-exp` of component scores.

6. **Unified `infer` entry point** -- `(infer {:method :mh :samples 1000 :burn 100} model-fn)` dispatching to existing methods. More ergonomic than separate macros.

7. **Entropy on discrete distributions** -- `-sum(p * log(p))` on enumerable distributions.

### Phase 2: Better Inference (medium effort)

8. **Enumeration strategies** -- WebPPL's `likelyFirst` (priority queue using ClojureScript `sorted-set-by`), `breadthFirst`, execution count limits, time limits.

9. **AIS (Annealed Importance Sampling)** -- Temperature schedule from prior to posterior using existing MH kernel. Present in both WebPPL and Anglican.

10. **Incremental rejection** -- Reject at factor statements (not just condition). Requires `maxScore` bound for efficiency.

11. **Adaptive MH (ALMH)** -- UCB bandit scheduling for site selection, faster mixing.

12. **KDE distribution** -- Gaussian kernel, Silverman bandwidth rule. Useful for smoothing empirical distributions.

13. **MCMC kernel composition** -- `sequence`, `repeat` combinators for building compound kernels.

14. **Inference callbacks/hooks** -- Iteration, sample, and finish hooks for monitoring convergence.

### Phase 3: Interruptible Execution

15. **Interruptible execution** -- Options:
    - **(a) CPS macros** (Anglican/WebPPL approach) -- most powerful, most complex
    - **(b) Generator-based** -- use JS `function*`/`yield` if ClojureScript can interop

    Needed for SMC and particle methods. Both WebPPL and Anglican use CPS.

16. **SMC / Particle Filtering** -- Multiple particles, resample at observe points. Requires interruptible execution. Present in both WebPPL and Anglican.

17. **PMCMC / Particle Gibbs / PGAS** -- Iterated conditional SMC.

### Phase 4: Expressive Modeling

18. ~~**`DPmem`**~~ -- **Done.** Dirichlet Process memoization with CRP table selection, trace-aware for MH compatibility.

19. **Random processes** -- `produce`/`absorb` protocol for CRP, DP, GP (Anglican-inspired).

20. ~~**`conditional` / `marginal-dist`**~~ -- **Done.** `marginal-dist` wraps inner inference as a full distribution object. `condition-equal` provides soft conditioning. `mh-query-scored-fn` and `map-query-fn` provide scored samples and MAP inference.

21. ~~**PRNG seeding**~~ -- **Done.** `set-seed!` with xoshiro128** PRNG.

### Phase 5: Missing Distributions (harder)

22. **Medium distributions:**
    - Uniform-discrete, Chi-squared, LogitNormal

23. **Multivariate distributions** (requires linear algebra):
    - DiagCovGaussian, MultivariateGaussian, LogisticNormal, Wishart, multivariate-t
    - Needs: Cholesky decomposition, matrix inverse/determinant in pure ClojureScript

### Phase 6: Advanced Algorithms (hard, requires Phase 3)

24. **IncrementalMH** -- WebPPL's C3 algorithm with adaptive caching. Requires CPS + caching transforms.

25. **HMC** -- Hamiltonian Monte Carlo. Requires automatic differentiation.

26. **Variational inference (ELBO)** -- Guide programs with gradient-based optimization. Requires AD + tensor ops. WebPPL supports Adam, SGD, Adagrad, RMSprop optimizers.

27. **Gradient protocol** -- `grad-log` and `grad-step` for distributions. Required for VI.

### Phase 7: Ecosystem

28. ~~**Weighted sample utilities**~~ -- **Done.** `weighted-mean`, `weighted-variance`, `empirical-distribution`, `expectation`.

29. **Additional statistics** -- `sd`, `skew`, `kurtosis`, `mode`, KL divergence, L2/KS distances.

30. **Additional visualizations** -- `viz.auto` (smart chart selection), heat map, marginals display.

31. **Matrix operations** -- Cholesky, inverse, determinant (pure ClojureScript or optional dep).

---

## prob-cljs Advantages to Preserve

Anglican is powerful but heavy (Apache Commons Math, core.matrix, vectorz, claypoole, timbre). WebPPL requires a full compiler pipeline (esprima, escodegen, CPS/naming/store/trampoline/AD transforms), `adnn` for tensors/AD, and browserify for browser bundles. prob-cljs should stay:

- **Zero-dependency core** -- all distribution math in pure ClojureScript
- **nbb-native** -- no JVM, no compilation step, just `nbb -cp src script.cljs`
- **Browser-ready via Scittle** -- same code runs in `<script type="application/x-scittle">` with no build step (vs WebPPL's browserify)
- **JS-interop-first** -- easy to use with React, D3, Planck.js, Web APIs
- **Simple mental model** -- you're writing ClojureScript that happens to be probabilistic
- **Idiomatic Clojure** -- protocols, records, persistent data structures, standard library. No DSL strings, no AST transforms, no special syntax
