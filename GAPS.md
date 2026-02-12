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

### Additional distributions

| Distribution | webchurch | WebPPL | Anglican | prob-cljs | Status |
|---|---|---|---|---|---|
| delta | — | `Delta({v})` | — | `delta-dist`: sample, observe, enumerate | **Done** |
| cauchy | — | `Cauchy({location, scale})` | — | `cauchy-dist`: sample, observe (inverse-transform) | **Done** |
| laplace | — | `Laplace({location, scale})` | `(laplace loc scale)` | `laplace-dist`: sample, observe (inverse-transform) | **Done** |
| log-normal | — | `LogNormal({mu, sigma})` | — | `lognormal-dist`: sample, observe (`exp(gaussian)`) | **Done** |
| student-t | — | `StudentT({df, location, scale})` | `(student-t nu)` + location-scale | `student-t-dist`: sample, observe (Gaussian/Gamma ratio) | **Done** |
| mixture | — | `Mixture({dists, ps})` | — | `mixture-dist`: sample, observe (`log-sum-exp` scoring) | **Done** |
| KDE | — | `KDE({data, width})` | — | `kde-dist`: Gaussian kernel, Silverman bandwidth, sample, observe | **Done** |

### Additional distributions (continued)

| Distribution | webchurch | WebPPL | Anglican | prob-cljs | Status |
|---|---|---|---|---|---|
| uniform-discrete | — | — | `(uniform-discrete min max)` | `uniform-discrete-dist`: sample, observe, enumerate, propose | **Done** |
| logit-normal | — | `LogitNormal({mu, sigma, a, b})` | — | `logit-normal-dist`: sigmoid(Gaussian), sample, observe | **Done** |
| chi-squared | — | — | `(chi-squared nu)` | `chi-squared-dist`: Gamma(df/2, 2) wrapper, sample, observe | **Done** |

### Missing distributions

| Distribution | webchurch | WebPPL | Anglican | prob-cljs | Priority |
|---|---|---|---|---|---|
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

### Distribution utilities

| Capability | WebPPL | Anglican | prob-cljs | Status |
|---|---|---|---|---|
| `entropy` on discrete dists | Yes | — | `(entropy dist)` computes `-sum(p * log(p))` | **Done** |
| `isContinuous` / `isDiscrete` flag | Yes | Via type check | `(discrete? d)` / `(continuous? d)` via protocol check | **Done** |
| KL divergence | — | — | Yes | `(kl-divergence p q)` for discrete distributions | **Done** |
| Distribution serialization (JSON round-trip) | Yes | — | not implemented | Low |
| Reparameterization (`base`/`transform`) | Yes (for variational inference) | — | not implemented | Low (only useful with VI) |

---

## Architecture: CPS / Checkpoint Interrupts

Present in **WebPPL**, **Anglican**, and now **prob-cljs**.

WebPPL compiles `.wppl` programs through a pipeline of AST transforms: **CPS** (continuation-passing style), **naming** (address assignment per call site), **store** (threading global state), and **trampoline** (stack overflow prevention). When execution hits `sample` or `factor`, it yields control to the inference coroutine, which decides how to proceed.

Anglican uses the same approach at macro-expansion time: CPS transformation that returns interrupt records at `sample`/`observe` points, with the inference algorithm inspecting and resuming continuations.

**prob-cljs** now implements CPS transformation via `cps_transform.cljc` (shared between Clojure macros and SCI). The `smc-query` macro CPS-transforms model code at macro expansion time. At each `sample`/`observe`/`factor`/`condition`, execution yields a checkpoint record (`Sample`, `Observe`, `Factor`, `Result`) containing the continuation. The SMC driver processes these checkpoints across all particles via a trampoline loop.

Supported CPS forms: `let`, `if`, `do`, `fn`, `when`, `when-not`, `cond`, `case`, `and`, `or`, `loop`/`recur`, `doseq` (single binding), all ERPs (auto-converted to `sample*` + dist constructor), vectors, maps, sets, quoted forms, and primitive function application. `doseq` is desugared to `loop`/`recur` at CPS transform time, enabling the natural `(doseq [d data] (observe ...))` pattern. All emitted symbols are fully qualified for SCI compatibility.

Trace-replay MH and enumeration continue to use the simpler re-execution approach (no CPS needed).

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
| Exact enumeration | Trace-based, iterates all ERP combinations | Depth-first, breadth-first, likelyFirst (priority queue) | Not emphasized | Trace-based: full odometer + likelyFirst priority queue | **Done** |
| Importance sampling | — | Built into rejection with weights | Weighted prior sampling | N runs with score normalization, aggregates duplicates | **Done** |
| `condition` | Sets flag on trace | `factor(bool ? 0 : -Inf)` | Via observe | Throws exception (rejection-style) | **Done** |
| `condition-equal` | Soft conditioning via enumerate | — | — | Enumerate thunk, factor log(P(return=value)) | **Done** |
| `factor` | Adds to trace logprob (exact) | Adds to trace logprob | Via observe log-weight | Exact in MH mode, probabilistic in rejection mode | **Done** |
| `observe` | — | Equivalent to factor(dist.score(val)) | Via observe log-weight | Delegates to factor via dist protocol | **Done** |
| Forward sampling | — | `method: 'forward'` (ignores factors) | — | `forward-query-fn`: condition/factor/observe become no-ops | **Done** |
| MCMC burn-in / thinning | — | `burn`, `lag` options on MCMC | — | `(mh-query-fn n lag burn thunk)` 4-arity | **Done** |
| MCMC callbacks / hooks | — | `callbacks` array on MCMC | — | Optional callback fn: `{:iter :value :score}` per kept sample | **Done** |
| SMC / Particle filtering | — | `method: 'SMC'` with particles, rejuvSteps | SMC, conditional SMC | `smc-query` macro with CPS transform, multinomial resampling, optional MH rejuvenation | **Done** |
| Particle Gibbs (PMCMC) | — | `method: 'PMCMC'` | Conditional SMC | `particle-gibbs-query` macro with retained particle, burn-in/lag/rejuvenation | **Done** |
| Unified inference entry | — | `Infer(options, model)` dispatches to all methods | — | `(infer {:method :mh :samples 1000 :burn 100} thunk)` | **Done** |
| Incremental rejection | — | Reject at factor statements with `maxScore` bound | — | not implemented | Medium |

### prob-cljs MH implementation

1. Initializes a trace via traced rejection sampling
2. Each step selects one random choice address uniformly at random
3. Checks if the distribution supports `IProposable` — if so, uses drift proposal; otherwise resamples from prior
4. Re-executes the program with trace replay (persistent hash-map for structural sharing)
5. MH acceptance: `log-accept = (new-score - old-score) + log(old-size/new-size) + proposal-correction`
6. For drift proposals, correction includes prior log-prob ratio + proposal asymmetry: `(new-lp - old-lp) + (rev-lp - fwd-lp)`

### prob-cljs enumeration implementation

1. **Discovery pass**: execute thunk once with `:enum-discovery true` to find all choice points, their domains, and per-value log-probs
2. **Strategies**: `:full` (exhaustive odometer) or `:likely-first` (priority queue exploring high-probability combinations first)
3. **Replay**: for each combo, build trace and replay via `make-replay-state`
4. **Collection**: `{:value return-val :log-prob (sum-choice-log-probs + score)}`; skip rejections
5. **Normalization**: `log-sum-exp` for numerical stability; aggregate duplicate return values
6. **Options**: `:max-executions` limits exploration for both strategies; `:strategy :likely-first` uses priority queue
7. **Guards**: error if >1M combos (full strategy without max-executions) or non-enumerable ERP encountered

### Missing inference algorithms

| Algorithm | Source | Type | Description | Feasibility |
|---|---|---|---|---|
| ~~**Forward sampling**~~ | WebPPL | Sampling | ~~Run model ignoring factors, collect prior samples~~ | **Done** via `forward-query-fn` |
| **AIS** | WebPPL, Anglican | Annealing | Annealed Importance Sampling with temperature schedule | Medium (uses existing MH kernel) |
| **ALMH** | Anglican | MCMC | Adaptive LMH with UCB bandit scheduling for faster mixing | Medium |
| **Kernel composition** | WebPPL | MCMC | `sequence`, `repeat` combinators for MCMC kernels | Medium |
| ~~**Enumeration strategies**~~ | WebPPL | Exact | ~~`likelyFirst` (priority queue), execution limits~~ | **Done**: `:likely-first` + `:max-executions` |
| ~~**SMC**~~ | WebPPL, Anglican | Particle | ~~Sequential Monte Carlo with systematic resampling~~ | **Done** via `smc-query` with CPS + multinomial resampling |
| **IncrementalMH** | WebPPL | MCMC | C3 algorithm with adaptive caching, incremental re-execution | Hard (requires CPS + caching transforms) |
| ~~**PMCMC / Particle Gibbs**~~ | WebPPL, Anglican | PMCMC | ~~Conditional SMC with retained particle~~ | **Done** via `particle-gibbs-query` with burn-in, lag, rejuvenation |
| **PGAS** | Anglican | PMCMC | Particle Gibbs with ancestor sampling | Medium (requires Particle Gibbs — now available) |
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

prob-cljs has both per-method macros (`rejection-query`, `mh-query`, `enumeration-query`, `importance-query`, `forward-query`) and a unified `infer` function:

```clojure
(infer {:method :mh :samples 1000 :lag 1 :burn 100 :callback my-fn} model-thunk)
```

Supports `:rejection`, `:mh`, `:enumeration`, `:importance`, `:forward`, `:mh-scored`, `:map`, `:smc`, and `:particle-gibbs` methods with method-specific options.

### `predict` -- Labeled output collection

**Anglican-specific.** prob-cljs uses the return value of the query thunk as the sole output. WebPPL similarly uses the return value.

### `store`/`retrieve` -- Per-particle key-value store

**Anglican-specific.** Per-particle state accessible from within the probabilistic program.

### `mapData` -- Conditional independence

**WebPPL-specific.** `mapData({data: [...]}, fn)` annotates conditional independence, enabling data sub-sampling for scalable variational inference. Only useful if VI is implemented.

### `conditional` / `marginal-dist` -- Queries as distributions

Anglican wraps an entire query into a distribution object that can be `sample`d, enabling **nested inference**. WebPPL does the same via `Infer` returning a `Marginal` distribution. prob-cljs now has `marginal-dist` which wraps inner inference output as a full distribution object implementing `IDistribution` (`sample*`, `observe*`) and `IEnumerable` (`enumerate*`). Inner inference runs isolated from any outer trace. Additionally, `conditional-fn` creates a reusable sampler function from query parameters.

### `cache` -- LRU-cached deterministic functions

prob-cljs has `(cache f)` and `(cache f max-size)` providing LRU-bounded deterministic memoization. Default max-size is 1000.

### Per-particle memoization

prob-cljs's `mem` is now **trace-aware**: during inference, the cache is stored in `:mem-cache` of the trace state (keyed by `[fn-id args]`), so MH can roll back memoized values on rejection. Outside inference, `mem` uses a closure-local `volatile!`. WebPPL's `mem` uses JSON serialization for cache keys; prob-cljs's approach is equivalent but uses structural equality.

---

## Memoization

| Capability | webchurch | WebPPL | Anglican | prob-cljs | Status |
|---|---|---|---|---|---|
| `mem` | Deterministic memoization | JSON-keyed memoization | Per-particle memoization | Trace-aware memoization (cache in trace state) | **Done** |
| `DPmem` | Dirichlet Process memoization (CRP) | `DPmem` in header (beta-bernoulli / dirichlet-discrete) | Via random process protocol | CRP table selection, trace-aware, returns single function | **Done** |
| `cache` (LRU) | — | `cache(fn, maxSize)` | — | `(cache f max-size)` with LRU eviction | **Done** |

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
| `sd` (standard deviation) | Yes | — | Yes | **Done** |
| `skew` | Yes | Yes | not implemented | Low priority |
| `kurtosis` | Yes | Yes | not implemented | Low priority |
| `mode` | Yes | — | Yes | **Done** |
| `kde` (function) | Yes (Epanechnikov kernel) | — | `kde-dist` (Gaussian kernel, Silverman bandwidth) | **Done** |
| KL divergence | — | Yes | `(kl-divergence p q)` for discrete dists | **Done** |
| `softmax` | Yes (in `adnn` tensors) | — | `(softmax utilities beta)` — Luce choice rule with inverse temperature | **Done** |
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
| Auto-detect | — | `viz.auto` (analyzes data dimensionality, dispatches to best chart) | `auto` classifies data and dispatches to appropriate chart | **Done** |
| Heat map | — | `viz.heatMap` (2D joint distributions) | `heatmap` with SVG grid and color scale | **Done** |
| Marginals | — | `viz.marginals` (each marginal of joint dist separately) | `marginals` displays per-variable hist/density | **Done** |
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

### Phase 1: Easy Wins — all **Done**

1. ~~**Importance sampling**~~ -- **Done.** `importance-query-fn` / `importance-query` macro.

2. ~~**MCMC burn-in and thinning**~~ -- **Done.** `(mh-query-fn n lag burn thunk)` 4-arity with burn-in support.

3. ~~**Forward sampling**~~ -- **Done.** `forward-query-fn` / `forward-query` macro. Binds `*forward-mode*` so condition/factor/observe are no-ops.

4. ~~**New easy distributions**~~ -- **Done.** Delta, Cauchy, Laplace, LogNormal, StudentT all implemented in `dist.cljs` with full sample/observe support.

5. ~~**Mixture distribution**~~ -- **Done.** `(mixture-dist dists probs)` with `log-sum-exp` scoring.

6. ~~**Unified `infer` entry point**~~ -- **Done.** `(infer {:method :mh :samples 1000 :burn 100 :callback fn} thunk)` dispatches to all 7 methods.

7. ~~**Entropy on discrete distributions**~~ -- **Done.** `(entropy dist)` computes `-sum(p * log(p))` on any IEnumerable distribution.

### Phase 2: Better Inference (medium effort)

8. ~~**Enumeration strategies**~~ -- **Done.** `:likely-first` strategy with priority-queue exploration of high-probability combinations first. `:max-executions` limits for both strategies.

9. **AIS (Annealed Importance Sampling)** -- Temperature schedule from prior to posterior using existing MH kernel. Present in both WebPPL and Anglican.

10. **Incremental rejection** -- Reject at factor statements (not just condition). Requires `maxScore` bound for efficiency.

11. **Adaptive MH (ALMH)** -- UCB bandit scheduling for site selection, faster mixing.

12. ~~**KDE distribution**~~ -- **Done.** `(kde-dist data)` with Gaussian kernel, Silverman bandwidth. Also `(kde-dist data bandwidth)` for manual bandwidth.

13. **MCMC kernel composition** -- `sequence`, `repeat` combinators for building compound kernels.

14. ~~**Inference callbacks/hooks**~~ -- **Done.** Optional callback fn on `mh-query-fn` and `mh-query-scored-fn`. Called with `{:iter :value :score}` for each kept sample.

### Phase 3: Interruptible Execution

15. ~~**Interruptible execution**~~ -- **Done.** CPS macro transformation (Anglican/WebPPL approach) implemented in `cps_transform.cljc`. Shared between Clojure macros (nbb) and SCI macros (Scittle). Checkpoint records (`Sample`, `Observe`, `Factor`, `Result`) with continuations, processed by trampoline.

16. ~~**SMC / Particle Filtering**~~ -- **Done.** `smc-query` macro CPS-transforms model body. SMC driver runs N particles, multinomial resampling at observe points (adaptive via ESS). Optional MH rejuvenation via `{:rejuv-steps n}`. Supports `let`, `if`, `do`, `fn`, `when`, `cond`, `case`, `and`, `or`, `loop`/`recur`, all ERPs, and primitive function calls.

17. ~~**PMCMC / Particle Gibbs**~~ -- **Done.** `particle-gibbs-query` macro with conditional SMC (retained reference particle). Supports burn-in, lag, MH rejuvenation, and callbacks. PGAS (ancestor sampling) not yet implemented.

### Phase 4: Expressive Modeling

18. ~~**`DPmem`**~~ -- **Done.** Dirichlet Process memoization with CRP table selection, trace-aware for MH compatibility.

19. **Random processes** -- `produce`/`absorb` protocol for CRP, DP, GP (Anglican-inspired).

20. ~~**`conditional` / `marginal-dist`**~~ -- **Done.** `marginal-dist` wraps inner inference as a full distribution object. `condition-equal` provides soft conditioning. `mh-query-scored-fn` and `map-query-fn` provide scored samples and MAP inference.

21. ~~**PRNG seeding**~~ -- **Done.** `set-seed!` with xoshiro128** PRNG.

### Phase 5: Missing Distributions (harder)

22. ~~**Medium distributions:**~~
    - ~~Uniform-discrete, Chi-squared, LogitNormal~~ -- **Done.** All three implemented in `dist.cljs`.

23. **Multivariate distributions** (requires linear algebra):
    - DiagCovGaussian, MultivariateGaussian, LogisticNormal, Wishart, multivariate-t
    - Needs: Cholesky decomposition, matrix inverse/determinant in pure ClojureScript

### Phase 6: Advanced Algorithms (hard)

24. **IncrementalMH** -- WebPPL's C3 algorithm with adaptive caching. Requires CPS + caching transforms.

25. **HMC** -- Hamiltonian Monte Carlo. Requires automatic differentiation.

26. **Variational inference (ELBO)** -- Guide programs with gradient-based optimization. Requires AD + tensor ops. WebPPL supports Adam, SGD, Adagrad, RMSprop optimizers.

27. **Gradient protocol** -- `grad-log` and `grad-step` for distributions. Required for VI.

### Phase 7: Ecosystem

28. ~~**Weighted sample utilities**~~ -- **Done.** `weighted-mean`, `weighted-variance`, `empirical-distribution`, `expectation`.

29. ~~**`sd` (standard deviation)**~~ -- **Done.** `(sd lst)` = `sqrt(variance)`.

30. ~~**`cache` (LRU memoization)**~~ -- **Done.** `(cache f)` or `(cache f max-size)` with LRU eviction.

31. **Additional statistics** -- `skew`, `kurtosis`, L2/KS distances. ~~`mode`~~: **Done.** ~~KL divergence~~: **Done.**

32. ~~**Additional visualizations**~~ -- ~~`viz.auto` (smart chart selection), heat map, marginals display.~~ **Done.** `auto`, `heatmap`, `marginals` in `viz.cljs`.

33. **Matrix operations** -- Cholesky, inverse, determinant (pure ClojureScript or optional dep).

---

## prob-cljs Advantages to Preserve

Anglican is powerful but heavy (Apache Commons Math, core.matrix, vectorz, claypoole, timbre). WebPPL requires a full compiler pipeline (esprima, escodegen, CPS/naming/store/trampoline/AD transforms), `adnn` for tensors/AD, and browserify for browser bundles. prob-cljs should stay:

- **Zero-dependency core** -- all distribution math in pure ClojureScript
- **nbb-native** -- no JVM, no compilation step, just `nbb -cp src script.cljs`
- **Browser-ready via Scittle** -- same code runs in `<script type="application/x-scittle">` with no build step (vs WebPPL's browserify)
- **JS-interop-first** -- easy to use with React, D3, Planck.js, Web APIs
- **Simple mental model** -- you're writing ClojureScript that happens to be probabilistic
- **Idiomatic Clojure** -- protocols, records, persistent data structures, standard library. No DSL strings, no special syntax. CPS transform is transparent (only inside `smc-query`; all other inference uses normal ClojureScript)
