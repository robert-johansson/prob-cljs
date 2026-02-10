# prob-cljs Gap Analysis

Systematic comparison of prob-cljs against [webchurch](https://github.com/probmods/webchurch) and [Anglican](https://probprog.github.io/anglican/) to catalog missing features and plan the path to a research-grade probabilistic programming library.

**webchurch** is a trace-based PPL — relatively simple, with single-site MH and trace-based enumeration.
**Anglican** is a research-grade Clojure PPL with 15+ inference algorithms, CPS compilation, particle methods, and variational inference.

---

## ERPs / Distributions

### Existing distributions

| Distribution | webchurch | Anglican | prob-cljs | Gap |
|---|---|---|---|---|
| flip (Bernoulli) | sampling + scoring + proposal + enumerable | full dist object | sampling only | **Missing: scorer, proposal, enumeration** |
| gaussian | sampling + scoring + drift proposal | full dist + gradient | sampling only | **Missing: scorer, drift proposal** |
| uniform (continuous) | sampling + scoring | full dist | sampling only | **Missing: scorer** |
| uniform-draw | sampling + scoring + enumerable | — (use categorical) | sampling only | **Missing: scorer, enumeration** |
| multinomial | sampling + scoring + proposal + enumerable | — (use categorical) | sampling only | **Missing: scorer, proposal, enumeration** |
| beta | sampling + scoring | full dist + gradient | sampling only | **Missing: scorer** |
| gamma | sampling + scoring | full dist + gradient | sampling only | **Missing: scorer** |
| dirichlet | sampling + scoring | full dist + gradient | sampling only | **Missing: scorer** |
| exponential | sampling + scoring | full dist + gradient | sampling only | **Missing: scorer** |
| random-integer | sampling + scoring + enumerable | — (use uniform-discrete) | sampling only | **Missing: scorer, enumeration** |
| sample-discrete | sampling + scoring | — (use discrete/categorical) | sampling only | **Missing: scorer** |

### Missing distributions

| Distribution | webchurch | Anglican | prob-cljs | Priority |
|---|---|---|---|---|
| **binomial** | full ERP with scoring | full dist | not implemented | High |
| **poisson** | full ERP with scoring | full dist | not implemented | High |
| **categorical** | — | labeled weighted discrete | not implemented | High |
| **uniform-discrete** | — | `(uniform-discrete min max)` | not implemented | Medium |
| **chi-squared** | — | `(chi-squared nu)` | not implemented | Medium |
| **student-t** | — | `(student-t nu)` + location-scale | not implemented | Medium |
| **laplace** | — | `(laplace loc scale)` | not implemented | Medium |
| **multivariate normal** | — | `(mvn mean cov)` with Cholesky | not implemented | Low (needs linear algebra) |
| **multivariate-t** | — | `(multivariate-t nu mu sigma)` | not implemented | Low (needs linear algebra) |
| **wishart** | — | `(wishart n V)` with Bartlett decomp | not implemented | Low (needs linear algebra) |

### Key structural gap: Distribution protocol

In webchurch, every ERP is a `RandomPrimitive` object with:
- `sample_impl(params)` -- forward sampling
- `logprob(val, params)` -- log-probability scoring
- `proposal(currval, params)` -- MH proposal kernel (default: resample from prior)
- `logProposalProb(currval, propval, params)` -- log-probability of proposal
- `nextVal(currval, params)` -- for enumerable ERPs: iterate over domain

In Anglican, every distribution implements protocols:
- `sample*` -- forward sampling
- `observe*` -- log-probability scoring (equivalent to webchurch's `logprob`)
- Optionally: gradient support via `grad-log` and `grad-step`

In prob-cljs, ERPs are bare sampling functions with no scoring or proposal support. This is the single biggest gap -- it blocks real MH and real enumeration.

---

## Architecture: CPS / Checkpoint Interrupts

**Anglican-specific.** Not present in webchurch or GAPS.md's original scope.

Anglican's core innovation: every program is CPS-transformed at macro-expansion time. When execution hits `sample` or `observe`, it returns an **interrupt record** containing the distribution, the continuation, and the current state. The inference algorithm inspects this record, decides what value to use, and resumes by calling the continuation.

This is fundamentally more powerful than:
- webchurch's trace-replay approach (re-run entire program with changed values)
- prob-cljs's exception-based rejection (throw on condition failure)

**Why it matters:** CPS interrupts enable SMC (particles pause at observe points for resampling), variational inference (proposals learned per-checkpoint), and efficient MH (re-execute from a specific checkpoint, not from scratch).

**Options for prob-cljs:** Full CPS transformation is not the only path. JavaScript generators (`function*`/`yield`) or async/await could provide the same interrupt semantics more naturally in a JS-hosted environment. The key requirement is: execution must be **interruptible at sample/observe points** and **resumable with a chosen value**.

**Recommendation:** Start with trace-based replay (webchurch's approach, sufficient for single-site MH and enumeration), then add interruptible execution later for SMC and particle methods.

---

## `observe` as First-Class Primitive

**Anglican-specific.** Not present in webchurch (which uses `condition` + `factor`).

Anglican separates the concepts:
- `(sample dist)` -- draw from a distribution (inference algorithm may override)
- `(observe dist value)` -- condition on observing `value` from `dist`, adding `log-pdf(value | dist)` to the weight

prob-cljs only has `condition` (hard boolean) and `factor` (log-weight via probabilistic rejection). The `observe` primitive is strictly more powerful -- it's the standard way modern PPLs do soft conditioning.

Adding `observe` requires distributions to implement log-probability scoring (the distribution protocol from Phase 1).

---

## Inference

### Current state

| Capability | webchurch | Anglican | prob-cljs | Gap |
|---|---|---|---|---|
| Rejection sampling | Trace-based, uses logprob | Supported | Simple: run thunk, catch rejection exception | Works but no scoring |
| MH / MCMC | Real single-site trace MH | LMH, RMH, ALMH | Repeated rejection sampling (not actual MH) | **Not real MH** |
| Exact enumeration | Trace-based, iterates all ERP combinations | Not emphasized (CPS-based) | Approximate: 1000 rejection samples | **Not real enumeration** |
| `condition` | Sets flag on trace | Via observe | Throws exception (rejection-style) | Functional but different mechanism |
| `factor` | Adds to trace logprob (exact) | Via observe log-weight | Probabilistic rejection | **Approximate, not exact** |

### Anglican inference algorithms (all missing from prob-cljs)

| Algorithm | Type | Description |
|---|---|---|
| **Importance Sampling** | Basic | Sample from prior, weight by likelihood. Simplest weighted inference. |
| **LMH** | MCMC | Lightweight single-site MH with random database. The "real MH" that GAPS.md identifies as critical. |
| **RMH** | MCMC | Random-walk MH with distribution-specific kernel proposals. Much better mixing for continuous distributions. |
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

### How webchurch MH actually works

1. Initializes a trace (via rejection or enumeration-based initialization)
2. At each step, `RandomWalkKernel` selects a single random variable uniformly at random from all free (unconditioned) variables
3. Proposes a new value using the ERP's `proposal()` method
4. Re-executes the program with that change via `traceUpdate`
5. Applies standard MH acceptance: `acceptThresh = nextTrace.logprob - currTrace.logprob + rvsPropLP - fwdPropLP`

### How webchurch enumeration actually works

Not continuation-based. Trace-based:
1. Creates an initial trace where all enumerable ERPs are initialized to the first value in their domain
2. Iterates through all combinations using `nextEnumState()` (odometer-style counter)
3. For each complete assignment, re-executes via `traceUpdate()` to compute log-probability
4. Only works for programs with finitely enumerable random choices (flip, multinomial)
5. Uses Kahan summation for numerical stability

---

## Trace / Address System

| Capability | webchurch | Anglican | prob-cljs | Gap |
|---|---|---|---|---|
| Execution traces | `RandomExecutionTrace` with variable database | CPS checkpoint records | none | **Missing entirely** |
| Structural addresses | Call-site ID stack + loop counter | Stable gensym per sample site | none | **Missing entirely** |
| `traceUpdate` (replay) | Re-execute program with changed variables | CPS continuation replay | none | **Missing entirely** |
| Variable records | name, ERP, params, value, logprob, structural?, conditioned? | dist, value, log-prob, continuation | none | **Missing entirely** |

The trace system is the backbone of real MCMC. Each random choice gets a structural address from the call-site stack + loop counter. This allows MCMC to identify corresponding random choices across executions.

### Webchurch address construction

```
Call stack path (colon-separated call-site IDs) + "." + loop counter
Example: ":0:3:7.2" = third call at the stack path :0:3:7
```

### Webchurch variable record

```
name        -- address string
erp         -- the ERP object
params      -- parameters to the ERP
val         -- current value
logprob     -- log P(val | params)
active      -- touched during current traceUpdate?
structural  -- is this a structural choice?
conditioned -- is the value conditioned?
```

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

prob-cljs has no random process abstraction. GAPS.md previously mentioned `DPmem` (from webchurch) but not the general protocol.

---

## Expressive Modeling Features

### `predict` -- Labeled output collection

**Anglican-specific.** Anglican separates the program's return value from its **predictions**. `(predict :label value)` accumulates labeled outputs in the state. This allows:
- A single program run to produce multiple named outputs
- Inference algorithms to track which predictions change (used by ALMH for adaptive scheduling)
- Results processing to aggregate by label

prob-cljs uses the return value of the query thunk as the sole output.

### `store`/`retrieve` -- Per-particle key-value store

**Anglican-specific.** Allows programs to maintain state that is:
- Per-particle (each SMC particle has its own store)
- Accessible from within the probabilistic program
- Preserved across inference iterations (in MCMC)

### `conditional` -- Queries as distributions

Anglican wraps an entire query into a distribution object that can be `sample`d, enabling **nested inference** -- the posterior of one query becomes a distribution inside another. This is Anglican's version of webchurch's `marginalize`.

### Per-particle memoization

prob-cljs's `mem` uses a global atom. Anglican's `mem` stores the cache **in the particle state**, meaning:
- Each particle in SMC has independent memoization
- MCMC can roll back memoization along with other state
- Stochastic memoization (CRP-style) works correctly across particles

---

## Memoization

| Capability | webchurch | Anglican | prob-cljs | Gap |
|---|---|---|---|---|
| `mem` | Deterministic memoization | Per-particle memoization | Global atom memoization | **Per-particle needed for SMC/MCMC** |
| `DPmem` | Dirichlet Process memoization (CRP) | Via random process protocol | not implemented | **Missing** |

DPmem is needed for nonparametric Bayesian models (infinite mixture models, HDP, etc.). It maintains per-argument-set tables of labels and counts, sampling from a multinomial over existing labels plus a "new table" option weighted by concentration parameter alpha.

---

## Gradient Infrastructure

**Anglican-specific.** Not present in webchurch.

Anglican's `DistGradient` protocol provides `grad-log` and `grad-step` for 9 distribution types, enabling BBVB to automatically learn variational proposal distributions. All parameters are reparameterized to unconstrained space (log/logit transforms).

Required for variational inference (BBVB). Not needed for MCMC or SMC.

---

## Mathematical Special Functions

| Function | Description | Needed for | In prob-cljs? |
|---|---|---|---|
| `log-sum-exp` | Numerically stable log-space addition | Enumeration, importance sampling | No |
| `log-gamma-fn` | Log-Gamma function | Beta, Gamma, Dirichlet, Binomial log-probs | No |
| `digamma` | Digamma (psi) function | Gradient of Beta, Gamma, Dirichlet | No |
| `log-mv-gamma-fn` | Multivariate log-Gamma | Wishart log-prob | No |
| `erf` | Error function | Gaussian CDF | No |
| `cholesky` | Cholesky decomposition | Multivariate Normal sampling + scoring | No |
| `inverse` / `det` | Matrix inverse, determinant | Wishart, MVN | No |

The first three (`log-sum-exp`, `log-gamma-fn`, `digamma`) are needed for Phase 1 (distribution log-probabilities). Matrix operations are only needed for multivariate distributions (Phase 6+).

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

| Category | webchurch | Anglican | prob-cljs | Gap |
|---|---|---|---|---|
| List ops | Full suite | Clojure standard | Full suite | OK |
| Math | Full suite | Clojure standard | Full suite | OK |
| Comparison | Full suite | Clojure standard | Full suite | OK |
| String ops | Full suite | Clojure standard | Full suite | OK |
| Set ops | union, intersection, difference | Clojure standard | Same | OK |
| `curry` / `uncurry` | Implemented | — | not implemented | Minor gap |
| `read-file` / `read-csv` / `write-csv` | Implemented | — | not implemented | Minor (users have `js/require "fs"`) |
| `set-seed` | PRNG seeding | Via runtime config | not implemented | **Missing** |
| Visualization | Full Vega-based viz suite | — (external tools) | not implemented | Missing (different context) |

---

## Priority Roadmap

### Phase 1: Foundation (enables everything else)

1. **Distribution protocol** -- Replace bare sampling functions with protocol objects implementing `sample*` and `observe*` (log-probability). This is prerequisite for every algorithm.

2. **Log-probability implementations** -- For all existing distributions: flip, gaussian, uniform, beta, gamma, dirichlet, exponential, multinomial, uniform-draw, random-integer, sample-discrete.

3. **Mathematical special functions** -- `log-gamma-fn`, `log-sum-exp`, `digamma`, `erf`. Needed for log-probability computations. Pure ClojureScript, zero deps.

4. **`observe` primitive** -- Add `(observe dist value)` that adds `(observe* dist value)` to accumulated log-weight. Replaces probabilistic rejection in `factor`.

### Phase 2: Execution Control (the architectural leap)

5. **Interruptible execution** -- Options:
   - **(a) CPS macros** (Anglican's approach) -- most powerful, most complex
   - **(b) Tracing + replay** (webchurch's approach) -- simpler, sufficient for MH + enumeration
   - **(c) Generator-based** -- use JS `function*`/`yield` if ClojureScript can interop

   Recommendation: **(b) first, (a) later.** Tracing is sufficient for single-site MH and enumeration and is much simpler to implement in nbb/Scittle. CPS can come later for SMC.

6. **Execution trace** -- Record of `{address, dist, value, log-prob}` at each sample/observe point. Addresses from call-site stack + counter (like webchurch) or stable gensym (like Anglican).

7. **Trace replay (`traceUpdate`)** -- Re-execute program with a random database of previous values. At each sample point, check the RDB; if present, reuse value; otherwise, sample fresh.

### Phase 3: Real Inference Algorithms

8. **Importance sampling** -- Simplest weighted inference. Sample from prior, weight by observe log-probs. Foundation for everything else.

9. **Lightweight MH (single-site)** -- Select one random choice, resample from prior, replay, accept/reject via MH ratio. This is the "real MH" that the original gap analysis identifies as critical.

10. **Real enumeration** -- For discrete-only programs, iterate all combinations. Needs `enumerate` on discrete distributions (flip, categorical, discrete, uniform-draw).

11. **SMC / Particle Filtering** -- Multiple particles, resample at observe points. Requires interruptible execution (Phase 2). This is where CPS or generators become necessary.

12. **Random-walk MH** -- Distribution-specific local proposals instead of prior resampling. Much better mixing for continuous distributions.

### Phase 4: Missing Distributions

13. **New distributions with full sample*/observe*:**
    - Binomial, Poisson (high priority)
    - Categorical -- labeled weighted discrete (high priority)
    - Chi-squared, Student-t, Laplace (medium priority)
    - Uniform-discrete (medium priority)
    - Multivariate Normal (low priority -- requires Cholesky / linear algebra)

### Phase 5: Expressive Modeling

14. **Per-particle memoization** -- `mem` stores cache in inference state, not global atom.

15. **Random processes** -- `produce`/`absorb` protocol for CRP, DP, GP.

16. **`conditional`** -- Wrap query output as a distribution for nested inference.

17. **`predict`** -- Labeled output collection alongside return value.

18. **`store`/`retrieve`** -- Per-particle key-value store.

19. **PRNG seeding** -- Reproducible sampling.

### Phase 6: Advanced Algorithms

20. **Particle Gibbs** -- Iterated conditional SMC with retained particle.

21. **PGAS** -- Particle Gibbs with ancestor sampling.

22. **Adaptive MH (ALMH)** -- UCB bandit scheduling for faster mixing.

23. **BBVB** -- Black-box variational inference. Requires gradient infrastructure.

24. **Gradient protocol** -- `grad-log` and `grad-step` for distributions. Needed for BBVB.

### Phase 7: Ecosystem

25. **Weighted sample utilities** -- `empirical-distribution`, weighted mean/variance, KL divergence.

26. **Matrix operations** -- Cholesky, inverse, determinant (pure ClojureScript or optional dep).

27. **Multivariate distributions** -- MVN, Wishart, multivariate-t (require matrix ops).

---

## prob-cljs Advantages to Preserve

Anglican is powerful but heavy (Apache Commons Math, core.matrix, vectorz, claypoole, timbre). prob-cljs should stay:

- **Zero-dependency core** -- all distribution math in pure ClojureScript
- **nbb-native** -- no JVM, no compilation step, just `nbb -cp src script.cljs`
- **Browser-ready via Scittle** -- same code runs in `<script type="application/x-scittle">`
- **JS-interop-first** -- easy to use with React, D3, Planck.js, Web APIs
- **Simple mental model** -- you're writing ClojureScript that happens to be probabilistic
