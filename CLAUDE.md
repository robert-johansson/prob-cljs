# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

prob-cljs is a probabilistic programming library for ClojureScript. It lets users write probabilistic inference programs as native ClojureScript code (no DSL strings or wrappers). It runs on:
- **nbb** (Node.js ClojureScript) for CLI/Node usage
- **Scittle** (browser SCI interpreter) for in-browser probabilistic programming

Zero external dependencies for the core library.

## Commands

### Run tests
```bash
nbb -cp src:test test/prob_tests.cljs
```

### Run demo
```bash
nbb -cp src examples/prob-demo.cljs
```

### Build Scittle plugin (browser)
```bash
cd scittle && npm install && npm run build
```

### Run Ink example
```bash
cd examples/ink-task-list && npm install && npx nbb -cp ../../src task-list.cljs
```

## Architecture

### Source layout (`src/prob/`)

- **core.cljs** — Public API. Thin re-export layer over erp, inference, and builtins.
- **erp.cljs** — Elementary Random Primitives (flip, gaussian, beta, gamma, dirichlet, binomial, poisson, categorical, etc.). Trace-aware sampling with log-probability scoring via the distribution protocol.
- **dist.cljs** — Distribution protocol (`IDistribution`: `sample*`/`observe*`), enumeration protocol (`IEnumerable`: `enumerate*`), drift proposal protocol (`IProposable`: `propose*`), and utility functions (`entropy`, `kl-divergence`, `discrete?`, `continuous?`). 24 distributions: Bernoulli, Gaussian, Uniform, Beta, Gamma, Exponential, Dirichlet, UniformDraw, RandomInteger, Multinomial, SampleDiscrete, Binomial, Poisson, Categorical, Delta, Cauchy, Laplace, LogNormal, StudentT, Mixture, KDE, UniformDiscrete, ChiSquared, LogitNormal. Plus `Marginal` record for nested inference.
- **math.cljs** — Mathematical special functions: `log-gamma-fn`, `log-sum-exp`, `digamma`, `erf`. Pure ClojureScript, zero deps.
- **inference.cljs** — Inference algorithms: rejection sampling, real single-site trace-based MH with drift proposals, exact enumeration (full + likelyFirst), forward sampling, importance sampling, MAP inference, scored MH, soft conditioning, and unified `infer` entry point. MCMC supports burn-in, thinning, and callbacks. Uses `volatile!` with persistent hash-map traces.
- **builtins.cljs** — Utility functions: Lisp-style list operations (pair/car/cdr), math, string ops, set operations, trace-aware memoization (`mem`), LRU cache (`cache`), Dirichlet Process memoization (`DPmem`), statistics (`sd`, `mean`, `variance`, `mode`), and type predicates.
- **macros.clj** — Clojure macros (`rejection-query`, `mh-query`, `mh-query-scored`, `map-query`, `enumeration-query`, `forward-query`, `query`) that wrap body in thunks and delegate to the `-fn` variants in inference.
- **sci.cljs** — SCI configuration for Scittle. Registers all prob namespaces with `sci/copy-var`. Defines SCI-compatible macros with `^:macro` metadata.

### Scittle plugin (`src/scittle/prob.cljs`)

Entry point that calls `scittle/register-plugin!` to make prob namespaces available in `<script type="application/x-scittle">` tags.

### Key design patterns

**Exception-based rejection**: `condition` throws with a `::rejection` sentinel. `rejection-query-fn` catches these and retries (up to 100,000 attempts). `factor` does probabilistic rejection in rejection mode, or exact score accumulation in MH mode. `observe` delegates to `factor` via the distribution protocol's `observe*`.

**Scoped volatiles**: Inference state uses `volatile!` (not `atom`) holding persistent hash-map traces. Volatiles are created inside inference functions and never escape. Outside inference, ERPs are pure sampling functions.

**Macro → function delegation**: Macros like `(rejection-query ...)` wrap body in `(fn [] ...)` and call `rejection-query-fn`. The `-fn` variants are what SCI and direct callers use.

**Three execution contexts**: nbb loads `.cljs` files directly; Scittle uses SCI via the sci.cljs config; the `docs/` directory inlines source directly in HTML for GitHub Pages.

### docs/

GitHub Pages deployment. `docs/index.html` has a standalone demo. `docs/prob/` contains copies of source files loaded via Scittle CDN. `docs/probmods/` contains ProbMods tutorial adaptations (Chapters 1-9).

## Known Limitations

See `GAPS.md` for a detailed comparison with webchurch and Anglican. Remaining gaps:
- No interruptible execution (CPS/generators) — blocks SMC/particle methods
- Missing distributions: multivariate normal (requires linear algebra)
- No random process abstraction (CRP, DP, GP as first-class objects; DPmem covers main use case)
- No gradient infrastructure (needed for HMC and variational inference)
