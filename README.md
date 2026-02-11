# prob-cljs

Probabilistic programming as a ClojureScript library. Zero dependencies.

Write normal ClojureScript that uses `flip`, `gaussian`, `mh-query`, `enumeration-query`, etc. alongside Clojure's `let`/`defn`/`map` and full JS interop. No DSL strings, no wrappers, no compilation step.

Runs on:
- **[nbb](https://github.com/babashka/nbb)** — Node.js ClojureScript for CLI/scripting
- **[Scittle](https://github.com/babashka/scittle)** — browser-based ClojureScript via SCI (no build step)

## Quick Start

```bash
# Requires nbb: npm install -g nbb

# Run the demo
nbb -cp src examples/prob-demo.cljs

# Run tests (215 tests)
nbb -cp src:test test/prob_tests.cljs
```

## Usage

```clojure
(ns my-analysis
  (:require [prob.core :refer [flip gaussian beta uniform condition factor
                                observe mean variance sd mem DPmem
                                bernoulli-dist gaussian-dist sample* observe*
                                infer]])
  (:require-macros [prob.macros :refer [rejection-query mh-query enumeration-query
                                         forward-query]]))

;; Coin flip with conditioning
(rejection-query
  (let [x (flip)]
    (condition x)
    x))
;=> true

;; Estimate bias from observations
(let [data [true true true false true]
      samples (mh-query 1000 1
                (let [bias (beta 1 1)]
                  (doseq [obs data]
                    (condition (= obs (flip bias))))
                  bias))]
  (mean samples))
;=> ~0.75

;; Exact enumeration
(let [[values probs] (enumeration-query
                       (let [a (flip) b (flip)]
                         (condition (or a b))
                         (list a b)))]
  (zipmap values probs))

;; Distribution objects — sample, score, enumerate
(let [d (gaussian-dist 0 1)]
  [(sample* d)        ;=> a random draw
   (observe* d 0.5)]) ;=> log-probability of 0.5

;; Observe for soft conditioning
(mh-query 1000 1
  (let [mu (gaussian 0 10)]
    (observe (gaussian-dist mu 1) 5.0)
    (observe (gaussian-dist mu 1) 4.5)
    mu))

;; Unified inference entry point
(infer {:method :mh :samples 1000 :lag 1 :burn 100}
  (fn []
    (let [x (flip 0.7)]
      (condition x)
      x)))

;; Forward sampling (ignore all conditioning)
(forward-query 100 (flip 0.7))
;=> [true true false true ...]

;; Memoized stochastic functions
(let [eye-color (mem (fn [person] (uniform-draw ["blue" "brown" "green"])))]
  [(eye-color "alice") (eye-color "alice")])  ;=> same value twice

;; Dirichlet Process memoization
(let [get-category (DPmem 1.0 (fn [x] (gaussian 0 1)))]
  [(get-category "a") (get-category "a")])  ;=> same value twice (CRP)
```

## API

### Elementary Random Primitives

All available from `prob.core`:

| Function | Description |
|----------|-------------|
| `(flip)` / `(flip p)` | Bernoulli; returns boolean, p defaults to 0.5 |
| `(gaussian mu sigma)` | Normal distribution |
| `(uniform a b)` | Continuous uniform on [a, b] |
| `(uniform-draw lst)` | Discrete uniform over list elements |
| `(random-integer n)` | Uniform integer in [0, n) |
| `(multinomial items probs)` | Weighted discrete choice |
| `(sample-discrete weights)` | Sample index by weights |
| `(beta a b)` | Beta distribution |
| `(gamma shape scale)` | Gamma distribution |
| `(dirichlet alpha-vec)` | Dirichlet distribution |
| `(exponential rate)` | Exponential distribution |
| `(binomial n p)` | Binomial distribution |
| `(poisson mu)` | Poisson distribution |
| `(categorical vals probs)` | Labeled weighted discrete choice |

### Distributions (First-Class Objects)

Distribution constructors return objects implementing the `IDistribution` protocol:

| Constructor | Description |
|-------------|-------------|
| `(bernoulli-dist p)` | Bernoulli (enumerable, proposable) |
| `(gaussian-dist mu sigma)` | Gaussian (drift proposal) |
| `(uniform-dist a b)` | Continuous uniform |
| `(beta-dist a b)` | Beta (drift proposal) |
| `(gamma-dist shape scale)` | Gamma |
| `(exponential-dist rate)` | Exponential |
| `(dirichlet-dist alpha)` | Dirichlet |
| `(binomial-dist n p)` | Binomial (enumerable) |
| `(poisson-dist mu)` | Poisson |
| `(categorical-dist vals probs)` | Categorical (enumerable, proposable) |
| `(uniform-draw-dist items)` | Discrete uniform (enumerable, proposable) |
| `(random-integer-dist n)` | Random integer (enumerable, proposable) |
| `(multinomial-dist items probs)` | Multinomial (enumerable, proposable) |
| `(sample-discrete-dist probs)` | Sample-discrete (enumerable, proposable) |
| `(delta-dist v)` | Point mass (enumerable) |
| `(cauchy-dist loc scale)` | Cauchy |
| `(laplace-dist loc scale)` | Laplace |
| `(lognormal-dist mu sigma)` | Log-normal |
| `(student-t-dist df loc scale)` | Student's t |
| `(mixture-dist dists probs)` | Mixture of distributions |
| `(kde-dist data)` | Kernel density estimate (Silverman bandwidth) |
| `(uniform-discrete-dist lo hi)` | Discrete uniform on [lo, hi] (enumerable, proposable) |
| `(chi-squared-dist df)` | Chi-squared (Gamma wrapper) |
| `(logit-normal-dist mu sigma)` | Logit-normal; samples in (0, 1) |
| `(marginal-dist values probs)` | Empirical marginal from inference |

Protocol operations:

| Function | Description |
|----------|-------------|
| `(sample* dist)` | Draw a sample |
| `(observe* dist value)` | Log-probability of value under dist |
| `(enumerate* dist)` | Support of discrete distribution |
| `(entropy dist)` | Shannon entropy of discrete distribution |
| `(kl-divergence p q)` | KL divergence between discrete distributions |
| `(dist? x)` | Check if x is a distribution |
| `(discrete? x)` | Check if x has finite support |
| `(continuous? x)` | Check if x is continuous |

### Inference

Query macros from `prob.macros`:

| Macro | Description |
|-------|-------------|
| `(rejection-query & body)` | Single sample via rejection sampling |
| `(mh-query n lag & body)` | n samples via single-site trace-based MH |
| `(enumeration-query & body)` | Exact enumeration; returns `[values probs]` |
| `(importance-query n & body)` | Importance sampling; returns `[values probs]` |
| `(forward-query n & body)` | Prior samples (condition/factor/observe are no-ops) |
| `(mh-query-scored n lag & body)` | MH returning `{:value :score}` maps |
| `(map-query n lag & body)` | MAP: highest-scoring MH sample |
| `(query method & body)` | Reusable conditional sampler |

Unified entry point from `prob.core`:

```clojure
(infer {:method :mh         ;; :rejection :mh :enumeration :importance :forward :mh-scored :map
        :samples 1000
        :lag 1
        :burn 100
        :callback (fn [{:keys [iter value score]}] ...)}
  model-thunk)
```

Enumeration supports strategies:

```clojure
(infer {:method :enumeration :strategy :likely-first :max-executions 10000} thunk)
```

Conditioning functions from `prob.core`:

| Function | Description |
|----------|-------------|
| `(condition pred)` | Hard constraint — rejects when pred is false |
| `(factor log-weight)` | Soft constraint — adds log-weight to score |
| `(observe dist value)` | Soft constraint — adds `(observe* dist value)` to score |
| `(condition-equal thunk value)` | Soft conditioning via nested enumeration |

### Utilities

From `prob.core`:

| Function | Description |
|----------|-------------|
| `(mem f)` | Trace-aware stochastic memoization |
| `(cache f)` / `(cache f n)` | LRU-bounded deterministic memoization |
| `(DPmem alpha f)` | Dirichlet Process memoization (CRP) |
| `(mode lst)` | Most frequent value |
| `(mean lst)` | Mean |
| `(variance lst)` | Variance |
| `(sd lst)` | Standard deviation |
| `(weighted-mean vals weights)` | Weighted mean |
| `(weighted-variance vals weights)` | Weighted variance |
| `(expectation samples)` / `(expectation samples f)` | Expected value |
| `(empirical-distribution samples)` | Frequency map |
| `(sum lst)` / `(prod lst)` | Sum / product |
| `(repeat-fn n f)` | Call f n times, collect results |
| `(set-seed! n)` | Seed the PRNG (xoshiro128**) |

### Math

From `prob.core`:

| Function | Description |
|----------|-------------|
| `(log-gamma-fn z)` | Log-Gamma (Lanczos approximation) |
| `(log-beta-fn a b)` | Log-Beta |
| `(log-fact n)` | Log-factorial |
| `(log-sum-exp a b)` / `(log-sum-exp xs)` | Numerically stable log-sum-exp |
| `(digamma x)` | Digamma function |
| `(erf x)` | Error function |

### Extended Builtins

`prob.builtins` provides Lisp-style list operations (`pair`/`car`/`cdr`/`null?`/`fold`/`zip`), set operations (`union`/`intersection`/`difference`), math wrappers, string operations, type predicates, and more. See [builtins.cljs](src/prob/builtins.cljs) for the full list.

## Browser (Scittle)

prob-cljs runs in the browser via Scittle with no build step:

```html
<script src="https://cdn.jsdelivr.net/npm/scittle@0.6.20/dist/scittle.js"></script>
<!-- load prob-cljs source files -->
<script type="application/x-scittle">
(require '[prob.core :refer [flip gaussian mh-query-fn condition mean]])

(let [samples (mh-query-fn 1000 1
                (fn []
                  (let [x (flip 0.7)]
                    (condition x)
                    x)))]
  (js/console.log (mean (map #(if % 1 0) samples))))
</script>
```

The `docs/` directory contains a full deployment with interactive ProbMods tutorial chapters (1-9).

## Project Structure

```
src/prob/
  core.cljs       - Public API (re-exports everything)
  erp.cljs        - Elementary Random Primitives (trace-aware sampling)
  dist.cljs       - Distribution protocol + 24 distribution types
  inference.cljs  - Inference: rejection, MH, enumeration, importance, forward, MAP
  math.cljs       - Special functions: log-gamma, digamma, erf, log-sum-exp
  builtins.cljs   - Utilities: list ops, math, strings, mem, cache, DPmem, stats
  macros.clj      - Query macros (rejection-query, mh-query, etc.)
  sci.cljs        - SCI configuration for Scittle browser deployment
src/scittle/
  prob.cljs       - Scittle plugin entry point
docs/
  prob/           - Source copies for GitHub Pages (loaded via Scittle)
  probmods/       - ProbMods tutorial chapters 1-9 (interactive, browser-based)
examples/
  prob-demo.cljs  - Comprehensive demo
  ink-task-list/  - React/Ink terminal UI example
test/
  prob_tests.cljs - Test suite (215 tests)
```

## How It Works

**Exception-based rejection**: `condition` throws a sentinel exception. `rejection-query-fn` catches and retries. `factor` does probabilistic rejection in rejection mode, exact score accumulation in MH mode.

**Single-site trace-based MH**: Initializes trace via rejection, then proposes changes to one random choice per step. Drift proposals for Gaussian and Beta; smart discrete proposals for Bernoulli, Categorical, etc. Persistent hash-map traces for structural sharing.

**Enumeration**: Discovery pass finds all choice points and their domains. Full odometer or likelyFirst (priority queue) exploration. Normalization via log-sum-exp.

**Scoped volatiles**: Inference state uses `volatile!` holding persistent hash-maps. Volatiles never escape inference boundaries — outside inference, ERPs are pure sampling functions.

## References

- [Probabilistic Models of Cognition](https://probmods.org/) — Church/WebPPL tutorial
- [webchurch](https://github.com/probmods/webchurch) — Original Church-to-JS compiler
- [Goodman et al. (2008)](https://web.stanford.edu/~ngoodman/papers/churchUAI08_rev2.pdf) — "Church: a language for generative models"
