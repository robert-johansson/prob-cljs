# prob-cljs

Probabilistic programming as a ClojureScript library for [nbb](https://github.com/babashka/nbb).

Write normal ClojureScript/nbb scripts that use `flip`, `gaussian`, `rejection-query`, etc. alongside `(js/require "axios")`, Clojure's `let`/`defn`/`map`, and everything else nbb provides. No DSL strings, no wrappers — just `require` and go.

## Quick Start

```bash
# Requires nbb: npm install -g nbb

# Run the demo
nbb -cp src examples/prob-demo.cljs

# Run tests
nbb -cp src:test test/prob_tests.cljs
```

## Usage

```clojure
(ns my-analysis
  (:require [prob.core :refer [flip gaussian beta uniform condition factor
                                mem mean variance]])
  (:require-macros [prob.macros :refer [rejection-query mh-query enumeration-query]]))

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

;; Enumeration
(let [[values probs] (enumeration-query
                       (let [a (flip) b (flip)]
                         (condition (or a b))
                         (list a b)))]
  (zipmap values probs))

;; Memoized stochastic functions
(let [eye-color (mem (fn [person] (uniform-draw ["blue" "brown" "green"])))]
  [(eye-color "alice") (eye-color "alice")])  ;=> same value twice

;; npm interop — just works
(def fs (js/require "fs"))
(def data (-> (.readFileSync fs "data.json" "utf8") js/JSON.parse js->clj))
```

## API

### Elementary Random Primitives

All available from `prob.core`:

| Function | Description |
|----------|-------------|
| `(flip)` / `(flip p)` | Bernoulli; returns boolean, p defaults to 0.5 |
| `(gaussian)` / `(gaussian mu sigma)` | Normal distribution (default 0, 1) |
| `(uniform a b)` | Continuous uniform on [a, b] |
| `(uniform-draw lst)` | Discrete uniform over list elements |
| `(random-integer n)` | Uniform integer in [0, n) |
| `(multinomial items probs)` | Weighted discrete choice |
| `(beta a b)` | Beta distribution |
| `(gamma shape scale)` | Gamma distribution |
| `(dirichlet alpha-vec)` | Dirichlet distribution |
| `(exponential rate)` | Exponential distribution |
| `(sample-discrete weights)` | Sample index by weights |

### Inference

Query macros from `prob.macros`:

| Macro | Description |
|-------|-------------|
| `(rejection-query & body)` | Single sample via rejection sampling |
| `(mh-query n lag & body)` | n samples via Metropolis-Hastings |
| `(enumeration-query & body)` | Returns `(values probs)` pair |
| `(query method & body)` | Reusable conditional sampler |

Conditioning functions from `prob.core`:

| Function | Description |
|----------|-------------|
| `(condition pred)` | Hard constraint — rejects when pred is false |
| `(factor log-weight)` | Soft constraint — probabilistic rejection |

### Utilities

From `prob.core`:

| Function | Description |
|----------|-------------|
| `(mem f)` | Memoize a stochastic function |
| `(mean lst)` | Mean of a list of numbers |
| `(variance lst)` | Variance of a list of numbers |
| `(sum lst)` | Sum of a list |
| `(prod lst)` | Product of a list |
| `(repeat-fn n f)` | Call f n times, collect results |

### Extended Builtins

`prob.builtins` provides additional functions (list ops, math, string ops, comparisons) for direct use:

```clojure
(:require [prob.builtins :as b])

(b/mean [1 2 3 4])     ;=> 5/2
(b/sum [1 2 3])         ;=> 6
(b/mem some-fn)         ;=> memoized version
(b/expt 2 10)           ;=> 1024
(b/union [1 2] [2 3])   ;=> [1 2 3]
```

## Project Structure

```
src/prob/
  core.cljs       - Public API (re-exports ERPs, inference, utilities)
  macros.clj      - Query macros (rejection-query, mh-query, etc.)
  erp.cljs        - Elementary Random Primitives
  inference.cljs  - Inference algorithms
  builtins.cljs   - Utility functions (math, lists, strings, etc.)
examples/
  prob-demo.cljs  - Comprehensive demo
  ink-task-list/  - React/Ink terminal UI with probabilistic outcomes
test/
  prob_tests.cljs - Test suite
```

## Future Work

- **Trace-based MH**: Proper Metropolis-Hastings with address tracking and proposals
- **True enumeration**: Continuation-based exploration of all ERP paths
- **Particle filtering / SMC**: Sequential Monte Carlo
- **DPmem**: Dirichlet Process stochastic memoization
- **WebPPL-style inference**: Variational inference, HMC

## References

- [Probabilistic Models of Cognition](https://probmods.org/) — Church/WebPPL tutorial and examples
- [webchurch](https://github.com/probmods/webchurch) — Original Church-to-JS compiler
- [Goodman et al. (2008)](https://web.stanford.edu/~ngoodman/papers/churchUAI08_rev2.pdf) — "Church: a language for generative models"
