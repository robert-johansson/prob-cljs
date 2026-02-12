(ns prob.macros
  "Query macros for probabilistic programming.
   These wrap the body in a thunk and delegate to prob.core inference fns.

   Usage:
     (ns my-ns
       (:require [prob.core :refer [flip condition mean]])
       (:require-macros [prob.macros :refer [rejection-query mh-query enumeration-query]]))"
  (:require [prob.cps-transform :as ct]))

(defmacro rejection-query
  "Sample a single value from the conditional distribution defined by body.
   Use (condition expr) inside body to constrain.

   Example:
     (rejection-query
       (let [x (flip)]
         (condition x)
         x))
   ;=> true"
  [& body]
  `(prob.core/rejection-query-fn (fn [] ~@body)))

(defmacro mh-query
  "Generate n samples with given lag using Metropolis-Hastings.
   Returns a list of samples.

   Example:
     (mh-query 100 1
       (let [x (flip 0.7)]
         (condition x)
         x))
   ;=> (true true true ...)"
  [n lag & body]
  `(prob.core/mh-query-fn ~n ~lag (fn [] ~@body)))

(defmacro importance-query
  "Importance sampling with n samples.
   Returns (values probs).

   Example:
     (importance-query 1000
       (let [x (flip)]
         (factor (if x 0 -2))
         x))"
  [n & body]
  `(prob.core/importance-query-fn ~n (fn [] ~@body)))

(defmacro enumeration-query
  "Approximate the conditional distribution by enumeration.
   Returns (values probs).

   Example:
     (enumeration-query
       (let [x (flip)]
         (condition x)
         x))
   ;=> ((true) (1))"
  [& body]
  `(prob.core/enumeration-query-fn (fn [] ~@body)))

(defmacro mh-query-scored
  "Like mh-query but returns {:value :score} maps.

   Example:
     (mh-query-scored 100 1
       (let [x (flip 0.7)]
         (condition x)
         x))
   ;=> ({:value true :score ...} ...)"
  [n lag & body]
  `(prob.core/mh-query-scored-fn ~n ~lag (fn [] ~@body)))

(defmacro map-query
  "MAP inference: return the highest-scoring value from MH samples.

   Example:
     (map-query 500 1
       (let [x (uniform-draw [:a :b :c])]
         (condition (not= x :c))
         x))"
  [n lag & body]
  `(prob.core/map-query-fn ~n ~lag (fn [] ~@body)))

(defmacro forward-query
  "Forward sampling: run body n times from the prior.
   factor/observe/condition are no-ops.

   Example:
     (forward-query 100 (flip 0.7))
   ;=> [true true false true ...]"
  [n & body]
  `(prob.core/forward-query-fn ~n (fn [] ~@body)))

(defmacro query
  "Create a reusable conditional sampler.
   method is a list like '(rejection), '(enumerate), or '(mh lag).

   Example:
     (def coin (query '(rejection)
                 (let [x (flip)]
                   (condition x)
                   x)))
     (coin) ;=> true"
  [method & body]
  `(prob.core/conditional-fn ~method (fn [] ~@body)))

(defmacro smc-query
  "Sequential Monte Carlo inference with n-particles.
   CPS-transforms the model body at macro expansion time.
   Optionally accepts an opts map as first body form:
     (smc-query 100 {:rejuv-steps 5} ...)

   Example:
     (smc-query 100
       (let [p (beta 1 1)]
         (observe (bernoulli-dist p) true)
         (observe (bernoulli-dist p) true)
         (observe (bernoulli-dist p) false)
         p))
   ;=> list of weighted samples"
  [n-particles & body]
  (let [[opts body] (if (map? (first body))
                      [(first body) (rest body)]
                      [{} body])
        cps-body (ct/cps-of-expr (cons 'do body) 'k__final)]
    `(prob.core/smc-query-fn ~n-particles ~opts
       (fn [~'k__final ~'$state] ~cps-body))))

(defmacro particle-gibbs-query
  "Particle Gibbs (PMCMC) inference.
   n-particles: particles per SMC sweep. n-samples: MCMC samples to collect.
   Optionally accepts an opts map: {:burn :lag :rejuv-steps}

   Example:
     (particle-gibbs-query 20 200 {:burn 50}
       (let [p (beta 1 1)]
         (observe (bernoulli-dist p) true)
         (observe (bernoulli-dist p) false)
         p))"
  [n-particles n-samples & body]
  (let [[opts body] (if (map? (first body))
                      [(first body) (rest body)]
                      [{} body])
        cps-body (ct/cps-of-expr (cons 'do body) 'k__final)]
    `(prob.core/particle-gibbs-fn ~n-particles ~n-samples ~opts
       (fn [~'k__final ~'$state] ~cps-body))))
