(ns prob.macros
  "Query macros for probabilistic programming.
   These wrap the body in a thunk and delegate to prob.core inference fns.

   Expansion helpers (*-suffixed functions) are shared between:
   - defmacro forms (nbb, via #?(:org.babashka/nbb ...) reader conditional)
   - SCI ^:macro functions (Scittle, via prob.sci)

   Usage (nbb):
     (ns my-ns
       (:require [prob.core :refer [flip condition mean]])
       (:require-macros [prob.macros :refer [rejection-query mh-query enumeration-query]]))"
  (:require [prob.cps-transform :as ct]))

;; ---------------------------------------------------------------------------
;; Expansion helpers (plain functions, work in both CLJ and CLJS)
;; ---------------------------------------------------------------------------

(defn rejection-query* [body]
  `(prob.core/rejection-query-fn (fn [] ~@body)))

(defn mh-query* [n lag body]
  `(prob.core/mh-query-fn ~n ~lag (fn [] ~@body)))

(defn importance-query* [n body]
  `(prob.core/importance-query-fn ~n (fn [] ~@body)))

(defn enumeration-query* [body]
  `(prob.core/enumeration-query-fn (fn [] ~@body)))

(defn mh-query-scored* [n lag body]
  `(prob.core/mh-query-scored-fn ~n ~lag (fn [] ~@body)))

(defn map-query* [n lag body]
  `(prob.core/map-query-fn ~n ~lag (fn [] ~@body)))

(defn forward-query* [n body]
  `(prob.core/forward-query-fn ~n (fn [] ~@body)))

(defn query* [method body]
  `(prob.core/conditional-fn ~method (fn [] ~@body)))

(defn ais-query* [n-particles body]
  (let [[opts body] (if (map? (first body))
                      [(first body) (rest body)]
                      [{} body])]
    `(prob.core/ais-query-fn ~n-particles ~opts (fn [] ~@body))))

(defn smc-query* [n-particles body]
  (let [[opts body] (if (map? (first body))
                      [(first body) (rest body)]
                      [{} body])
        cps-body (ct/cps-of-expr (cons 'do body) 'k__final)]
    `(prob.core/smc-query-fn ~n-particles ~opts
       (fn [~'k__final ~'$state] ~cps-body))))

(defn particle-gibbs-query* [n-particles n-samples body]
  (let [[opts body] (if (map? (first body))
                      [(first body) (rest body)]
                      [{} body])
        cps-body (ct/cps-of-expr (cons 'do body) 'k__final)]
    `(prob.core/particle-gibbs-fn ~n-particles ~n-samples ~opts
       (fn [~'k__final ~'$state] ~cps-body))))

;; ---------------------------------------------------------------------------
;; Macros (nbb only — nbb reads .cljc with :org.babashka/nbb feature,
;;         shadow-cljs does not, so these are invisible to the Scittle build)
;; ---------------------------------------------------------------------------

#?(:org.babashka/nbb
   (do
     (defmacro rejection-query
       "Sample a single value from the conditional distribution defined by body."
       [& body]
       (rejection-query* body))

     (defmacro mh-query
       "Generate n samples with given lag using Metropolis-Hastings."
       [n lag & body]
       (mh-query* n lag body))

     (defmacro importance-query
       "Importance sampling with n samples."
       [n & body]
       (importance-query* n body))

     (defmacro enumeration-query
       "Approximate the conditional distribution by enumeration."
       [& body]
       (enumeration-query* body))

     (defmacro mh-query-scored
       "Like mh-query but returns {:value :score} maps."
       [n lag & body]
       (mh-query-scored* n lag body))

     (defmacro map-query
       "MAP inference: return the highest-scoring value from MH samples."
       [n lag & body]
       (map-query* n lag body))

     (defmacro forward-query
       "Forward sampling: run body n times from the prior."
       [n & body]
       (forward-query* n body))

     (defmacro query
       "Create a reusable conditional sampler."
       [method & body]
       (query* method body))

     (defmacro ais-query
       "Annealed Importance Sampling with n-particles.
        Optionally accepts an opts map as the first body argument."
       [n-particles & body]
       (ais-query* n-particles body))

     (defmacro smc-query
       "Sequential Monte Carlo inference with n-particles.
        CPS-transforms the model body at macro expansion time."
       [n-particles & body]
       (smc-query* n-particles body))

     (defmacro particle-gibbs-query
       "Particle Gibbs (PMCMC) inference."
       [n-particles n-samples & body]
       (particle-gibbs-query* n-particles n-samples body))))
