(ns prob.macros
  "Query macros for probabilistic programming.
   These wrap the body in a thunk and delegate to prob.core inference fns.

   Usage:
     (ns my-ns
       (:require [prob.core :refer [flip condition mean]])
       (:require-macros [prob.macros :refer [rejection-query mh-query enumeration-query]]))")

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
