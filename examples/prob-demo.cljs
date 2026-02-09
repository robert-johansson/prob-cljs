(ns prob-demo
  "Demo: probabilistic programming as a native ClojureScript library.
   Run with: nbb -cp src examples/prob-demo.cljs"
  (:require [prob.core :refer [flip gaussian beta uniform uniform-draw condition factor
                                mem mean variance rejection-query-fn mh-query-fn]])
  (:require-macros [prob.macros :refer [rejection-query mh-query enumeration-query]]))

;; ---------------------------------------------------------------------------
;; 1. Basic coin flip with condition
;; ---------------------------------------------------------------------------

(println "--- Rejection query: flip with condition ---")
(println "Result:" (rejection-query
                     (let [x (flip)]
                       (condition x)
                       x)))
;; Always true — we conditioned on it

;; ---------------------------------------------------------------------------
;; 2. Estimate a coin's bias from observations
;; ---------------------------------------------------------------------------

(println "\n--- Estimate bias of a coin ---")
(let [observations [true true true false true true false true true true]
      samples (mh-query 500 1
                (let [bias (beta 1 1)]
                  (doseq [obs observations]
                    (condition (= obs (flip bias))))
                  bias))
      est (mean samples)]
  (println "Observations:" observations)
  (println "Estimated bias:" est "(expected ~0.8)"))

;; ---------------------------------------------------------------------------
;; 3. Using factor for soft conditioning
;; ---------------------------------------------------------------------------

(println "\n--- Soft conditioning with factor ---")
(let [samples (mh-query 500 1
                (let [x (uniform-draw [1 2 3 4 5])]
                  ;; Prefer larger values
                  (factor (- x 3))
                  x))]
  (println "Mean with soft preference for larger:" (mean samples) "(expected >3)"))

;; ---------------------------------------------------------------------------
;; 4. Enumeration query
;; ---------------------------------------------------------------------------

(println "\n--- Enumeration: which coin is heads given at least one is? ---")
(let [[values probs] (enumeration-query
                       (let [a (flip) b (flip)]
                         (condition (or a b))
                         (list a b)))]
  (println "Values:" (vec values))
  (println "Probs: " (vec probs)))

;; ---------------------------------------------------------------------------
;; 5. Memoization — stochastic functions that are consistent
;; ---------------------------------------------------------------------------

(println "\n--- Memoized random function ---")
(let [eye-color (mem (fn [person] (if (flip 0.3) "blue" "brown")))]
  (println "Alice's eye color:" (eye-color "alice"))
  (println "Alice's eye color (same!):" (eye-color "alice"))
  (println "Bob's eye color:" (eye-color "bob")))

;; ---------------------------------------------------------------------------
;; 6. Native ClojureScript interop — no wrappers needed
;; ---------------------------------------------------------------------------

(println "\n--- Native ClojureScript interop ---")
(let [path (js/require "path")]
  (println "Joined path:" (.join path "/tmp" "prob" "output.txt")))

(println "\nDone!")
