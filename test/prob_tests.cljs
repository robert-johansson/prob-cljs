(ns prob-tests
  "Tests for the prob library API — native ClojureScript probabilistic programming."
  (:require [prob.core :refer [flip gaussian beta uniform uniform-draw random-integer
                                multinomial sample-discrete gamma dirichlet exponential
                                condition factor rejection-query-fn mh-query-fn
                                enumeration-query-fn conditional-fn
                                mem mean variance sum prod repeat-fn]])
  (:require-macros [prob.macros :refer [rejection-query mh-query enumeration-query query]]))

;; ---------------------------------------------------------------------------
;; Test harness
;; ---------------------------------------------------------------------------

(def ^:private pass-count (atom 0))
(def ^:private fail-count (atom 0))

(defn- test-assert [name pred]
  (if pred
    (swap! pass-count inc)
    (do
      (swap! fail-count inc)
      (println "FAIL:" name))))

(defn- approx= [a b tol]
  (< (js/Math.abs (- a b)) tol))

;; ---------------------------------------------------------------------------
;; ERP tests
;; ---------------------------------------------------------------------------

(println "=== ERPs ===")

(test-assert "flip returns boolean"
  (boolean? (flip)))

(test-assert "flip 1.0 always true"
  (every? true? (repeatedly 20 #(flip 1.0))))

(test-assert "flip 0.0 always false"
  (every? false? (repeatedly 20 #(flip 0.0))))

(test-assert "gaussian returns number"
  (number? (gaussian)))

(test-assert "gaussian 5 0 returns exactly 5"
  (= (gaussian 5 0) 5))

(test-assert "uniform in range"
  (let [x (uniform 3 7)]
    (and (>= x 3) (<= x 7))))

(test-assert "uniform-draw from list"
  (contains? #{:a :b :c} (uniform-draw [:a :b :c])))

(test-assert "random-integer in range"
  (let [x (random-integer 10)]
    (and (>= x 0) (< x 10) (integer? x))))

(test-assert "beta in [0,1]"
  (let [x (beta 2 5)]
    (and (>= x 0) (<= x 1))))

(test-assert "gamma positive"
  (> (gamma 2 1) 0))

(test-assert "exponential positive"
  (> (exponential 1) 0))

(test-assert "dirichlet sums to ~1"
  (let [d (dirichlet [1 1 1])]
    (approx= (reduce + d) 1.0 0.001)))

(test-assert "multinomial returns item from list"
  (contains? #{:x :y :z} (multinomial [:x :y :z] [0.2 0.3 0.5])))

(test-assert "sample-discrete returns valid index"
  (let [i (sample-discrete [0.1 0.2 0.7])]
    (and (>= i 0) (< i 3))))

;; ---------------------------------------------------------------------------
;; Condition / Factor
;; ---------------------------------------------------------------------------

(println "=== Condition / Factor ===")

(test-assert "condition true does not throw"
  (do (condition true) true))

(test-assert "condition false throws"
  (try
    (condition false)
    false
    (catch :default _e true)))

(test-assert "factor 0 does not throw"
  (do (factor 0) true))

;; ---------------------------------------------------------------------------
;; Rejection query
;; ---------------------------------------------------------------------------

(println "=== Rejection Query ===")

(test-assert "rejection-query-fn basic"
  (= true (rejection-query-fn (fn [] (let [x (flip)] (condition x) x)))))

(test-assert "rejection-query macro basic"
  (= true (rejection-query (let [x (flip)] (condition x) x))))

(test-assert "rejection-query returns conditioned value"
  (let [result (rejection-query
                 (let [x (uniform-draw [1 2 3 4 5])]
                   (condition (> x 3))
                   x))]
    (and (> result 3) (<= result 5))))

;; ---------------------------------------------------------------------------
;; MH query
;; ---------------------------------------------------------------------------

(println "=== MH Query ===")

(test-assert "mh-query returns correct count"
  (= 50 (count (mh-query 50 1
                  (let [x (flip)] (condition x) x)))))

(test-assert "mh-query all true when conditioned"
  (every? true? (mh-query 20 1
                  (let [x (flip)] (condition x) x))))

(test-assert "mh-query mean of biased flip"
  (let [samples (mh-query 500 1
                  (let [x (flip 0.9)]
                    (condition x)
                    (if x 1 0)))]
    ;; All conditioned on true, so mean should be 1
    (= (mean samples) 1)))

;; ---------------------------------------------------------------------------
;; Enumeration query
;; ---------------------------------------------------------------------------

(println "=== Enumeration Query ===")

(test-assert "enumeration-query returns values and probs"
  (let [result (enumeration-query
                 (let [x (flip)]
                   (condition x)
                   x))]
    (and (seq? result)
         (= 2 (count result)))))

(test-assert "enumeration-query conditioned flip is all true"
  (let [[values probs] (enumeration-query
                         (let [x (flip)]
                           (condition x)
                           x))]
    ;; Only true should appear
    (and (every? true? values)
         (every? #(= 1 %) probs))))

;; ---------------------------------------------------------------------------
;; Conditional (query macro)
;; ---------------------------------------------------------------------------

(println "=== Conditional / Query ===")

(test-assert "query rejection returns function"
  (fn? (query '(rejection)
         (let [x (flip)] (condition x) x))))

(test-assert "query rejection samples correctly"
  (let [sampler (query '(rejection)
                  (let [x (flip)] (condition x) x))]
    (= true (sampler))))

;; ---------------------------------------------------------------------------
;; Utilities
;; ---------------------------------------------------------------------------

(println "=== Utilities ===")

(test-assert "mean of list"
  (= 2 (mean [1 2 3])))

(test-assert "variance of constant"
  (= 0 (variance [5 5 5])))

(test-assert "variance of [1 2 3]"
  (approx= (variance [1 2 3]) 0.6667 0.001))

(test-assert "sum of list"
  (= 10 (sum [1 2 3 4])))

(test-assert "prod of list"
  (= 24 (prod [1 2 3 4])))

(test-assert "mem returns same value for same arg"
  (let [f (mem (fn [x] (flip)))]
    (= (f "a") (f "a"))))

(test-assert "mem returns potentially different for different args"
  ;; Just test it doesn't crash — values may or may not differ
  (let [f (mem (fn [x] (random-integer 1000000)))]
    (number? (f "a"))
    (number? (f "b"))))

(test-assert "repeat-fn generates n samples"
  (= 10 (count (repeat-fn 10 flip))))

;; ---------------------------------------------------------------------------
;; Integration: real probabilistic inference
;; ---------------------------------------------------------------------------

(println "=== Integration ===")

(test-assert "fair coin: mean of flips near 0.5"
  (let [samples (mh-query 1000 1 (if (flip) 1 0))]
    (approx= (mean samples) 0.5 0.15)))

(test-assert "three-coin constraint: all must be heads"
  (let [samples (mh-query 100 1
                  (let [a (flip) b (flip) c (flip)]
                    (condition (and a b c))
                    (if a 1 0)))]
    (= (mean samples) 1)))

(test-assert "rejection-query with uniform-draw constraint"
  (let [result (rejection-query
                 (let [die (uniform-draw [1 2 3 4 5 6])]
                   (condition (even? die))
                   die))]
    (and (even? result) (>= result 1) (<= result 6))))

(test-assert "factor accepts factor 0"
  (let [samples (mh-query 20 1
                  (let [x (uniform-draw [1 2 3])]
                    (factor 0)
                    x))]
    (= 20 (count samples))))

;; ---------------------------------------------------------------------------
;; Summary
;; ---------------------------------------------------------------------------

(println)
(println (str "=== prob library tests: " @pass-count " passed, " @fail-count " failed ==="))
(when (pos? @fail-count)
  (js/process.exit 1))
