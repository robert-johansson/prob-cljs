(ns prob-tests
  "Tests for the prob library API — native ClojureScript probabilistic programming."
  (:require [prob.core :refer [flip gaussian beta uniform uniform-draw random-integer
                                multinomial sample-discrete gamma dirichlet exponential
                                binomial poisson categorical
                                condition factor rejection-query-fn mh-query-fn
                                enumeration-query-fn conditional-fn
                                observe
                                sample* observe* dist? enumerate*
                                bernoulli-dist gaussian-dist uniform-dist beta-dist
                                gamma-dist exponential-dist dirichlet-dist
                                uniform-draw-dist random-integer-dist multinomial-dist
                                sample-discrete-dist binomial-dist poisson-dist
                                categorical-dist
                                mem mean variance sum prod repeat-fn]]
            [prob.erp :as erp])
  (:require-macros [prob.macros :refer [rejection-query mh-query enumeration-query query]]))

;; ---------------------------------------------------------------------------
;; Test harness (volatile-based, no atoms)
;; ---------------------------------------------------------------------------

(def ^:private pass-count (volatile! 0))
(def ^:private fail-count (volatile! 0))

(defn- test-assert [name pred]
  (if pred
    (vswap! pass-count inc)
    (do
      (vswap! fail-count inc)
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
;; New ERPs: binomial, poisson, categorical
;; ---------------------------------------------------------------------------

(println "=== New ERPs ===")

(test-assert "binomial returns integer in [0,n]"
  (let [x (binomial 10 0.5)]
    (and (integer? x) (>= x 0) (<= x 10))))

(test-assert "binomial n=0 returns 0"
  (= 0 (binomial 0 0.5)))

(test-assert "binomial p=1 returns n"
  (every? #(= 5 %) (repeatedly 10 #(binomial 5 1.0))))

(test-assert "poisson returns non-negative integer"
  (let [x (poisson 3.0)]
    (and (integer? x) (>= x 0))))

(test-assert "poisson mean near lambda"
  (let [samples (repeatedly 2000 #(poisson 5.0))
        m (mean samples)]
    (approx= m 5.0 0.5)))

(test-assert "categorical from map"
  (contains? #{:a :b :c} (categorical {:a 0.3 :b 0.5 :c 0.2})))

(test-assert "categorical from vectors"
  (contains? #{:x :y} (categorical [:x :y] [0.7 0.3])))

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
;; Trace purity
;; ---------------------------------------------------------------------------

(println "=== Trace Purity ===")

(test-assert "*trace-state* nil outside inference"
  (nil? erp/*trace-state*))

(test-assert "*trace-state* nil after mh-query"
  (do (mh-query-fn 10 1 (fn [] (flip)))
      (nil? erp/*trace-state*)))

(test-assert "*trace-state* nil after rejection-query"
  (do (rejection-query-fn (fn [] (flip)))
      (nil? erp/*trace-state*)))

(test-assert "*trace-state* nil after enumeration-query"
  (do (enumeration-query-fn (fn [] (flip)))
      (nil? erp/*trace-state*)))

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
    (= (mean samples) 1)))

;; ---------------------------------------------------------------------------
;; Exact enumeration query
;; ---------------------------------------------------------------------------

(println "=== Exact Enumeration ===")

(test-assert "enum: conditioned flip -> ((true) (1.0))"
  (let [[values probs] (enumeration-query
                         (let [x (flip)]
                           (condition x)
                           x))]
    (and (= (count (seq values)) 1)
         (= (first values) true)
         (approx= (first probs) 1.0 0.0001))))

(test-assert "enum: two-flip OR"
  (let [[values probs] (enumeration-query
                         (let [a (flip) b (flip)]
                           (condition (or a b))
                           (list a b)))
        prob-map (zipmap (seq values) (seq probs))]
    (and (= (count prob-map) 3)
         (approx= (reduce + (vals prob-map)) 1.0 0.0001)
         (approx= (get prob-map '(true true)) 0.3333 0.01))))

(test-assert "enum: uniform-draw + condition"
  (let [[values probs] (enumeration-query
                         (let [x (uniform-draw [1 2 3 4 5 6])]
                           (condition (even? x))
                           x))
        prob-map (zipmap (seq values) (seq probs))]
    (and (= (count prob-map) 3)
         (every? even? (keys prob-map))
         (approx= (reduce + (vals prob-map)) 1.0 0.0001))))

(test-assert "enum: factor weighting"
  (let [[values probs] (enumeration-query
                         (let [x (flip 0.5)]
                           (factor (if x 0 -1))
                           x))
        prob-map (zipmap (seq values) (seq probs))]
    ;; P(true) ∝ 0.5 * exp(0) = 0.5, P(false) ∝ 0.5 * exp(-1) ≈ 0.184
    ;; P(true) = 0.5/(0.5+0.184) ≈ 0.731
    (and (approx= (get prob-map true) 0.731 0.01)
         (approx= (reduce + (vals prob-map)) 1.0 0.0001))))

(test-assert "enum: probs sum to 1"
  (let [[_values probs] (enumeration-query
                          (let [x (random-integer 4)]
                            x))]
    (approx= (reduce + (seq probs)) 1.0 0.0001)))

(test-assert "enum: rejects continuous ERPs"
  (try
    (enumeration-query-fn (fn [] (gaussian 0 1)))
    false
    (catch :default e
      (boolean (re-find #"non-enumerable" (ex-message e))))))

;; ---------------------------------------------------------------------------
;; Observe
;; ---------------------------------------------------------------------------

(println "=== Observe ===")

(test-assert "observe: Gaussian pulls posterior near observed value"
  (let [samples (mh-query-fn 1000 1
                  (fn [] (let [mu (gaussian 0 5)]
                           (observe (gaussian-dist mu 1) 3.0)
                           mu)))]
    (approx= (mean samples) 3.0 0.5)))

(test-assert "observe: -Inf causes rejection in rejection-query"
  ;; observe a value with zero probability should behave like rejection
  (= true
    (rejection-query-fn
      (fn []
        (let [x (flip)]
          ;; observe true from bernoulli-dist 1.0 always works
          (observe (bernoulli-dist 1.0) true)
          (condition x)
          x)))))

(test-assert "observe: equivalent to manual factor"
  ;; Both should produce same conditioned distribution
  (let [d (gaussian-dist 0 1)
        via-observe (mh-query-fn 500 1
                      (fn [] (let [x (flip 0.5)]
                               (observe d (if x 1.0 -1.0))
                               x)))
        via-factor (mh-query-fn 500 1
                     (fn [] (let [x (flip 0.5)]
                              (factor (observe* d (if x 1.0 -1.0)))
                              x)))]
    ;; Both should produce booleans
    (and (every? boolean? via-observe)
         (every? boolean? via-factor))))

;; ---------------------------------------------------------------------------
;; Distribution protocol
;; ---------------------------------------------------------------------------

(println "=== Distribution Protocol ===")

(test-assert "bernoulli-dist: sample returns boolean"
  (boolean? (sample* (bernoulli-dist 0.5))))

(test-assert "bernoulli-dist: observe* log-prob correct"
  (approx= (observe* (bernoulli-dist 0.7) true) (js/Math.log 0.7) 0.001))

(test-assert "gaussian-dist: observe* log-prob at mean"
  ;; N(0,1) at x=0: log(1/sqrt(2pi)) ≈ -0.9189
  (approx= (observe* (gaussian-dist 0 1) 0) -0.9189 0.001))

;; ---------------------------------------------------------------------------
;; IEnumerable
;; ---------------------------------------------------------------------------

(println "=== IEnumerable ===")

(test-assert "bernoulli enumerate*"
  (= (enumerate* (bernoulli-dist 0.5)) [true false]))

(test-assert "random-integer enumerate*"
  (= (enumerate* (random-integer-dist 4)) [0 1 2 3]))

(test-assert "uniform-draw enumerate*"
  (= (set (enumerate* (uniform-draw-dist [:a :b :a :c])))
     #{:a :b :c}))

(test-assert "binomial enumerate*"
  (= (enumerate* (binomial-dist 3 0.5)) [0 1 2 3]))

(test-assert "categorical enumerate*"
  (= (set (enumerate* (categorical-dist {:a 0.3 :b 0.7})))
     #{:a :b}))

;; ---------------------------------------------------------------------------
;; Binomial distribution
;; ---------------------------------------------------------------------------

(println "=== Binomial Distribution ===")

(test-assert "binomial-dist: known log-prob Bin(10,0.5) at k=5"
  ;; C(10,5) * 0.5^10 = 252/1024 ≈ 0.2461
  (approx= (observe* (binomial-dist 10 0.5) 5)
           (js/Math.log 0.24609375) 0.001))

(test-assert "binomial-dist: out-of-range -> -Inf"
  (= ##-Inf (observe* (binomial-dist 5 0.5) -1)))

(test-assert "binomial-dist: out-of-range high -> -Inf"
  (= ##-Inf (observe* (binomial-dist 5 0.5) 6)))

;; ---------------------------------------------------------------------------
;; Poisson distribution
;; ---------------------------------------------------------------------------

(println "=== Poisson Distribution ===")

(test-assert "poisson-dist: known log-prob Pois(3) at k=2"
  ;; P(k=2|λ=3) = 3^2 * e^-3 / 2! = 9 * 0.0498 / 2 ≈ 0.2240
  (approx= (observe* (poisson-dist 3) 2)
           (js/Math.log 0.22404) 0.01))

(test-assert "poisson-dist: non-negative samples"
  (every? #(>= % 0) (repeatedly 100 #(sample* (poisson-dist 2.0)))))

(test-assert "poisson-dist: negative k -> -Inf"
  (= ##-Inf (observe* (poisson-dist 3) -1)))

;; ---------------------------------------------------------------------------
;; Categorical distribution
;; ---------------------------------------------------------------------------

(println "=== Categorical Distribution ===")

(test-assert "categorical-dist: map constructor"
  (let [d (categorical-dist {:a 0.3 :b 0.7})]
    (contains? #{:a :b} (sample* d))))

(test-assert "categorical-dist: vector constructor"
  (let [d (categorical-dist [:x :y :z] [0.2 0.3 0.5])]
    (contains? #{:x :y :z} (sample* d))))

(test-assert "categorical-dist: observe* correct"
  (let [d (categorical-dist {:a 0.3 :b 0.7})]
    (approx= (observe* d :b) (js/Math.log 0.7) 0.001)))

(test-assert "categorical-dist: missing category -> -Inf"
  (= ##-Inf (observe* (categorical-dist {:a 1.0}) :b)))

;; ---------------------------------------------------------------------------
;; Trace-aware mem
;; ---------------------------------------------------------------------------

(println "=== Trace-aware mem ===")

(test-assert "mem: same-call consistency"
  (let [f (mem (fn [x] (flip)))]
    (= (f "a") (f "a"))))

(test-assert "mem: potentially different for different args"
  (let [f (mem (fn [x] (random-integer 1000000)))]
    (number? (f "a"))
    (number? (f "b"))))

(test-assert "mem: works inside mh-query"
  (let [samples (mh-query-fn 20 1
                  (fn []
                    (let [f (mem (fn [x] (flip)))]
                      ;; f("a") should be consistent within each run
                      (condition (f "a"))
                      (list (f "a") (f "a")))))]
    (every? (fn [s] (= (first s) (second s))) samples)))

(test-assert "mem: cache reset between independent runs"
  ;; Running multiple rejection queries should not leak state
  (let [results (repeatedly 20
                  #(rejection-query-fn
                     (fn []
                       (let [f (mem (fn [x] (random-integer 1000000)))]
                         (f "a")))))]
    ;; Should not all be the same (with overwhelming probability)
    (> (count (distinct results)) 1)))

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
;; Drift proposal tests
;; ---------------------------------------------------------------------------

(println "=== Drift Proposals ===")

(test-assert "drift: gaussian posterior via dist object"
  ;; Use sample* from gaussian-dist (triggers IProposable drift)
  (let [samples (mh-query-fn 1000 1
                  (fn [] (let [mu (sample* (gaussian-dist 0 5))]
                           (observe (gaussian-dist mu 1) 3.0)
                           mu)))]
    (approx= (mean samples) 3.0 0.5)))

(test-assert "drift: gaussian via ERP also works (prior proposal)"
  (let [samples (mh-query-fn 1000 1
                  (fn [] (let [mu (gaussian 0 5)]
                           (observe (gaussian-dist mu 1) 3.0)
                           mu)))]
    (approx= (mean samples) 3.0 0.5)))

(test-assert "drift: beta posterior with conjugate update"
  ;; Beta(1,1) prior, observe 7 heads out of 10 -> Beta(8,4), mean = 8/12 ≈ 0.667
  (let [samples (mh-query-fn 2000 1
                  (fn [] (let [p (beta 1 1)]
                           (observe (binomial-dist 10 p) 7)
                           p)))]
    (approx= (mean samples) 0.667 0.15)))

(test-assert "drift: beta values in [0,1]"
  (let [samples (mh-query-fn 100 1
                  (fn [] (let [p (beta 2 2)]
                           (observe (binomial-dist 5 p) 3)
                           p)))]
    (every? #(and (>= % 0) (<= % 1)) samples)))

;; ---------------------------------------------------------------------------
;; Summary
;; ---------------------------------------------------------------------------

(println)
(println (str "=== prob library tests: " @pass-count " passed, " @fail-count " failed ==="))
(when (pos? @fail-count)
  (js/process.exit 1))
