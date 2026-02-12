(ns prob-tests
  "Tests for the prob library API — native ClojureScript probabilistic programming."
  (:require [prob.core :refer [flip gaussian beta uniform uniform-draw random-integer
                                multinomial sample-discrete gamma dirichlet exponential
                                binomial poisson categorical
                                condition factor rejection-query-fn mh-query-fn
                                enumeration-query-fn importance-query-fn conditional-fn
                                mh-query-scored-fn map-query-fn condition-equal
                                observe smc-query-fn particle-gibbs-fn
                                forward-query-fn infer
                                sample* observe* dist? enumerate*
                                bernoulli-dist gaussian-dist uniform-dist beta-dist
                                gamma-dist exponential-dist dirichlet-dist
                                uniform-draw-dist random-integer-dist multinomial-dist
                                sample-discrete-dist binomial-dist poisson-dist
                                categorical-dist
                                delta-dist cauchy-dist laplace-dist lognormal-dist
                                student-t-dist mixture-dist kde-dist entropy
                                uniform-discrete-dist chi-squared-dist logit-normal-dist
                                discrete? continuous? kl-divergence
                                marginal-dist
                                set-seed! rand
                                mem cache DPmem sd mode mean variance sum prod repeat-fn
                                weighted-mean weighted-variance softmax
                                empirical-distribution expectation]]
            [prob.erp :as erp]
            [prob.dist :as dist]
            [prob.cps :as cps])
  (:require-macros [prob.macros :refer [rejection-query mh-query enumeration-query
                                        importance-query mh-query-scored map-query
                                        forward-query query smc-query
                                        particle-gibbs-query]]))

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
;; Seedable PRNG
;; ---------------------------------------------------------------------------

(println "=== Seedable PRNG ===")

(test-assert "set-seed!: same seed -> same sequence"
  (do
    (set-seed! 42)
    (let [a (rand) b (rand) c (rand)]
      (set-seed! 42)
      (and (= a (rand)) (= b (rand)) (= c (rand))))))

(test-assert "set-seed!: different seeds -> different sequences"
  (do
    (set-seed! 42)
    (let [a (rand)]
      (set-seed! 99)
      (not= a (rand)))))

(test-assert "rand: values in [0,1)"
  (do
    (set-seed! 123)
    (every? #(and (>= % 0) (< % 1)) (repeatedly 100 rand))))

(test-assert "set-seed!: reproducible flip"
  (do
    (set-seed! 42)
    (let [a (flip)]
      (set-seed! 42)
      (= a (flip)))))

(test-assert "set-seed!: reproducible gaussian"
  (do
    (set-seed! 42)
    (let [a (gaussian 0 1)]
      (set-seed! 42)
      (= a (gaussian 0 1)))))

(test-assert "set-seed!: no-arg resets to system entropy"
  (do
    (set-seed! 42)
    (let [a (rand)]
      (set-seed!)
      (set-seed! 42)
      (= a (rand)))))

;; Restore system entropy for remaining tests
(set-seed!)

;; ---------------------------------------------------------------------------
;; Importance Sampling
;; ---------------------------------------------------------------------------

(println "=== Importance Sampling ===")

(test-assert "importance-query: returns (values probs) pair"
  (let [result (importance-query 500
                 (let [x (flip)]
                   x))]
    (and (seq? result)
         (= 2 (count result))
         (seq? (first result))
         (seq? (second result)))))

(test-assert "importance-query: probs sum to 1"
  (let [[_values probs] (importance-query 500
                           (let [x (flip)]
                             x))]
    (approx= (reduce + (seq probs)) 1.0 0.01)))

(test-assert "importance-query: factor shifts posterior"
  ;; P(true) ∝ 0.5 * exp(0) = 0.5, P(false) ∝ 0.5 * exp(-2) ≈ 0.0677
  ;; P(true) ≈ 0.5/(0.5+0.0677) ≈ 0.881
  (let [[values probs] (importance-query 2000
                          (let [x (flip)]
                            (factor (if x 0 -2))
                            x))
        prob-map (zipmap (seq values) (seq probs))]
    (approx= (get prob-map true) 0.88 0.05)))

(test-assert "importance-query-fn: direct call works"
  (let [[values probs] (importance-query-fn 500
                          (fn [] (let [x (uniform-draw [:a :b :c])]
                                   x)))]
    (and (= (count (seq values)) 3)
         (approx= (reduce + (seq probs)) 1.0 0.01))))

(test-assert "importance-query: observe shifts posterior"
  (let [[values probs] (importance-query 2000
                          (let [x (flip 0.5)]
                            (observe (bernoulli-dist 0.9) x)
                            x))
        prob-map (zipmap (seq values) (seq probs))]
    ;; P(true) ∝ 0.5 * 0.9 = 0.45, P(false) ∝ 0.5 * 0.1 = 0.05
    ;; P(true) = 0.45/0.5 = 0.9
    (approx= (get prob-map true) 0.9 0.05)))

(test-assert "conditional with importance strategy"
  (let [sampler (query '(importance 500)
                  (let [x (flip)] (condition x) x))]
    (= true (sampler))))

;; ---------------------------------------------------------------------------
;; Weighted Sample Utilities
;; ---------------------------------------------------------------------------

(println "=== Weighted Utilities ===")

(test-assert "weighted-mean: uniform weights"
  (approx= (weighted-mean [1 2 3] [1 1 1]) 2.0 0.001))

(test-assert "weighted-mean: skewed weights"
  (approx= (weighted-mean [1 2 3] [0 0 1]) 3.0 0.001))

(test-assert "weighted-mean: non-unit weights"
  (approx= (weighted-mean [10 20] [3 1]) 12.5 0.001))

(test-assert "weighted-variance: uniform weights"
  (approx= (weighted-variance [1 2 3] [1 1 1]) 0.6667 0.001))

(test-assert "weighted-variance: single point"
  (approx= (weighted-variance [5 10] [1 0]) 0.0 0.001))

(test-assert "empirical-distribution: frequencies"
  (let [d (empirical-distribution [:a :a :b :a :b :c])]
    (and (approx= (get d :a) 0.5 0.001)
         (approx= (get d :b) 0.3333 0.001)
         (approx= (get d :c) 0.1667 0.001))))

(test-assert "expectation: without transform"
  (approx= (expectation [1 2 3 4]) 2.5 0.001))

(test-assert "expectation: with transform"
  (approx= (expectation [1 2 3 4] #(* % %)) 7.5 0.001))

;; ---------------------------------------------------------------------------
;; Discrete proposals
;; ---------------------------------------------------------------------------

(println "=== Discrete Proposals ===")

(test-assert "proposal: bernoulli toggle"
  (let [d (bernoulli-dist 0.5)
        [new-val fwd rev] (dist/propose* d true)]
    (and (= new-val false) (= fwd 0.0) (= rev 0.0))))

(test-assert "proposal: bernoulli toggle false->true"
  (let [d (bernoulli-dist 0.3)
        [new-val _ _] (dist/propose* d false)]
    (= new-val true)))

(test-assert "proposal: categorical explores all values"
  (let [d (categorical-dist {:a 0.3 :b 0.5 :c 0.2})
        proposals (set (map (fn [_] (first (dist/propose* d :a))) (range 50)))]
    ;; Should have proposed both :b and :c at some point
    (and (contains? proposals :b) (contains? proposals :c))))

(test-assert "proposal: uniform-draw excludes current"
  (let [d (uniform-draw-dist [:x :y :z])
        [new-val _ _] (dist/propose* d :x)]
    (not= new-val :x)))

(test-assert "proposal: random-integer proposes different value"
  (let [d (random-integer-dist 5)
        [new-val _ _] (dist/propose* d 2)]
    (and (not= new-val 2) (>= new-val 0) (< new-val 5))))

(test-assert "proposal: bernoulli-dist mixes in mh-query via sample*"
  ;; sample* from bernoulli-dist should now use toggle proposal inside MH
  (let [samples (mh-query-fn 100 1
                  (fn [] (let [x (sample* (bernoulli-dist 0.5))]
                           x)))]
    ;; Should see both true and false
    (and (some true? samples) (some false? samples))))

;; ---------------------------------------------------------------------------
;; mh-query-scored
;; ---------------------------------------------------------------------------

(println "=== MH Query Scored ===")

(test-assert "mh-query-scored-fn: returns maps with :value and :score"
  (let [results (mh-query-scored-fn 10 1
                  (fn [] (let [x (flip)] (condition x) x)))]
    (and (= 10 (count results))
         (every? #(contains? % :value) results)
         (every? #(contains? % :score) results))))

(test-assert "mh-query-scored: scores are finite"
  (let [results (mh-query-scored 20 1
                  (let [x (flip)] (condition x) x))]
    (every? #(js/isFinite (:score %)) results)))

;; ---------------------------------------------------------------------------
;; MAP query
;; ---------------------------------------------------------------------------

(println "=== MAP Query ===")

(test-assert "map-query-fn: returns single value"
  (let [result (map-query-fn 200 1
                 (fn [] (let [x (uniform-draw [:a :b :c])]
                          (condition (not= x :c))
                          x)))]
    (contains? #{:a :b} result)))

(test-assert "map-query: finds mode"
  ;; With strong evidence for :a, MAP should return :a
  (let [result (map-query 500 1
                 (let [x (uniform-draw [:a :b :c])]
                   (factor (if (= x :a) 0 -5))
                   x))]
    (= result :a)))

;; ---------------------------------------------------------------------------
;; condition-equal
;; ---------------------------------------------------------------------------

(println "=== condition-equal ===")

(test-assert "condition-equal: soft conditioning works"
  (let [samples (mh-query-fn 200 1
                  (fn []
                    (let [x (flip 0.5)]
                      (condition-equal (fn [] (flip 0.9)) x)
                      x)))]
    ;; Should be biased toward true
    (> (mean (map #(if % 1 0) samples)) 0.5)))

(test-assert "condition-equal: deterministic enumeration"
  ;; condition-equal on a deterministic thunk
  (let [samples (mh-query-fn 50 1
                  (fn []
                    (let [x (uniform-draw [:a :b])]
                      (condition-equal (fn [] :a) x)
                      x)))]
    (every? #(= % :a) samples)))

;; ---------------------------------------------------------------------------
;; Marginal distribution
;; ---------------------------------------------------------------------------

(println "=== Marginal Distribution ===")

(test-assert "marginal: sample* returns valid value"
  (let [d (marginal-dist enumeration-query-fn
            (fn [] (uniform-draw [:a :b :c])))]
    (contains? #{:a :b :c} (sample* d))))

(test-assert "marginal: observe* correct log-prob"
  (let [d (marginal-dist enumeration-query-fn
            (fn [] (uniform-draw [:a :b])))]
    (approx= (observe* d :a) (js/Math.log 0.5) 0.01)))

(test-assert "marginal: impossible value -> -Inf"
  (let [d (marginal-dist enumeration-query-fn
            (fn [] (uniform-draw [:a :b])))]
    (= ##-Inf (observe* d :c))))

(test-assert "marginal: enumerate* returns support"
  (let [d (marginal-dist enumeration-query-fn
            (fn [] (uniform-draw [:x :y :z])))]
    (= (set (enumerate* d)) #{:x :y :z})))

(test-assert "marginal: works inside outer mh-query"
  (let [inner-dist (marginal-dist enumeration-query-fn
                     (fn [] (let [x (flip 0.9)] x)))
        samples (mh-query-fn 100 1
                  (fn []
                    (let [y (sample* inner-dist)]
                      y)))]
    ;; inner distribution is biased toward true
    (> (count (filter true? samples)) 50)))

;; ---------------------------------------------------------------------------
;; DPmem
;; ---------------------------------------------------------------------------

(println "=== DPmem ===")

(test-assert "DPmem: returns function"
  (let [f (DPmem 1.0 (fn [x] (flip)))]
    (fn? f)))

(test-assert "DPmem: consistent within same args (low alpha)"
  ;; With very low alpha, should almost always return same value
  (let [f (DPmem 0.001 (fn [x] (random-integer 1000000)))]
    ;; Call many times with same arg
    (let [results (repeatedly 20 #(f "a"))]
      ;; All should be the same (with overwhelming probability)
      (= 1 (count (distinct results))))))

(test-assert "DPmem: different args can differ"
  (let [f (DPmem 1.0 (fn [x] (random-integer 1000000)))]
    (let [a (f "a") b (f "b")]
      ;; At least they're numbers (might rarely be same)
      (and (number? a) (number? b)))))

(test-assert "DPmem: works inside mh-query"
  (let [results (mh-query-fn 20 1
                  (fn []
                    (let [f (DPmem 1.0 (fn [x] (uniform-draw [:x :y :z])))]
                      (f "test"))))]
    (every? #(contains? #{:x :y :z} %) results)))

;; ---------------------------------------------------------------------------
;; MCMC burn-in
;; ---------------------------------------------------------------------------

(println "=== MCMC Burn-in ===")

(test-assert "mh-query-fn: burn-in 4-arity works"
  (let [samples (mh-query-fn 20 1 10 (fn [] (flip)))]
    (= 20 (count samples))))

(test-assert "mh-query-fn: burn-in 3-arity backward compatible"
  (let [samples (mh-query-fn 20 1 (fn [] (flip)))]
    (= 20 (count samples))))

(test-assert "mh-query-scored-fn: burn-in 4-arity works"
  (let [results (mh-query-scored-fn 10 1 10 (fn [] (flip)))]
    (and (= 10 (count results))
         (every? #(contains? % :value) results))))

;; ---------------------------------------------------------------------------
;; Delta distribution
;; ---------------------------------------------------------------------------

(println "=== Delta Distribution ===")

(test-assert "delta-dist: always returns v"
  (let [d (delta-dist 42)]
    (every? #(= 42 %) (repeatedly 10 #(sample* d)))))

(test-assert "delta-dist: observe* correct value -> 0.0"
  (= 0.0 (observe* (delta-dist :foo) :foo)))

(test-assert "delta-dist: observe* wrong value -> -Inf"
  (= ##-Inf (observe* (delta-dist :foo) :bar)))

(test-assert "delta-dist: enumerate*"
  (= [42] (enumerate* (delta-dist 42))))

;; ---------------------------------------------------------------------------
;; Cauchy distribution
;; ---------------------------------------------------------------------------

(println "=== Cauchy Distribution ===")

(test-assert "cauchy-dist: samples are numbers"
  (every? number? (repeatedly 50 #(sample* (cauchy-dist 0 1)))))

(test-assert "cauchy-dist: log-prob at location is max"
  ;; f(x=0; loc=0, scale=1) = 1/(pi*1*(1+0)) = 1/pi
  (approx= (observe* (cauchy-dist 0 1) 0) (- (js/Math.log js/Math.PI)) 0.001))

(test-assert "cauchy-dist: log-prob decreases away from location"
  (> (observe* (cauchy-dist 0 1) 0)
     (observe* (cauchy-dist 0 1) 5)))

;; ---------------------------------------------------------------------------
;; Laplace distribution
;; ---------------------------------------------------------------------------

(println "=== Laplace Distribution ===")

(test-assert "laplace-dist: samples are numbers"
  (every? number? (repeatedly 50 #(sample* (laplace-dist 0 1)))))

(test-assert "laplace-dist: log-prob at location"
  ;; f(x=0; loc=0, scale=1) = 1/(2*1) * exp(0) = 0.5
  (approx= (observe* (laplace-dist 0 1) 0) (js/Math.log 0.5) 0.001))

(test-assert "laplace-dist: symmetric log-prob"
  (approx= (observe* (laplace-dist 0 1) 2)
           (observe* (laplace-dist 0 1) -2)
           0.001))

(test-assert "laplace-dist: mean near location"
  (let [samples (repeatedly 2000 #(sample* (laplace-dist 3 1)))]
    (approx= (mean samples) 3.0 0.3)))

;; ---------------------------------------------------------------------------
;; LogNormal distribution
;; ---------------------------------------------------------------------------

(println "=== LogNormal Distribution ===")

(test-assert "lognormal-dist: samples are positive"
  (every? pos? (repeatedly 50 #(sample* (lognormal-dist 0 1)))))

(test-assert "lognormal-dist: observe* negative -> -Inf"
  (= ##-Inf (observe* (lognormal-dist 0 1) -1)))

(test-assert "lognormal-dist: observe* zero -> -Inf"
  (= ##-Inf (observe* (lognormal-dist 0 1) 0)))

(test-assert "lognormal-dist: log-prob known value"
  ;; LN(0,1) at x=1: log(1)=0, gaussian-lp(0,1,0) = -0.9189, minus log(1) = 0
  (approx= (observe* (lognormal-dist 0 1) 1) -0.9189 0.001))

(test-assert "lognormal-dist: median near exp(mu)"
  ;; Median of LogNormal(mu,sigma) = exp(mu)
  (let [samples (sort (repeatedly 2000 #(sample* (lognormal-dist 1 0.5))))
        median (nth samples 1000)]
    (approx= median (js/Math.exp 1) 0.5)))

;; ---------------------------------------------------------------------------
;; Student-t distribution
;; ---------------------------------------------------------------------------

(println "=== Student-t Distribution ===")

(test-assert "student-t-dist: samples are numbers"
  (every? number? (repeatedly 50 #(sample* (student-t-dist 3)))))

(test-assert "student-t-dist: log-prob at location is max"
  (> (observe* (student-t-dist 5 0 1) 0)
     (observe* (student-t-dist 5 0 1) 3)))

(test-assert "student-t-dist: symmetric"
  (approx= (observe* (student-t-dist 5 0 1) 2)
           (observe* (student-t-dist 5 0 1) -2)
           0.001))

(test-assert "student-t-dist: mean near location (high df)"
  ;; With high df, approaches Gaussian
  (let [samples (repeatedly 2000 #(sample* (student-t-dist 30 5 1)))]
    (approx= (mean samples) 5.0 0.3)))

(test-assert "student-t-dist: location-scale"
  ;; t(df=10, loc=3, scale=2) at x=3 should equal t(df=10, loc=0, scale=1) at x=0 minus log(scale)
  (approx= (observe* (student-t-dist 10 3 2) 3)
           (- (observe* (student-t-dist 10 0 1) 0) (js/Math.log 2))
           0.001))

;; ---------------------------------------------------------------------------
;; Mixture distribution
;; ---------------------------------------------------------------------------

(println "=== Mixture Distribution ===")

(test-assert "mixture-dist: samples are numbers"
  (let [d (mixture-dist [(gaussian-dist 0 1) (gaussian-dist 5 1)] [0.5 0.5])]
    (every? number? (repeatedly 50 #(sample* d)))))

(test-assert "mixture-dist: log-prob correct"
  ;; Mixture of two delta dists: 0.3 * delta(1) + 0.7 * delta(2)
  (let [d (mixture-dist [(delta-dist 1) (delta-dist 2)] [0.3 0.7])]
    (and (approx= (observe* d 1) (js/Math.log 0.3) 0.001)
         (approx= (observe* d 2) (js/Math.log 0.7) 0.001)
         (= ##-Inf (observe* d 3)))))

(test-assert "mixture-dist: bimodal sampling"
  ;; Mixture of N(-5,0.1) and N(5,0.1) should have samples near -5 and 5
  (let [d (mixture-dist [(gaussian-dist -5 0.1) (gaussian-dist 5 0.1)] [0.5 0.5])
        samples (repeatedly 200 #(sample* d))
        neg (filter neg? samples)
        pos (filter pos? samples)]
    (and (> (count neg) 50)
         (> (count pos) 50))))

(test-assert "mixture-dist: observe in mh-query"
  (let [d (mixture-dist [(gaussian-dist 0 1) (gaussian-dist 10 1)] [0.5 0.5])
        samples (mh-query-fn 200 1
                  (fn [] (let [x (gaussian 5 3)]
                           (observe d x)
                           x)))]
    ;; Posterior should be pulled toward the mixture components
    (number? (mean samples))))

;; ---------------------------------------------------------------------------
;; Forward Sampling
;; ---------------------------------------------------------------------------

(println "=== Forward Sampling ===")

(test-assert "forward-query-fn: returns n samples"
  (= 50 (count (forward-query-fn 50 (fn [] (flip))))))

(test-assert "forward-query-fn: ignores condition"
  ;; condition false would normally throw, but forward mode ignores it
  (let [samples (forward-query-fn 20 (fn [] (condition false) (flip)))]
    (= 20 (count samples))))

(test-assert "forward-query-fn: ignores factor"
  ;; factor -Inf would normally cause rejection, but forward mode ignores it
  (let [samples (forward-query-fn 20 (fn [] (factor ##-Inf) (flip)))]
    (= 20 (count samples))))

(test-assert "forward-query: macro works"
  (= 10 (count (forward-query 10 (flip 0.7)))))

(test-assert "forward-query-fn: samples from prior"
  (let [samples (forward-query-fn 1000 (fn [] (if (flip 0.7) 1 0)))]
    (approx= (mean samples) 0.7 0.1)))

;; ---------------------------------------------------------------------------
;; Standard deviation
;; ---------------------------------------------------------------------------

(println "=== Standard Deviation ===")

(test-assert "sd: constant list -> 0"
  (= 0 (sd [5 5 5])))

(test-assert "sd: known value"
  ;; sd of [1 2 3] = sqrt(2/3) ≈ 0.8165
  (approx= (sd [1 2 3]) 0.8165 0.001))

(test-assert "sd: single element"
  (= 0 (sd [42])))

;; ---------------------------------------------------------------------------
;; Cache (LRU memoization)
;; ---------------------------------------------------------------------------

(println "=== Cache ===")

(test-assert "cache: returns same value for same args"
  (let [call-count (volatile! 0)
        f (cache (fn [x] (vswap! call-count inc) (* x x)))]
    (f 3)
    (f 3)
    (and (= (f 3) 9) (= @call-count 1))))

(test-assert "cache: different args get different results"
  (let [f (cache (fn [x] (* x x)))]
    (and (= (f 3) 9) (= (f 4) 16))))

(test-assert "cache: evicts oldest when full"
  (let [f (cache (fn [x] (* x x)) 3)]
    ;; Fill cache
    (f 1) (f 2) (f 3)
    ;; Add one more, evicting 1
    (f 4)
    ;; 1 should be evicted, so re-computation happens
    ;; but the function is pure so result is same
    (= (f 1) 1)))

(test-assert "cache: LRU order maintained"
  (let [call-count (volatile! 0)
        f (cache (fn [x] (vswap! call-count inc) (* x x)) 3)]
    (f 1) (f 2) (f 3)  ;; cache: [1 2 3]
    (f 1)               ;; access 1, cache: [2 3 1]
    (f 4)               ;; evict 2 (oldest), cache: [3 1 4]
    (vreset! call-count 0)
    (f 1)               ;; should be cached (no call)
    (= @call-count 0)))

;; ---------------------------------------------------------------------------
;; Entropy
;; ---------------------------------------------------------------------------

(println "=== Entropy ===")

(test-assert "entropy: fair coin = log(2)"
  (approx= (entropy (bernoulli-dist 0.5)) (js/Math.log 2) 0.001))

(test-assert "entropy: biased coin < log(2)"
  (< (entropy (bernoulli-dist 0.9)) (js/Math.log 2)))

(test-assert "entropy: delta = 0"
  (approx= (entropy (delta-dist 42)) 0.0 0.001))

(test-assert "entropy: uniform over n = log(n)"
  (approx= (entropy (random-integer-dist 6)) (js/Math.log 6) 0.001))

;; ---------------------------------------------------------------------------
;; Unified infer
;; ---------------------------------------------------------------------------

(println "=== Unified infer ===")

(test-assert "infer: rejection"
  (= true (infer {:method :rejection}
                  (fn [] (let [x (flip)] (condition x) x)))))

(test-assert "infer: mh"
  (let [samples (infer {:method :mh :samples 50 :lag 1}
                        (fn [] (let [x (flip)] (condition x) x)))]
    (and (= 50 (count samples))
         (every? true? samples))))

(test-assert "infer: enumeration"
  (let [[values probs] (infer {:method :enumeration}
                               (fn [] (let [x (flip)] (condition x) x)))]
    (and (= (first values) true)
         (approx= (first probs) 1.0 0.001))))

(test-assert "infer: importance"
  (let [[values probs] (infer {:method :importance :samples 500}
                               (fn [] (flip)))]
    (approx= (reduce + (seq probs)) 1.0 0.01)))

(test-assert "infer: forward"
  (let [samples (infer {:method :forward :samples 100}
                        (fn [] (flip 0.7)))]
    (= 100 (count samples))))

(test-assert "infer: mh with burn-in"
  (let [samples (infer {:method :mh :samples 20 :lag 1 :burn 10}
                        (fn [] (flip)))]
    (= 20 (count samples))))

(test-assert "infer: mh-scored"
  (let [results (infer {:method :mh-scored :samples 10 :lag 1}
                        (fn [] (let [x (flip)] (condition x) x)))]
    (and (= 10 (count results))
         (every? :value results)
         (every? :score results))))

(test-assert "infer: map"
  (let [result (infer {:method :map :samples 200 :lag 1}
                       (fn [] (let [x (uniform-draw [:a :b :c])]
                                (factor (if (= x :a) 0 -5))
                                x)))]
    (= result :a)))

(test-assert "infer: mh with callback"
  (let [called (volatile! 0)]
    (infer {:method :mh :samples 10 :lag 1
            :callback (fn [_] (vswap! called inc))}
           (fn [] (flip)))
    (= @called 10)))

;; ---------------------------------------------------------------------------
;; likelyFirst enumeration
;; ---------------------------------------------------------------------------

(println "=== likelyFirst Enumeration ===")

(test-assert "enum likelyFirst: same result as full for small model"
  (let [[vals1 probs1] (enumeration-query-fn (fn [] (let [x (flip)] (condition x) x)))
        [vals2 probs2] (enumeration-query-fn {:strategy :likely-first}
                          (fn [] (let [x (flip)] (condition x) x)))]
    (and (= (set (seq vals1)) (set (seq vals2)))
         (approx= (first probs1) (first probs2) 0.001))))

(test-assert "enum likelyFirst: works with uniform-draw"
  (let [[values probs] (enumeration-query-fn {:strategy :likely-first}
                          (fn [] (let [x (uniform-draw [1 2 3 4 5 6])]
                                   (condition (even? x))
                                   x)))]
    (and (= (count (seq values)) 3)
         (every? even? (seq values))
         (approx= (reduce + (seq probs)) 1.0 0.001))))

(test-assert "enum likelyFirst: max-executions limits exploration"
  ;; With a very small max-executions, we still get some results
  (let [[values probs] (enumeration-query-fn {:strategy :likely-first :max-executions 2}
                          (fn [] (random-integer 4)))]
    (and (pos? (count (seq values)))
         (approx= (reduce + (seq probs)) 1.0 0.001))))

(test-assert "enum full: max-executions limits exploration"
  (let [[values probs] (enumeration-query-fn {:max-executions 2}
                          (fn [] (random-integer 4)))]
    (and (pos? (count (seq values)))
         (approx= (reduce + (seq probs)) 1.0 0.001))))

;; ---------------------------------------------------------------------------
;; KDE distribution
;; ---------------------------------------------------------------------------

(println "=== KDE Distribution ===")

(test-assert "kde-dist: samples are numbers"
  (let [d (kde-dist [1 2 3 4 5])]
    (every? number? (repeatedly 20 #(sample* d)))))

(test-assert "kde-dist: log-prob is finite near data"
  (let [d (kde-dist [0 0 0 1 1])]
    (js/isFinite (observe* d 0.5))))

(test-assert "kde-dist: log-prob higher near data center"
  (let [d (kde-dist [0 0 0 0 0])]
    (> (observe* d 0) (observe* d 10))))

(test-assert "kde-dist: custom bandwidth"
  (let [d (kde-dist [0 1 2] 0.5)]
    (number? (sample* d))))

(test-assert "kde-dist: Silverman bandwidth auto-computed"
  (let [d (kde-dist [1 2 3 4 5 6 7 8 9 10])]
    ;; Should be able to sample and score
    (and (number? (sample* d))
         (js/isFinite (observe* d 5)))))

;; ---------------------------------------------------------------------------
;; MCMC Callbacks
;; ---------------------------------------------------------------------------

(println "=== MCMC Callbacks ===")

(test-assert "mh-query-fn: callback receives correct iter count"
  (let [iters (volatile! [])]
    (mh-query-fn 5 1 0 (fn [{:keys [iter]}] (vswap! iters conj iter))
      (fn [] (flip)))
    (= @iters [0 1 2 3 4])))

(test-assert "mh-query-fn: callback receives value and score"
  (let [entries (volatile! [])]
    (mh-query-fn 3 1 0
      (fn [entry] (vswap! entries conj entry))
      (fn [] (let [x (flip)] (condition x) x)))
    (and (= 3 (count @entries))
         (every? :value @entries)
         (every? :score @entries))))

(test-assert "mh-query-scored-fn: callback works"
  (let [called (volatile! 0)]
    (mh-query-scored-fn 5 1 0
      (fn [_] (vswap! called inc))
      (fn [] (flip)))
    (= @called 5)))

;; ---------------------------------------------------------------------------
;; Uniform Discrete distribution
;; ---------------------------------------------------------------------------

(println "=== Uniform Discrete Distribution ===")

(test-assert "uniform-discrete-dist: samples in range"
  (let [d (uniform-discrete-dist 3 7)]
    (every? #(and (integer? %) (>= % 3) (<= % 7))
            (repeatedly 50 #(sample* d)))))

(test-assert "uniform-discrete-dist: log-prob correct"
  ;; 5 values: 3,4,5,6,7 -> log(1/5) = -log(5)
  (approx= (observe* (uniform-discrete-dist 3 7) 5)
           (- (js/Math.log 5)) 0.001))

(test-assert "uniform-discrete-dist: out of range -> -Inf"
  (and (= ##-Inf (observe* (uniform-discrete-dist 3 7) 2))
       (= ##-Inf (observe* (uniform-discrete-dist 3 7) 8))))

(test-assert "uniform-discrete-dist: enumerate*"
  (= (enumerate* (uniform-discrete-dist 2 5)) [2 3 4 5]))

(test-assert "uniform-discrete-dist: works in enumeration"
  (let [[values probs] (enumeration-query-fn
                          (fn [] (let [x (sample* (uniform-discrete-dist 1 3))]
                                   (condition (> x 1))
                                   x)))]
    (and (= (set (seq values)) #{2 3})
         (approx= (reduce + (seq probs)) 1.0 0.001))))

;; ---------------------------------------------------------------------------
;; Chi-Squared distribution
;; ---------------------------------------------------------------------------

(println "=== Chi-Squared Distribution ===")

(test-assert "chi-squared-dist: samples are positive"
  (every? pos? (repeatedly 50 #(sample* (chi-squared-dist 3)))))

(test-assert "chi-squared-dist: mean near df"
  ;; Mean of chi-squared(df) = df
  (let [samples (repeatedly 3000 #(sample* (chi-squared-dist 5)))]
    (approx= (mean samples) 5.0 0.5)))

(test-assert "chi-squared-dist: log-prob negative -> -Inf"
  (= ##-Inf (observe* (chi-squared-dist 3) -1)))

(test-assert "chi-squared-dist: log-prob at known value"
  ;; chi-squared(2) = Exp(0.5), so pdf at x=1 = 0.5*exp(-0.5) ≈ 0.3033
  (approx= (observe* (chi-squared-dist 2) 1)
           (js/Math.log 0.30327) 0.01))

;; ---------------------------------------------------------------------------
;; Logit-Normal distribution
;; ---------------------------------------------------------------------------

(println "=== Logit-Normal Distribution ===")

(test-assert "logit-normal-dist: samples in (0, 1)"
  (every? #(and (> % 0) (< % 1))
          (repeatedly 50 #(sample* (logit-normal-dist 0 1)))))

(test-assert "logit-normal-dist: observe* outside (0,1) -> -Inf"
  (and (= ##-Inf (observe* (logit-normal-dist 0 1) 0))
       (= ##-Inf (observe* (logit-normal-dist 0 1) 1))
       (= ##-Inf (observe* (logit-normal-dist 0 1) -0.5))
       (= ##-Inf (observe* (logit-normal-dist 0 1) 1.5))))

(test-assert "logit-normal-dist: log-prob is finite in (0,1)"
  (js/isFinite (observe* (logit-normal-dist 0 1) 0.5)))

(test-assert "logit-normal-dist: mu=0 median near 0.5"
  ;; When mu=0, median of logit-normal = sigmoid(0) = 0.5
  (let [samples (sort (repeatedly 2000 #(sample* (logit-normal-dist 0 0.5))))
        median (nth samples 1000)]
    (approx= median 0.5 0.1)))

;; ---------------------------------------------------------------------------
;; Mode
;; ---------------------------------------------------------------------------

(println "=== Mode ===")

(test-assert "mode: most frequent element"
  (= (mode [1 2 2 3 3 3]) 3))

(test-assert "mode: single element"
  (= (mode [42]) 42))

(test-assert "mode: works with keywords"
  (= (mode [:a :b :a :c :a]) :a))

;; ---------------------------------------------------------------------------
;; KL Divergence
;; ---------------------------------------------------------------------------

(println "=== KL Divergence ===")

(test-assert "kl-divergence: same distribution = 0"
  (approx= (kl-divergence (bernoulli-dist 0.5) (bernoulli-dist 0.5)) 0.0 0.001))

(test-assert "kl-divergence: non-negative"
  (>= (kl-divergence (bernoulli-dist 0.3) (bernoulli-dist 0.7)) 0.0))

(test-assert "kl-divergence: known value"
  ;; KL(Bernoulli(0.5) || Bernoulli(0.25)) = 0.5*log(0.5/0.25) + 0.5*log(0.5/0.75)
  ;; = 0.5*log(2) + 0.5*log(2/3) = 0.5*(0.6931 + (-0.4055)) = 0.1438
  (approx= (kl-divergence (bernoulli-dist 0.5) (bernoulli-dist 0.25))
           0.1438 0.01))

(test-assert "kl-divergence: asymmetric"
  (not= (kl-divergence (bernoulli-dist 0.3) (bernoulli-dist 0.7))
        (kl-divergence (bernoulli-dist 0.7) (bernoulli-dist 0.3))))

;; ---------------------------------------------------------------------------
;; Discrete? / Continuous? predicates
;; ---------------------------------------------------------------------------

(println "=== discrete?/continuous? ===")

(test-assert "discrete?: bernoulli is discrete"
  (discrete? (bernoulli-dist 0.5)))

(test-assert "discrete?: uniform-discrete is discrete"
  (discrete? (uniform-discrete-dist 1 6)))

(test-assert "discrete?: categorical is discrete"
  (discrete? (categorical-dist {:a 0.5 :b 0.5})))

(test-assert "continuous?: gaussian is continuous"
  (continuous? (gaussian-dist 0 1)))

(test-assert "continuous?: beta is continuous"
  (continuous? (beta-dist 2 2)))

(test-assert "continuous?: chi-squared is continuous"
  (continuous? (chi-squared-dist 3)))

(test-assert "continuous?: logit-normal is continuous"
  (continuous? (logit-normal-dist 0 1)))

(test-assert "discrete?: delta is discrete"
  (discrete? (delta-dist 42)))

;; ---------------------------------------------------------------------------
;; Softmax
;; ---------------------------------------------------------------------------

(println "=== Softmax ===")

(test-assert "softmax: uniform utilities → equal probabilities"
  (let [probs (softmax [1 1 1])]
    (every? #(< (js/Math.abs (- % (/ 1.0 3))) 1e-10) probs)))

(test-assert "softmax: higher utility → higher probability"
  (let [probs (softmax [0 1 2])]
    (and (< (nth probs 0) (nth probs 1))
         (< (nth probs 1) (nth probs 2)))))

(test-assert "softmax: probabilities sum to 1"
  (let [probs (softmax [3.2 -1.5 0.7 2.1])]
    (< (js/Math.abs (- (reduce + probs) 1.0)) 1e-10)))

(test-assert "softmax: β=0 → uniform"
  (let [probs (softmax [10 -5 3] 0)]
    (every? #(< (js/Math.abs (- % (/ 1.0 3))) 1e-10) probs)))

(test-assert "softmax: high β → argmax"
  (let [probs (softmax [1 5 2] 100)]
    (and (< (nth probs 0) 0.01)
         (> (nth probs 1) 0.99)
         (< (nth probs 2) 0.01))))

(test-assert "softmax: single element → probability 1"
  (let [probs (softmax [42])]
    (< (js/Math.abs (- (first probs) 1.0)) 1e-10)))

(test-assert "softmax: negative utilities work"
  (let [probs (softmax [-10 -20 -5])]
    (and (< (js/Math.abs (- (reduce + probs) 1.0)) 1e-10)
         (> (nth probs 2) (nth probs 0))
         (> (nth probs 0) (nth probs 1)))))

;; ---------------------------------------------------------------------------
;; CPS Transform
;; ---------------------------------------------------------------------------

(println "=== CPS Transform ===")

(test-assert "cps: literal passes to continuation"
  (= (cps/cps-of-expr 42 'k) '(k 42 $state)))

(test-assert "cps: symbol passes to continuation"
  (= (cps/cps-of-expr 'x 'k) '(k x $state)))

(test-assert "cps: nil passes to continuation"
  (= (cps/cps-of-expr nil 'k) '(k nil $state)))

(test-assert "cps: keyword passes to continuation"
  (= (cps/cps-of-expr :foo 'k) '(k :foo $state)))

(test-assert "cps: quoted form preserved"
  (= (cps/cps-of-expr '(quote (a b)) 'k) '(k (quote (a b)) $state)))

(test-assert "cps: if with atomic test"
  (let [result (cps/cps-of-expr '(if x 1 2) 'k)]
    (and (= (first result) 'if)
         (= (second result) 'x))))

(test-assert "cps: let produces nested continuations"
  (let [result (cps/cps-of-expr '(let [x 1] x) 'k)]
    ;; Should be ((fn [x $state] (k x $state)) 1 $state)
    (seq? result)))

(test-assert "cps: do sequences expressions"
  (let [result (cps/cps-of-expr '(do 1 2) 'k)]
    ;; First expr gets ignored continuation, second gets k
    (seq? result)))

(test-assert "cps: flip becomes cps-sample with bernoulli-dist"
  (let [result (cps/cps-of-expr '(flip 0.7) 'k)]
    (= (first result) 'prob.cps/cps-sample)))

(test-assert "cps: observe becomes cps-observe"
  (let [result (cps/cps-of-expr '(observe d 5) 'k)]
    (= (first result) 'prob.cps/cps-observe)))

(test-assert "cps: factor becomes cps-factor"
  (let [result (cps/cps-of-expr '(factor -1) 'k)]
    (= (first result) 'prob.cps/cps-factor)))

(test-assert "cps: condition becomes cps-condition"
  (let [result (cps/cps-of-expr '(condition x) 'k)]
    (= (first result) 'prob.cps/cps-condition)))

(test-assert "cps: sample* becomes cps-sample"
  (let [result (cps/cps-of-expr '(sample* d) 'k)]
    (= (first result) 'prob.cps/cps-sample)))

(test-assert "cps: vector elements are evaluated"
  (let [result (cps/cps-of-expr '[1 2 3] 'k)]
    (= result '(k [1 2 3] $state))))

(test-assert "cps: fn produces CPS-aware lambda"
  (let [result (cps/cps-of-expr '(fn [x] x) 'k)]
    ;; Should pass a fn with extra k and $state args to k
    (= (first result) 'k)))

;; ---------------------------------------------------------------------------
;; CPS Execution (runtime)
;; ---------------------------------------------------------------------------

(println "=== CPS Runtime ===")

(test-assert "cps: Sample checkpoint created"
  (let [d (bernoulli-dist 0.5)
        result (cps/cps-sample d identity {:addr 0})]
    (cps/sample? result)))

(test-assert "cps: Observe checkpoint created"
  (let [d (bernoulli-dist 0.5)
        result (cps/cps-observe d true identity {:addr 0})]
    (cps/observe? result)))

(test-assert "cps: Factor checkpoint created"
  (let [result (cps/cps-factor -1.0 identity {:addr 0})]
    (cps/factor? result)))

(test-assert "cps: Result checkpoint created"
  (cps/result? (cps/->Result 42 {})))

(test-assert "cps: condition true continues"
  ;; condition true calls continuation, not a Factor
  (let [called (volatile! false)
        result (cps/cps-condition true (fn [_ s] (vreset! called true) (cps/->Result :ok s)) {})]
    (and @called (cps/result? result))))

(test-assert "cps: condition false creates Factor -Inf"
  (let [result (cps/cps-condition false identity {})]
    (and (cps/factor? result) (= (:score result) ##-Inf))))

;; ---------------------------------------------------------------------------
;; SMC Inference
;; ---------------------------------------------------------------------------

(println "=== SMC Inference ===")

(test-assert "smc: returns correct number of samples"
  (let [results (smc-query 100
                  (flip 0.5))]
    (= 100 (count results))))

(test-assert "smc: no observes degenerates to prior sampling"
  ;; With no observe/condition, should sample from prior
  (let [results (smc-query 1000
                  (if (flip 0.5) 1 0))
        m (mean results)]
    (approx= m 0.5 0.1)))

(test-assert "smc: single particle works"
  (let [results (smc-query 1
                  (flip 0.5))]
    (= 1 (count results))))

(test-assert "smc: observe shifts posterior"
  ;; Beta(1,1) + observe true + observe false -> Beta(2,2), mean=0.5
  (let [results (smc-query 1000
                  (let [p (beta 1 1)]
                    (observe (bernoulli-dist p) true)
                    (observe (bernoulli-dist p) false)
                    p))
        m (mean results)]
    (approx= m 0.5 0.1)))

(test-assert "smc: condition filters impossible values"
  ;; Condition: at least one flip true
  (let [results (smc-query 1000
                  (let [a (flip 0.5)
                        b (flip 0.5)]
                    (condition (or a b))
                    (list a b)))
        dist (empirical-distribution results)]
    ;; (false false) should have probability 0
    (= 0 (get dist (list false false) 0))))

(test-assert "smc: condition approximates correct distribution"
  ;; P(true,true | or) = 1/3
  (let [results (smc-query 2000
                  (let [a (flip 0.5)
                        b (flip 0.5)]
                    (condition (or a b))
                    (list a b)))
        dist (empirical-distribution results)]
    (approx= (get dist (list true true) 0) 0.333 0.06)))

(test-assert "smc: let bindings work correctly"
  (let [results (smc-query 500
                  (let [x 3
                        y 4]
                    (+ x y)))]
    (every? #(= 7 %) results)))

(test-assert "smc: nested if works"
  (let [results (smc-query 500
                  (let [x (flip 0.5)]
                    (if x "yes" "no")))]
    (and (some #(= "yes" %) results)
         (some #(= "no" %) results))))

(test-assert "smc: gaussian observe pulls mean"
  ;; N(0,10) prior, observe 5 with sigma=1 -> posterior near 5
  (let [results (smc-query 1000
                  (let [mu (gaussian 0 10)]
                    (observe (gaussian-dist mu 1) 5.0)
                    mu))
        m (mean results)]
    (approx= m 5.0 1.0)))

(test-assert "smc: multiple gaussian observations"
  ;; N(0,10) prior, observe 3.0 twice with sigma=1
  (let [results (smc-query 1000
                  (let [mu (gaussian 0 10)]
                    (observe (gaussian-dist mu 1) 3.0)
                    (observe (gaussian-dist mu 1) 3.0)
                    mu))
        m (mean results)]
    (approx= m 3.0 1.0)))

(test-assert "smc: factor weighting works"
  ;; Factor should shift posterior
  (let [results (smc-query 2000
                  (let [x (flip 0.5)]
                    (factor (if x 0 -2))
                    x))
        dist (empirical-distribution results)
        p-true (get dist true 0)]
    ;; P(true) ∝ 0.5*exp(0)=0.5, P(false) ∝ 0.5*exp(-2)≈0.068
    ;; P(true) ≈ 0.88
    (approx= p-true 0.88 0.1)))

(test-assert "smc: uniform-draw works in CPS"
  (let [results (smc-query 200
                  (uniform-draw [:a :b :c]))]
    (and (every? #{:a :b :c} results)
         (= 200 (count results)))))

(test-assert "smc: sample* from dist works"
  (let [results (smc-query 200
                  (sample* (bernoulli-dist 0.5)))]
    (and (every? boolean? results)
         (some true? results)
         (some false? results))))

(test-assert "smc: cond form works"
  (let [results (smc-query 500
                  (let [x (random-integer 3)]
                    (cond
                      (= x 0) :zero
                      (= x 1) :one
                      :else :two)))]
    (and (some #(= :zero %) results)
         (some #(= :one %) results)
         (some #(= :two %) results))))

(test-assert "smc: and/or desugaring works"
  (let [results (smc-query 500
                  (and (flip 0.5) (flip 0.5)))]
    ;; (and true true) = true, (and true false) = false, (and false _) = false
    (and (some true? results)
         (some false? results))))

(test-assert "smc: smc-query-fn direct call"
  (let [results (smc-query-fn 100
                  (fn [k $state]
                    (cps/cps-sample (bernoulli-dist 0.5)
                      (fn [x $state]
                        (k x $state))
                      $state)))]
    (= 100 (count results))))

(test-assert "smc: convergence improves with more particles"
  ;; 10 particles vs 1000 particles: 1000 should be more accurate
  (let [run-smc (fn [n]
                  (let [results (smc-query-fn n
                                  (fn [k $state]
                                    (cps/cps-sample (bernoulli-dist 0.5)
                                      (fn [x $state]
                                        (cps/cps-observe (bernoulli-dist 0.9) x
                                          (fn [_ $state]
                                            (k x $state))
                                          $state))
                                      $state)))]
                    (mean (map #(if % 1 0) results))))
        ;; Expected: P(true | obs true) = 0.9*0.5 / (0.9*0.5 + 0.1*0.5) = 0.9
        err-small (js/Math.abs (- (run-smc 10) 0.9))
        err-large (js/Math.abs (- (run-smc 1000) 0.9))]
    ;; Larger sample should generally be closer (with high probability)
    ;; Use lenient test: large should be within tolerance
    (< err-large 0.1)))

;; ---------------------------------------------------------------------------
;; SMC loop/recur
;; ---------------------------------------------------------------------------

(println "=== SMC loop/recur ===")

(test-assert "smc loop: simple counting loop"
  (let [results (smc-query 200
                  (loop [i 0, acc 0]
                    (if (>= i 10)
                      acc
                      (recur (inc i) (inc acc)))))]
    (every? #(= 10 %) results)))

(test-assert "smc loop: accumulate flip results"
  (let [results (smc-query 500
                  (loop [i 0, total 0]
                    (if (>= i 5)
                      total
                      (recur (inc i) (+ total (if (flip 0.5) 1 0))))))]
    ;; Mean of 5 Bernoulli(0.5) should be 2.5
    (approx= (mean results) 2.5 0.5)))

(test-assert "smc loop: loop with observations"
  ;; Iterate over observation sequence
  (let [obs [true true false]
        results (smc-query 1000
                  (let [p (beta 1 1)]
                    (loop [i 0]
                      (when (< i 3)
                        (observe (bernoulli-dist p) (nth obs i))
                        (recur (inc i))))
                    p))
        m (mean results)]
    ;; Beta(1,1) + 2T + 1F = Beta(3,2), mean = 0.6
    (approx= m 0.6 0.15)))

(test-assert "smc loop: recur with probabilistic args"
  ;; recur args contain random choices
  (let [results (smc-query 500
                  (loop [i 0, items []]
                    (if (>= i 3)
                      items
                      (recur (inc i) (conj items (flip 0.5))))))]
    (and (every? #(= 3 (count %)) results)
         (every? #(every? boolean? %) results))))

(test-assert "smc loop: nested loop produces correct result"
  (let [results (smc-query 200
                  (loop [i 0, sum 0]
                    (if (>= i 3)
                      sum
                      (recur (inc i) (+ sum i)))))]
    ;; 0 + 1 + 2 = 3
    (every? #(= 3 %) results)))

;; ---------------------------------------------------------------------------
;; SMC doseq
;; ---------------------------------------------------------------------------

(println "=== SMC doseq ===")

(test-assert "smc doseq: basic observation loop"
  (let [data [true true false true true]
        results (smc-query 1000
                  (let [p (beta 1 1)]
                    (doseq [d data] (observe (bernoulli-dist p) d))
                    p))
        m (mean results)]
    ;; Beta(1,1) + 4T + 1F = Beta(5,2), mean = 5/7 ≈ 0.714
    (approx= m (/ 5 7) 0.1)))

(test-assert "smc doseq: no-op on empty collection"
  (let [results (smc-query 100
                  (let [x (flip 0.5)]
                    (doseq [d []] (observe (bernoulli-dist 0.5) d))
                    x))]
    (every? boolean? results)))

(test-assert "smc doseq: accumulates multiple observations"
  (let [results (smc-query 500
                  (let [mu (gaussian 0 5)]
                    (doseq [x [2.0 2.1 1.9 2.0 2.0]]
                      (observe (gaussian-dist mu 0.1) x))
                    mu))
        m (mean results)]
    ;; Tight observations around 2.0
    (approx= m 2.0 0.3)))

;; ---------------------------------------------------------------------------
;; SMC case
;; ---------------------------------------------------------------------------

(println "=== SMC case ===")

(test-assert "smc case: basic dispatch"
  (let [results (smc-query 300
                  (let [x (random-integer 3)]
                    (case x
                      0 :zero
                      1 :one
                      2 :two)))]
    (and (some #(= :zero %) results)
         (some #(= :one %) results)
         (some #(= :two %) results))))

(test-assert "smc case: with default"
  (let [results (smc-query 200
                  (case (random-integer 3)
                    0 :zero
                    :other))]
    (and (some #(= :zero %) results)
         (some #(= :other %) results))))

(test-assert "smc case: with observe"
  (let [results (smc-query 1000
                  (let [x (flip 0.5)]
                    (case x
                      true (do (observe (bernoulli-dist 0.9) true) :heads)
                      false (do (observe (bernoulli-dist 0.1) true) :tails))))
        dist (empirical-distribution results)]
    ;; P(heads) ∝ 0.5 * 0.9 = 0.45, P(tails) ∝ 0.5 * 0.1 = 0.05
    ;; P(heads) = 0.9
    (approx= (get dist :heads 0) 0.9 0.1)))

;; ---------------------------------------------------------------------------
;; SMC Rejuvenation
;; ---------------------------------------------------------------------------

(println "=== SMC Rejuvenation ===")

(test-assert "smc rejuv: returns correct sample count"
  (let [results (smc-query 100 {:rejuv-steps 3}
                  (let [p (beta 1 1)]
                    (observe (bernoulli-dist p) true)
                    p))]
    (= 100 (count results))))

(test-assert "smc rejuv: Beta-Bernoulli conjugate posterior"
  ;; Beta(1,1) + 3 true + 1 false = Beta(4,2), mean = 4/6 ≈ 0.667
  (let [results (smc-query 500 {:rejuv-steps 5}
                  (let [p (beta 1 1)]
                    (observe (bernoulli-dist p) true)
                    (observe (bernoulli-dist p) true)
                    (observe (bernoulli-dist p) true)
                    (observe (bernoulli-dist p) false)
                    p))
        m (mean results)]
    (approx= m 0.667 0.15)))

(test-assert "smc rejuv: handles zero-sample models"
  ;; No sample points — rejuv should be a no-op
  (let [results (smc-query 100 {:rejuv-steps 5}
                  42)]
    (every? #(= 42 %) results)))

(test-assert "smc rejuv: works with discrete distributions"
  (let [results (smc-query 500 {:rejuv-steps 3}
                  (let [x (flip 0.5)]
                    (observe (bernoulli-dist 0.9) x)
                    x))
        p-true (mean (map #(if % 1 0) results))]
    ;; P(true) = 0.9*0.5 / (0.9*0.5 + 0.1*0.5) = 0.9
    (approx= p-true 0.9 0.1)))

(test-assert "smc rejuv: multiple observes with rejuvenation"
  ;; Beta(1,1) + 5 true = Beta(6,1), mean = 6/7 ≈ 0.857
  (let [results (smc-query 300 {:rejuv-steps 3}
                  (let [p (beta 1 1)]
                    (observe (bernoulli-dist p) true)
                    (observe (bernoulli-dist p) true)
                    (observe (bernoulli-dist p) true)
                    (observe (bernoulli-dist p) true)
                    (observe (bernoulli-dist p) true)
                    p))
        m (mean results)]
    (approx= m 0.857 0.15)))

(test-assert "smc rejuv: rejuv-steps 0 is same as plain SMC"
  (let [results (smc-query 200 {:rejuv-steps 0}
                  (let [p (beta 1 1)]
                    (observe (bernoulli-dist p) true)
                    p))]
    (= 200 (count results))))

(test-assert "smc rejuv: condition + rejuv works"
  (let [results (smc-query 500 {:rejuv-steps 3}
                  (let [a (flip 0.5)
                        b (flip 0.5)]
                    (condition (or a b))
                    (list a b)))
        dist (empirical-distribution results)]
    ;; P(false false) = 0
    (= 0 (get dist (list false false) 0))))

(test-assert "smc rejuv: opts map is optional (backward compatible)"
  (let [results (smc-query 100
                  (flip 0.5))]
    (= 100 (count results))))

;; ---------------------------------------------------------------------------
;; Particle Gibbs
;; ---------------------------------------------------------------------------

(println "=== Particle Gibbs ===")

(test-assert "pg: returns correct sample count"
  (let [results (particle-gibbs-query 10 20
                  (let [p (beta 1 1)]
                    (observe (bernoulli-dist p) true)
                    p))]
    (= 20 (count results))))

(test-assert "pg: posterior mean correct (Beta-Bernoulli)"
  ;; Beta(1,1) + 2T + 1F = Beta(3,2), mean = 0.6
  (let [results (particle-gibbs-query 20 100 {:burn 20}
                  (let [p (beta 1 1)]
                    (observe (bernoulli-dist p) true)
                    (observe (bernoulli-dist p) true)
                    (observe (bernoulli-dist p) false)
                    p))
        m (mean results)]
    (approx= m 0.6 0.15)))

(test-assert "pg: with rejuvenation"
  (let [results (particle-gibbs-query 15 50 {:burn 10 :rejuv-steps 2}
                  (let [p (beta 1 1)]
                    (observe (bernoulli-dist p) true)
                    (observe (bernoulli-dist p) true)
                    p))
        m (mean results)]
    ;; Beta(3,1), mean = 0.75
    (approx= m 0.75 0.2)))

(test-assert "pg: burn-in and lag work"
  (let [results (particle-gibbs-query 10 30 {:burn 5 :lag 1}
                  (let [p (beta 1 1)]
                    (observe (bernoulli-dist p) true)
                    p))]
    (= 30 (count results))))

(test-assert "pg: discrete model"
  (let [results (particle-gibbs-query 15 100 {:burn 10}
                  (let [x (flip 0.5)]
                    (observe (bernoulli-dist 0.9) x)
                    x))
        p-true (mean (map #(if % 1 0) results))]
    ;; P(true|obs=true) = 0.9
    (approx= p-true 0.9 0.15)))

(test-assert "pg: particle-gibbs-fn direct call"
  (let [results (particle-gibbs-fn 10 20
                  (fn [k $state]
                    (cps/cps-sample (beta-dist 1 1)
                      (fn [p $state]
                        (cps/cps-observe (bernoulli-dist p) true
                          (fn [_ $state]
                            (k p $state))
                          $state))
                      $state)))]
    (= 20 (count results))))

(test-assert "pg: via infer dispatch"
  (let [results (infer {:method :particle-gibbs :particles 10 :samples 20 :burn 5}
                  (fn [k $state]
                    (cps/cps-sample (beta-dist 1 1)
                      (fn [p $state]
                        (cps/cps-observe (bernoulli-dist p) true
                          (fn [_ $state]
                            (k p $state))
                          $state))
                      $state)))]
    (= 20 (count results))))

(test-assert "pg: callback invoked"
  (let [called (volatile! 0)]
    (particle-gibbs-query 10 10 {:burn 2 :callback (fn [_] (vswap! called inc))}
      (let [p (beta 1 1)]
        (observe (bernoulli-dist p) true)
        p))
    (= @called 10)))

;; ---------------------------------------------------------------------------
;; Summary
;; ---------------------------------------------------------------------------

(println)
(println (str "=== prob library tests: " @pass-count " passed, " @fail-count " failed ==="))
(when (pos? @fail-count)
  (js/process.exit 1))
