(ns prob-dist-tests
  "Tests for prob.dist — distribution protocol, sampling, and log-probability scoring."
  (:require [prob.dist :refer [IDistribution sample* observe* dist?
                                bernoulli-dist gaussian-dist uniform-dist
                                beta-dist gamma-dist exponential-dist
                                dirichlet-dist uniform-draw-dist
                                random-integer-dist multinomial-dist
                                sample-discrete-dist]]))

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
;; Protocol / predicate
;; ---------------------------------------------------------------------------

(println "=== Distribution Protocol ===")

(test-assert "dist? true for Bernoulli"
  (dist? (bernoulli-dist 0.5)))

(test-assert "dist? true for Gaussian"
  (dist? (gaussian-dist 0 1)))

(test-assert "dist? false for number"
  (not (dist? 42)))

(test-assert "dist? false for function"
  (not (dist? inc)))

;; ---------------------------------------------------------------------------
;; Bernoulli
;; ---------------------------------------------------------------------------

(println "=== Bernoulli ===")

(test-assert "bernoulli observe* true"
  (approx= (observe* (bernoulli-dist 0.7) true)
           (js/Math.log 0.7) 1e-10))

(test-assert "bernoulli observe* false"
  (approx= (observe* (bernoulli-dist 0.7) false)
           (js/Math.log 0.3) 1e-10))

(test-assert "bernoulli observe* p=1 true"
  (= (observe* (bernoulli-dist 1.0) true) 0.0))

(test-assert "bernoulli observe* p=0 true = -Infinity"
  (= (observe* (bernoulli-dist 0.0) true) ##-Inf))

(test-assert "bernoulli observe* p=0 false = 0"
  (= (observe* (bernoulli-dist 0.0) false) 0.0))

(test-assert "bernoulli sample* returns boolean"
  (boolean? (sample* (bernoulli-dist 0.5))))

(test-assert "bernoulli default p=0.5"
  (dist? (bernoulli-dist)))

;; ---------------------------------------------------------------------------
;; Gaussian
;; ---------------------------------------------------------------------------

(println "=== Gaussian ===")

;; N(0,1) at x=0: -0.5*log(2*pi)
(test-assert "gaussian N(0,1) observe* at 0"
  (approx= (observe* (gaussian-dist 0 1) 0)
           (* -0.5 (js/Math.log (* 2 js/Math.PI)))
           1e-10))

;; N(0,1) at x=1: -0.5*(log(2*pi) + 1)
(test-assert "gaussian N(0,1) observe* at 1"
  (approx= (observe* (gaussian-dist 0 1) 1)
           (* -0.5 (+ (js/Math.log (* 2 js/Math.PI)) 1.0))
           1e-10))

;; N(3,2) at x=5: z=1, -0.5*(log(2*pi) + 2*log(2) + 1)
(test-assert "gaussian N(3,2) observe* at 5"
  (approx= (observe* (gaussian-dist 3 2) 5)
           (* -0.5 (+ (js/Math.log (* 2 js/Math.PI))
                      (* 2.0 (js/Math.log 2))
                      1.0))
           1e-10))

;; Degenerate: sigma=0
(test-assert "gaussian sigma=0 observe* at mu = 0"
  (= (observe* (gaussian-dist 5 0) 5) 0.0))

(test-assert "gaussian sigma=0 observe* away from mu = -Infinity"
  (= (observe* (gaussian-dist 5 0) 6) ##-Inf))

(test-assert "gaussian sample* returns number"
  (number? (sample* (gaussian-dist 0 1))))

(test-assert "gaussian default N(0,1)"
  (dist? (gaussian-dist)))

;; ---------------------------------------------------------------------------
;; Uniform (continuous)
;; ---------------------------------------------------------------------------

(println "=== Uniform ===")

;; U(3,7) at x=5: -log(4)
(test-assert "uniform observe* in support"
  (approx= (observe* (uniform-dist 3 7) 5)
           (- (js/Math.log 4))
           1e-10))

(test-assert "uniform observe* at lower bound"
  (approx= (observe* (uniform-dist 3 7) 3)
           (- (js/Math.log 4))
           1e-10))

(test-assert "uniform observe* below support = -Infinity"
  (= (observe* (uniform-dist 3 7) 2) ##-Inf))

(test-assert "uniform observe* above support = -Infinity"
  (= (observe* (uniform-dist 3 7) 8) ##-Inf))

;; Degenerate: a=b
(test-assert "uniform a=b observe* at a = 0"
  (= (observe* (uniform-dist 5 5) 5) 0.0))

(test-assert "uniform a=b observe* away = -Infinity"
  (= (observe* (uniform-dist 5 5) 6) ##-Inf))

(test-assert "uniform sample* in range"
  (let [x (sample* (uniform-dist 3 7))]
    (and (>= x 3) (<= x 7))))

;; ---------------------------------------------------------------------------
;; Beta
;; ---------------------------------------------------------------------------

(println "=== Beta ===")

;; Beta(2,5) at x=0.3:
;; (2-1)*log(0.3) + (5-1)*log(0.7) - log-beta(2,5)
;; B(2,5) = 1/30, so -log-beta = log(30)
(test-assert "beta(2,5) observe* at 0.3"
  (approx= (observe* (beta-dist 2 5) 0.3)
           (+ (js/Math.log 0.3)
              (* 4 (js/Math.log 0.7))
              (- (js/Math.log (/ 1.0 30.0))))
           1e-10))

;; Beta(1,1) = Uniform(0,1): log-pdf = 0 everywhere in (0,1)
(test-assert "beta(1,1) observe* = 0 (uniform)"
  (approx= (observe* (beta-dist 1 1) 0.5) 0.0 1e-10))

(test-assert "beta(1,1) observe* = 0 at any interior point"
  (approx= (observe* (beta-dist 1 1) 0.99) 0.0 1e-10))

;; Beta(1,3) at x=0.5: 0 + 2*log(0.5) - log(1/3) = 2*log(0.5) + log(3)
(test-assert "beta(1,3) observe* at 0.5"
  (approx= (observe* (beta-dist 1 3) 0.5)
           (+ (* 2 (js/Math.log 0.5)) (js/Math.log 3))
           1e-10))

;; Out of support
(test-assert "beta observe* below 0 = -Infinity"
  (= (observe* (beta-dist 2 5) -0.1) ##-Inf))

(test-assert "beta observe* above 1 = -Infinity"
  (= (observe* (beta-dist 2 5) 1.1) ##-Inf))

(test-assert "beta sample* in [0,1]"
  (let [x (sample* (beta-dist 2 5))]
    (and (>= x 0) (<= x 1))))

;; ---------------------------------------------------------------------------
;; Gamma
;; ---------------------------------------------------------------------------

(println "=== Gamma ===")

;; Gamma(shape=2, scale=3) at x=5:
;; (2-1)*log(5) - 5/3 - 2*log(3) - log-gamma(2)
;; log-gamma(2) = 0
(test-assert "gamma(2,3) observe* at 5"
  (approx= (observe* (gamma-dist 2 3) 5)
           (+ (js/Math.log 5)
              (- (/ 5.0 3.0))
              (- (* 2 (js/Math.log 3)))
              0)  ;; log-gamma(2)=0
           1e-10))

;; Gamma(1, scale) = Exponential(1/scale): cross-check
(let [scale 3.0
      x 2.5
      gamma-lp (observe* (gamma-dist 1 scale) x)
      exp-lp (observe* (exponential-dist (/ 1.0 scale)) x)]
  (test-assert "gamma(1,scale) = exponential(1/scale)"
    (approx= gamma-lp exp-lp 1e-10)))

;; Gamma(1, scale) at x=0: density = 1/scale, log = -log(scale)
(test-assert "gamma(1,3) observe* at x=0"
  (approx= (observe* (gamma-dist 1 3) 0)
           (- (js/Math.log 3))
           1e-10))

;; Out of support
(test-assert "gamma observe* x<0 = -Infinity"
  (= (observe* (gamma-dist 2 3) -1) ##-Inf))

(test-assert "gamma sample* positive"
  (> (sample* (gamma-dist 2 1)) 0))

;; ---------------------------------------------------------------------------
;; Exponential
;; ---------------------------------------------------------------------------

(println "=== Exponential ===")

;; Exponential(rate=2) at x=1: log(2) - 2
(test-assert "exponential(2) observe* at 1"
  (approx= (observe* (exponential-dist 2) 1)
           (- (js/Math.log 2) 2.0)
           1e-10))

;; At x=0: log(rate)
(test-assert "exponential(2) observe* at 0"
  (approx= (observe* (exponential-dist 2) 0)
           (js/Math.log 2)
           1e-10))

;; Out of support
(test-assert "exponential observe* x<0 = -Infinity"
  (= (observe* (exponential-dist 2) -1) ##-Inf))

(test-assert "exponential sample* positive"
  (> (sample* (exponential-dist 1)) 0))

;; ---------------------------------------------------------------------------
;; Dirichlet
;; ---------------------------------------------------------------------------

(println "=== Dirichlet ===")

;; Dirichlet([2, 3]) at [0.4, 0.6]:
;; log-gamma(5) - log-gamma(2) - log-gamma(3) + log(0.4) + 2*log(0.6)
(test-assert "dirichlet([2,3]) observe* at [0.4, 0.6]"
  (let [expected (+ (js/Math.log 24)             ;; log-gamma(5) = log(24)
                    (- 0)                         ;; -log-gamma(2) = 0
                    (- (js/Math.log 2))           ;; -log-gamma(3) = -log(2)
                    (js/Math.log 0.4)             ;; (2-1)*log(0.4)
                    (* 2 (js/Math.log 0.6)))]     ;; (3-1)*log(0.6)
    (approx= (observe* (dirichlet-dist [2 3]) [0.4 0.6])
             expected 1e-8)))

;; Dirichlet([1,1,1]) = uniform over 2-simplex, density = 2
(test-assert "dirichlet([1,1,1]) observe* = log(2) (uniform simplex)"
  (approx= (observe* (dirichlet-dist [1 1 1]) [0.33 0.34 0.33])
           (js/Math.log 2)
           1e-8))

;; Wrong dimension
(test-assert "dirichlet wrong dimension = -Infinity"
  (= (observe* (dirichlet-dist [1 1 1]) [0.5 0.5]) ##-Inf))

;; Negative component
(test-assert "dirichlet negative component = -Infinity"
  (= (observe* (dirichlet-dist [2 3]) [-0.1 1.1]) ##-Inf))

(test-assert "dirichlet sample* sums to ~1"
  (let [d (sample* (dirichlet-dist [1 1 1]))]
    (< (js/Math.abs (- (reduce + d) 1.0)) 0.001)))

;; ---------------------------------------------------------------------------
;; Uniform draw (discrete)
;; ---------------------------------------------------------------------------

(println "=== UniformDraw ===")

;; 3 items: -log(3)
(test-assert "uniform-draw observe* item in set"
  (approx= (observe* (uniform-draw-dist [:a :b :c]) :b)
           (- (js/Math.log 3))
           1e-10))

(test-assert "uniform-draw observe* item not in set = -Infinity"
  (= (observe* (uniform-draw-dist [:a :b :c]) :d) ##-Inf))

;; Duplicates: [:a :a :b] → P(:a) = 2/3
(test-assert "uniform-draw observe* with duplicates"
  (approx= (observe* (uniform-draw-dist [:a :a :b]) :a)
           (js/Math.log (/ 2.0 3.0))
           1e-10))

(test-assert "uniform-draw sample* returns item from set"
  (contains? #{:a :b :c} (sample* (uniform-draw-dist [:a :b :c]))))

;; ---------------------------------------------------------------------------
;; Random integer
;; ---------------------------------------------------------------------------

(println "=== RandomInteger ===")

;; [0, 5): -log(5)
(test-assert "random-integer observe* valid"
  (approx= (observe* (random-integer-dist 5) 3)
           (- (js/Math.log 5))
           1e-10))

(test-assert "random-integer observe* at 0"
  (approx= (observe* (random-integer-dist 5) 0)
           (- (js/Math.log 5))
           1e-10))

(test-assert "random-integer observe* out of range high = -Infinity"
  (= (observe* (random-integer-dist 5) 5) ##-Inf))

(test-assert "random-integer observe* negative = -Infinity"
  (= (observe* (random-integer-dist 5) -1) ##-Inf))

(test-assert "random-integer observe* non-integer = -Infinity"
  (= (observe* (random-integer-dist 5) 1.5) ##-Inf))

(test-assert "random-integer sample* in range"
  (let [x (sample* (random-integer-dist 10))]
    (and (>= x 0) (< x 10) (integer? x))))

;; ---------------------------------------------------------------------------
;; Multinomial (weighted discrete)
;; ---------------------------------------------------------------------------

(println "=== Multinomial ===")

;; Normalized weights: P(:b) = 0.3
(test-assert "multinomial observe* with normalized weights"
  (approx= (observe* (multinomial-dist [:a :b :c] [0.2 0.3 0.5]) :b)
           (js/Math.log 0.3)
           1e-10))

;; Non-normalized weights: P(:b) = 3/10
(test-assert "multinomial observe* with non-normalized weights"
  (approx= (observe* (multinomial-dist [:a :b :c] [2 3 5]) :b)
           (js/Math.log 0.3)
           1e-10))

;; Duplicate items: [:a :a :b] with [1 2 3] → P(:a) = 3/6 = 0.5
(test-assert "multinomial observe* with duplicate items"
  (approx= (observe* (multinomial-dist [:a :a :b] [1 2 3]) :a)
           (js/Math.log 0.5)
           1e-10))

;; Missing item
(test-assert "multinomial observe* missing item = -Infinity"
  (= (observe* (multinomial-dist [:a :b :c] [0.2 0.3 0.5]) :d) ##-Inf))

(test-assert "multinomial sample* returns item from set"
  (contains? #{:a :b :c}
    (sample* (multinomial-dist [:a :b :c] [0.2 0.3 0.5]))))

;; ---------------------------------------------------------------------------
;; Sample-discrete (weighted index)
;; ---------------------------------------------------------------------------

(println "=== SampleDiscrete ===")

;; P(index 2) = 0.7/1.0
(test-assert "sample-discrete observe* valid index"
  (approx= (observe* (sample-discrete-dist [0.1 0.2 0.7]) 2)
           (js/Math.log 0.7)
           1e-10))

;; Non-normalized: P(index 1) = 3/10
(test-assert "sample-discrete observe* non-normalized"
  (approx= (observe* (sample-discrete-dist [2 3 5]) 1)
           (js/Math.log 0.3)
           1e-10))

(test-assert "sample-discrete observe* out of range = -Infinity"
  (= (observe* (sample-discrete-dist [0.1 0.2 0.7]) 3) ##-Inf))

(test-assert "sample-discrete observe* negative = -Infinity"
  (= (observe* (sample-discrete-dist [0.1 0.2 0.7]) -1) ##-Inf))

(test-assert "sample-discrete sample* valid index"
  (let [x (sample* (sample-discrete-dist [0.1 0.2 0.7]))]
    (and (>= x 0) (< x 3) (integer? x))))

;; ---------------------------------------------------------------------------
;; Cross-distribution consistency
;; ---------------------------------------------------------------------------

(println "=== Consistency ===")

;; Beta(a,b) and Dirichlet([a,b]) should give same log-prob for first component
(let [a 2 b 5 x 0.3
      beta-lp (observe* (beta-dist a b) x)
      dir-lp (observe* (dirichlet-dist [a b]) [x (- 1.0 x)])]
  (test-assert "beta(a,b) = dirichlet([a,b]) log-prob"
    (approx= beta-lp dir-lp 1e-8)))

;; Bernoulli(p) and Multinomial([true false], [p 1-p]) should agree
(let [p 0.7
      bern-lp (observe* (bernoulli-dist p) true)
      multi-lp (observe* (multinomial-dist [true false] [p (- 1 p)]) true)]
  (test-assert "bernoulli(p) = multinomial([true false],[p,1-p])"
    (approx= bern-lp multi-lp 1e-10)))

;; ---------------------------------------------------------------------------
;; Summary
;; ---------------------------------------------------------------------------

(println)
(println (str "=== prob.dist tests: " @pass-count " passed, " @fail-count " failed ==="))
(when (pos? @fail-count)
  (js/process.exit 1))
