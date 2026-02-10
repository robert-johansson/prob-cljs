(ns prob-math-tests
  "Tests for prob.math special functions — known values from mathematical tables."
  (:require [prob.math :refer [log-gamma-fn log-beta-fn log-fact
                                log-sum-exp digamma erf]]))

;; ---------------------------------------------------------------------------
;; Test harness (same style as prob_tests.cljs)
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
;; log-gamma-fn
;; ---------------------------------------------------------------------------

(println "=== log-gamma-fn ===")

;; Gamma(1) = 1, ln(1) = 0
(test-assert "log-gamma(1) = 0"
  (approx= (log-gamma-fn 1) 0.0 1e-10))

;; Gamma(2) = 1, ln(1) = 0
(test-assert "log-gamma(2) = 0"
  (approx= (log-gamma-fn 2) 0.0 1e-10))

;; Gamma(0.5) = sqrt(pi), ln(sqrt(pi)) = 0.5*ln(pi)
(test-assert "log-gamma(0.5) = 0.5*ln(pi)"
  (approx= (log-gamma-fn 0.5) (* 0.5 (js/Math.log js/Math.PI)) 1e-10))

;; Gamma(3) = 2!, ln(2) = 0.6931471805599453
(test-assert "log-gamma(3) = ln(2)"
  (approx= (log-gamma-fn 3) (js/Math.log 2) 1e-10))

;; Gamma(6) = 5! = 120
(test-assert "log-gamma(6) = ln(120)"
  (approx= (log-gamma-fn 6) (js/Math.log 120) 1e-10))

;; Gamma(10) = 9! = 362880
(test-assert "log-gamma(10) = ln(362880)"
  (approx= (log-gamma-fn 10) (js/Math.log 362880) 1e-10))

;; Gamma(0.1) ~ 9.51351, ln(9.51351) ~ 2.25271
(test-assert "log-gamma(0.1) ~ 2.25271"
  (approx= (log-gamma-fn 0.1) 2.2527126517342055 1e-8))

;; Edge: non-positive returns Infinity
(test-assert "log-gamma(0) = Infinity"
  (= (log-gamma-fn 0) js/Infinity))

(test-assert "log-gamma(-1) = Infinity"
  (= (log-gamma-fn -1) js/Infinity))

;; Large argument (Stirling territory)
;; Gamma(20) = 19! = 121645100408832000
(test-assert "log-gamma(20) = ln(19!)"
  (approx= (log-gamma-fn 20) (js/Math.log 121645100408832000) 1e-6))

;; ---------------------------------------------------------------------------
;; log-beta-fn
;; ---------------------------------------------------------------------------

(println "=== log-beta-fn ===")

;; B(1,1) = 1, ln(1) = 0
(test-assert "log-beta(1,1) = 0"
  (approx= (log-beta-fn 1 1) 0.0 1e-10))

;; B(2,3) = Gamma(2)*Gamma(3)/Gamma(5) = 1*2/24 = 1/12
(test-assert "log-beta(2,3) = ln(1/12)"
  (approx= (log-beta-fn 2 3) (js/Math.log (/ 1.0 12.0)) 1e-10))

;; B(0.5,0.5) = pi, ln(pi)
(test-assert "log-beta(0.5,0.5) = ln(pi)"
  (approx= (log-beta-fn 0.5 0.5) (js/Math.log js/Math.PI) 1e-10))

;; Symmetry: B(a,b) = B(b,a)
(test-assert "log-beta symmetry: log-beta(3,7) = log-beta(7,3)"
  (approx= (log-beta-fn 3 7) (log-beta-fn 7 3) 1e-12))

;; ---------------------------------------------------------------------------
;; log-fact
;; ---------------------------------------------------------------------------

(println "=== log-fact ===")

(test-assert "log-fact(0) = 0"
  (approx= (log-fact 0) 0.0 1e-10))

(test-assert "log-fact(1) = 0"
  (approx= (log-fact 1) 0.0 1e-10))

(test-assert "log-fact(5) = ln(120)"
  (approx= (log-fact 5) (js/Math.log 120) 1e-10))

(test-assert "log-fact(10) = ln(3628800)"
  (approx= (log-fact 10) (js/Math.log 3628800) 1e-10))

;; ---------------------------------------------------------------------------
;; log-sum-exp
;; ---------------------------------------------------------------------------

(println "=== log-sum-exp ===")

;; Two-argument form
(test-assert "log-sum-exp(0, 0) = ln(2)"
  (approx= (log-sum-exp 0 0) (js/Math.log 2) 1e-10))

;; Numerical stability: both arguments very negative
(test-assert "log-sum-exp(-1000, -1000) = -1000 + ln(2)"
  (approx= (log-sum-exp -1000 -1000) (+ -1000 (js/Math.log 2)) 1e-10))

;; Identity: log-sum-exp(-Inf, x) = x
(test-assert "log-sum-exp(-Inf, 5) = 5"
  (= (log-sum-exp ##-Inf 5) 5))

(test-assert "log-sum-exp(5, -Inf) = 5"
  (= (log-sum-exp 5 ##-Inf) 5))

;; Asymmetric
(test-assert "log-sum-exp(0, 1) = ln(1 + e)"
  (approx= (log-sum-exp 0 1) (js/Math.log (+ 1.0 js/Math.E)) 1e-10))

;; Collection form
(test-assert "log-sum-exp([1, 2, 3]) = ln(e + e^2 + e^3)"
  (approx= (log-sum-exp [1 2 3])
           (js/Math.log (+ (js/Math.exp 1) (js/Math.exp 2) (js/Math.exp 3)))
           1e-10))

;; Empty collection
(test-assert "log-sum-exp([]) = -Infinity"
  (= (log-sum-exp []) ##-Inf))

;; Single element
(test-assert "log-sum-exp([7]) = 7"
  (approx= (log-sum-exp [7]) 7.0 1e-10))

;; Numerical stability with collection
(test-assert "log-sum-exp([-1000, -1001, -999]) stable"
  (let [result (log-sum-exp [-1000 -1001 -999])
        ;; = -999 + ln(exp(-1) + exp(-2) + 1) = -999 + ln(1 + e^-1 + e^-2)
        expected (+ -999 (js/Math.log (+ 1.0
                                         (js/Math.exp -1)
                                         (js/Math.exp -2))))]
    (approx= result expected 1e-10)))

;; ---------------------------------------------------------------------------
;; digamma
;; ---------------------------------------------------------------------------

(println "=== digamma ===")

;; psi(1) = -gamma (Euler-Mascheroni constant)
(def ^:private euler-mascheroni 0.5772156649015329)

(test-assert "digamma(1) = -euler-mascheroni"
  (approx= (digamma 1) (- euler-mascheroni) 1e-6))

;; psi(2) = 1 - gamma
(test-assert "digamma(2) = 1 - euler-mascheroni"
  (approx= (digamma 2) (- 1.0 euler-mascheroni) 1e-6))

;; psi(0.5) = -gamma - 2*ln(2)
(test-assert "digamma(0.5) = -gamma - 2*ln(2)"
  (approx= (digamma 0.5)
           (- (- euler-mascheroni) (* 2 (js/Math.log 2)))
           1e-6))

;; psi(6) = 1 + 1/2 + 1/3 + 1/4 + 1/5 - gamma = 137/60 - gamma
(test-assert "digamma(6) = 137/60 - gamma"
  (approx= (digamma 6) (- (/ 137.0 60.0) euler-mascheroni) 1e-6))

;; psi(10) = sum(1/k, k=1..9) - gamma
(test-assert "digamma(10) = H_9 - gamma"
  (let [h9 (reduce + (map #(/ 1.0 %) (range 1 10)))]
    (approx= (digamma 10) (- h9 euler-mascheroni) 1e-6)))

;; Edge: non-positive returns NaN
(test-assert "digamma(0) = NaN"
  (js/isNaN (digamma 0)))

(test-assert "digamma(-1) = NaN"
  (js/isNaN (digamma -1)))

;; Small positive value
(test-assert "digamma(0.01) ~ -100.56"
  ;; psi(0.01) = psi(1) - 1/0.01 = -0.5772... - 100 ~ -100.5772
  (approx= (digamma 0.01) -100.56088545786867 1e-4))

;; ---------------------------------------------------------------------------
;; erf
;; ---------------------------------------------------------------------------

(println "=== erf ===")

(test-assert "erf(0) = 0"
  (approx= (erf 0) 0.0 1e-7))

(test-assert "erf(1) ~ 0.84270"
  (approx= (erf 1) 0.8427007929497149 1e-6))

(test-assert "erf(-1) ~ -0.84270 (odd function)"
  (approx= (erf -1) -0.8427007929497149 1e-6))

(test-assert "erf(0.5) ~ 0.52050"
  (approx= (erf 0.5) 0.5204998778130465 1e-6))

(test-assert "erf(2) ~ 0.99532"
  (approx= (erf 2) 0.9953222650189527 1e-6))

(test-assert "erf(3) ~ 0.99998"
  (approx= (erf 3) 0.9999779095030014 1e-5))

;; Large argument saturates to 1
(test-assert "erf(6) ~ 1.0"
  (approx= (erf 6) 1.0 1e-7))

;; Symmetry: erf(-x) = -erf(x)
(test-assert "erf symmetry: erf(-2.5) = -erf(2.5)"
  (approx= (erf -2.5) (- (erf 2.5)) 1e-12))

;; ---------------------------------------------------------------------------
;; Summary
;; ---------------------------------------------------------------------------

(println)
(println (str "=== prob.math tests: " @pass-count " passed, " @fail-count " failed ==="))
(when (pos? @fail-count)
  (js/process.exit 1))
