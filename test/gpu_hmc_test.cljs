(ns gpu-hmc-test
  "HMC inference tests: statistical correctness, energy conservation, gradients."
  (:require [promesa.core :as p]
            [prob.gpu.device :as dev]
            [prob.gpu.tensor :as t]
            [prob.gpu.autograd :as ag]
            [prob.gpu.inference :as infer]))

;; ---------------------------------------------------------------------------
;; Test harness
;; ---------------------------------------------------------------------------

(def ^:private passed (volatile! 0))
(def ^:private failed (volatile! 0))

(defn- pass [name]
  (vswap! passed inc)
  (println (str "  PASS: " name)))

(defn- fail [name msg]
  (vswap! failed inc)
  (println (str "  FAIL: " name " — " msg)))

(defn- approx= [a b tol]
  (< (js/Math.abs (- a b)) tol))

(defn- arr-approx= [xs ys tol]
  (and (= (count xs) (count ys))
       (every? true? (map #(approx= %1 %2 tol) xs ys))))

(defn- check [name pred msg]
  (if pred (pass name) (fail name msg)))

;; ---------------------------------------------------------------------------
;; Test 1: 1D Gaussian N(3, 1)
;; ---------------------------------------------------------------------------

(defn- test-hmc-1d-gaussian []
  (println "\n--- HMC: 1D Gaussian N(3,1) ---")
  (t/set-rng-seed! 42)
  (let [log-density (fn [x]
                      (t/multiply (t/scalar -0.5)
                                  (t/square (t/subtract x (t/scalar 3.0)))))
        init (t/scalar 0.0)]
    (p/let [result (infer/hmc {:samples 500 :step-size 0.1
                                :leapfrog-steps 10 :burn 100}
                               log-density init)
            samples (:samples result)
            mean-val (/ (reduce + samples) (count samples))]
      (check "1D sample count" (= (count samples) 500)
             (str "expected 500, got " (count samples)))
      (check "1D mean ≈ 3.0" (approx= mean-val 3.0 0.5)
             (str "expected ~3.0, got " mean-val)))))

;; ---------------------------------------------------------------------------
;; Test 2: 2D Gaussian N([2, -1], I)
;; ---------------------------------------------------------------------------

(defn- test-hmc-2d-gaussian []
  (println "\n--- HMC: 2D Gaussian N([2,-1],I) ---")
  (t/set-rng-seed! 123)
  (let [mu (t/tensor [2 -1])
        log-density (fn [x]
                      (t/multiply (t/scalar -0.5)
                                  (t/sum (t/square (t/subtract x mu)))))
        init (t/tensor [0 0])]
    (p/let [result (infer/hmc {:samples 500 :step-size 0.1
                                :leapfrog-steps 10 :burn 100}
                               log-density init)
            samples (:samples result)
            mean-0 (/ (reduce + (map first samples)) (count samples))
            mean-1 (/ (reduce + (map second samples)) (count samples))]
      (check "2D sample count" (= (count samples) 500)
             (str "expected 500, got " (count samples)))
      (check "2D mean[0] ≈ 2.0" (approx= mean-0 2.0 0.5)
             (str "expected ~2.0, got " mean-0))
      (check "2D mean[1] ≈ -1.0" (approx= mean-1 -1.0 0.5)
             (str "expected ~-1.0, got " mean-1)))))

;; ---------------------------------------------------------------------------
;; Test 3: Acceptance rate
;; ---------------------------------------------------------------------------

(defn- test-acceptance-rate []
  (println "\n--- Acceptance Rate ---")
  (t/set-rng-seed! 456)
  (let [log-density (fn [x]
                      (t/multiply (t/scalar -0.5) (t/square x)))
        init (t/scalar 0.0)]
    (p/let [result (infer/hmc {:samples 200 :step-size 0.1
                                :leapfrog-steps 10 :burn 50}
                               log-density init)
            rate (:acceptance-rate result)]
      (check "acceptance rate > 0.3" (> rate 0.3)
             (str "expected > 0.3, got " rate))
      (check "acceptance rate <= 1.0" (<= rate 1.0)
             (str "expected <= 1.0, got " rate)))))

;; ---------------------------------------------------------------------------
;; Test 4: Leapfrog energy conservation
;; ---------------------------------------------------------------------------

(defn- test-leapfrog-conservation []
  (println "\n--- Leapfrog Energy Conservation ---")
  (t/set-rng-seed! 789)
  (let [log-density (fn [x] (t/multiply (t/scalar -0.5) (t/square x)))
        grad-fn (ag/grad log-density)
        q (t/scalar 1.0)
        p (t/scalar 0.5)
        step-size 0.05
        n-steps 50
        step-t (t/scalar step-size)
        half-t (t/scalar (* 0.5 step-size))
        h-fn (fn [q p]
                (t/add (t/negative (log-density q))
                       (t/multiply (t/scalar 0.5) (t/sum (t/square p)))))]
    (p/let [h-init (t/to-number (h-fn q p))
            [q-f p-f] (loop [i 0, q q, p p]
                        (if (>= i n-steps)
                          [q p]
                          (let [g (grad-fn q)
                                ph (t/add p (t/multiply half-t g))
                                qn (t/add q (t/multiply step-t ph))
                                gn (grad-fn qn)
                                pn (t/add ph (t/multiply half-t gn))]
                            (recur (inc i) qn pn))))
            h-final (t/to-number (h-fn q-f p-f))
            diff (js/Math.abs (- h-final h-init))]
      (check "energy conserved" (< diff 0.01)
             (str "H_init=" h-init " H_final=" h-final " |diff|=" diff)))))

;; ---------------------------------------------------------------------------
;; Test 5: Gradient correctness for HMC log-density
;; ---------------------------------------------------------------------------

(defn- test-gradient-correctness []
  (println "\n--- Gradient Correctness ---")
  (let [mu (t/tensor [2 -1])
        log-density (fn [x]
                      (t/multiply (t/scalar -0.5)
                                  (t/sum (t/square (t/subtract x mu)))))]
    (p/let [x (t/tensor [3.0 0.0])
            g ((ag/grad log-density) x)
            g-clj (t/to-clj g)]
      ;; grad = -(x - mu) = -(3-2, 0-(-1)) = (-1, -1)
      (check "grad[0] ≈ -1.0" (approx= (first g-clj) -1.0 0.05)
             (str "expected -1.0, got " (first g-clj)))
      (check "grad[1] ≈ -1.0" (approx= (second g-clj) -1.0 0.05)
             (str "expected -1.0, got " (second g-clj))))))

;; ---------------------------------------------------------------------------
;; Main
;; ---------------------------------------------------------------------------

(println "\n=== GPU HMC Tests ===")

(p/let [_ (dev/init!)
        _ (test-gradient-correctness)
        _ (test-leapfrog-conservation)
        _ (test-acceptance-rate)
        _ (test-hmc-1d-gaussian)
        _ (test-hmc-2d-gaussian)]
  (println (str "\n" @passed " passed, " @failed " failed"))
  (when (pos? @failed)
    (js/process.exit 1)))
