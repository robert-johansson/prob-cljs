(ns mlx-inference-test
  "Test HMC and NUTS inference with MLX."
  (:require [prob.mlx.core :as mx]
            [prob.mlx.dist :as md]
            [prob.mlx.inference :as infer]))

(defn pass [name] (println (str "  PASS: " name)))
(defn fail [name msg] (println (str "  FAIL: " name " - " msg)))
(defn approx= [a b tol] (< (js/Math.abs (- a b)) tol))

;; ── Test 1: HMC on 1D Gaussian ──
;; Sample from N(3, 1). Posterior mean should be ~3.
(println "\n=== Test 1: HMC on 1D Gaussian ===")
(let [;; log p(x) = -0.5 * (x - 3)^2  (up to constant)
      log-density (fn [params]
                    (let [x params]
                      (mx/multiply (mx/scalar -0.5)
                                   (mx/square (mx/subtract x (mx/scalar 3.0))))))
      samples (infer/hmc
                {:samples 500 :step-size 0.1 :leapfrog-steps 10 :burn 100}
                log-density
                (mx/scalar 0.0))
      mean-arr (infer/sample-mean samples)]
  (mx/eval! mean-arr)
  (let [m (mx/item mean-arr)]
    (println "  mean:" m "(expected ~3.0)")
    (if (approx= m 3.0 0.3)
      (pass (str "HMC 1D Gaussian: mean=" m))
      (fail "HMC 1D Gaussian" (str "mean=" m)))))

;; ── Test 2: HMC acceptance rate ──
(println "\n=== Test 2: HMC Acceptance Rate ===")
(let [log-density (fn [params]
                    (mx/multiply (mx/scalar -0.5)
                                 (mx/square (mx/subtract params (mx/scalar 0.0)))))
      samples (infer/hmc
                {:samples 200 :step-size 0.1 :leapfrog-steps 10 :burn 50}
                log-density
                (mx/scalar 0.0))
      rate (:acceptance-rate (meta samples))]
  (println "  acceptance rate:" rate)
  (if (> rate 0.5)
    (pass (str "acceptance rate=" rate))
    (fail "acceptance rate" rate)))

;; ── Test 3: HMC on 2D Gaussian ──
;; Sample from N([2, -1], I). Should recover means.
(println "\n=== Test 3: HMC on 2D Gaussian ===")
(let [target-mean (mx/array [2 -1])
      log-density (fn [params]
                    (let [diff (mx/subtract params target-mean)]
                      (mx/multiply (mx/scalar -0.5)
                                   (mx/sum (mx/multiply diff diff)))))
      samples (infer/hmc
                {:samples 500 :step-size 0.1 :leapfrog-steps 10 :burn 100}
                log-density
                (mx/zeros [2]))
      mean-arr (infer/sample-mean samples)]
  (mx/eval! mean-arr)
  (let [m (mx/->clj mean-arr)]
    (println "  mean:" m "(expected [2, -1])")
    (if (and (approx= (first m) 2.0 0.5)
             (approx= (second m) -1.0 0.5))
      (pass (str "HMC 2D: mean=" m))
      (fail "HMC 2D" (str "mean=" m)))))

;; ── Test 4: Bayesian Linear Regression ──
;; y = 2x + 1 + noise, recover w~2 and b~1
(println "\n=== Test 4: Bayesian Linear Regression ===")
(let [;; Data
      xs (mx/array [1 2 3 4 5])
      ys (mx/array [3.1 4.9 7.2 8.8 11.1])  ;; y ≈ 2x + 1
      n 5

      ;; params = [w, b]
      log-density (fn [params]
                    (let [w (mx/index params 0)
                          b (mx/index params 1)
                          y-hat (mx/add (mx/multiply w xs) b)
                          ;; Likelihood: N(y | y-hat, 0.5)
                          residuals (mx/subtract ys y-hat)
                          obs-lp (mx/multiply (mx/scalar -0.5)
                                              (mx/sum (mx/multiply
                                                        (mx/divide residuals (mx/scalar 0.5))
                                                        (mx/divide residuals (mx/scalar 0.5)))))
                          ;; Priors: N(0, 10) for both
                          prior-w (mx/multiply (mx/scalar -0.5)
                                               (mx/divide (mx/multiply w w)
                                                          (mx/scalar 100)))
                          prior-b (mx/multiply (mx/scalar -0.5)
                                               (mx/divide (mx/multiply b b)
                                                          (mx/scalar 100)))]
                      (mx/add obs-lp (mx/add prior-w prior-b))))

      samples (infer/hmc
                {:samples 500 :step-size 0.005 :leapfrog-steps 20 :burn 200}
                log-density
                (mx/zeros [2]))

      mean-arr (infer/sample-mean samples)
      std-arr (infer/sample-std samples)]
  (mx/eval! mean-arr std-arr)
  (let [m (mx/->clj mean-arr)
        s (mx/->clj std-arr)]
    (println "  w:" (first m) "+/-" (first s) "(expected ~2)")
    (println "  b:" (second m) "+/-" (second s) "(expected ~1)")
    (if (and (approx= (first m) 2.0 0.5)
             (approx= (second m) 1.0 1.0))
      (pass "Bayesian linear regression")
      (fail "linear regression" (str "w=" (first m) " b=" (second m))))))

;; ── Test 5: NUTS on 1D Gaussian ──
(println "\n=== Test 5: NUTS on 1D Gaussian ===")
(let [log-density (fn [params]
                    (mx/multiply (mx/scalar -0.5)
                                 (mx/square (mx/subtract params (mx/scalar 5.0)))))
      samples (infer/nuts
                {:samples 200 :step-size 0.1 :max-depth 5 :burn 50}
                log-density
                (mx/scalar 0.0))
      mean-arr (infer/sample-mean samples)]
  (mx/eval! mean-arr)
  (let [m (mx/item mean-arr)]
    (println "  mean:" m "(expected ~5.0)")
    (if (approx= m 5.0 0.5)
      (pass (str "NUTS 1D: mean=" m))
      (fail "NUTS 1D" (str "mean=" m)))))

;; ── Test 6: Sample statistics ──
(println "\n=== Test 6: Sample Statistics ===")
(let [log-density (fn [params]
                    (mx/multiply (mx/scalar -0.5)
                                 (mx/square (mx/subtract params (mx/scalar 0.0)))))
      samples (infer/hmc
                {:samples 1000 :step-size 0.2 :leapfrog-steps 10 :burn 100}
                log-density
                (mx/scalar 0.0))
      quantiles (infer/sample-quantiles samples)]
  (println "  median:" (:median quantiles) "(expected ~0)")
  (println "  95% CI: [" (:q025 quantiles) "," (:q975 quantiles) "]")
  (if (and (approx= (:median quantiles) 0.0 0.3)
           (approx= (:q025 quantiles) -1.96 0.5)
           (approx= (:q975 quantiles) 1.96 0.5))
    (pass "sample statistics")
    (fail "sample statistics" (str quantiles))))

(println "\n=== All MLX inference tests complete ===")
