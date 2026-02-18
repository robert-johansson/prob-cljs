(ns mlx-bayesian-regression
  "Bayesian linear regression with HMC on Metal GPU.
   Demonstrates prob-cljs + MLX integration.

   Run: nbb -cp src examples/mlx-bayesian-regression.cljs"
  (:require [prob.mlx.core :as mx]
            [prob.mlx.dist :as md]
            [prob.mlx.inference :as infer]))

;; ── Data ──
;; y = 2x + 1 + noise(0, 0.5)
(def xs (mx/array [1 2 3 4 5 6 7 8 9 10]))
(def ys (mx/array [3.1 4.9 7.2 8.8 11.1 12.7 15.2 17.1 18.8 21.3]))

(println "Bayesian Linear Regression with HMC on Metal GPU")
(println "=================================================")
(println "Model: y = w*x + b + noise")
(println "Data: 10 points along y = 2x + 1")
(println)

;; ── Log-posterior ──
;; params = [w, b]
;; Prior: w ~ N(0, 10), b ~ N(0, 10)
;; Likelihood: y_i ~ N(w*x_i + b, 0.5)

(defn log-posterior [params]
  (let [w (mx/index params 0)
        b (mx/index params 1)
        y-hat (mx/add (mx/multiply w xs) b)
        ;; Likelihood
        residuals (mx/subtract ys y-hat)
        sigma (mx/scalar 0.5)
        obs-lp (md/sum-gaussian-log-probs y-hat sigma ys)
        ;; Priors
        prior-w (md/gaussian-log-prob (mx/scalar 0) (mx/scalar 10) w)
        prior-b (md/gaussian-log-prob (mx/scalar 0) (mx/scalar 10) b)]
    (mx/add obs-lp (mx/add prior-w prior-b))))

;; ── HMC Sampling ──
(println "Running HMC (1000 samples, 200 burn-in)...")
(let [t0 (js/Date.now)
      samples (infer/hmc
                {:samples 1000
                 :step-size 0.005
                 :leapfrog-steps 20
                 :burn 200}
                log-posterior
                (mx/zeros [2]))
      elapsed (- (js/Date.now) t0)
      mean-arr (infer/sample-mean samples)
      std-arr (infer/sample-std samples)
      quantiles (infer/sample-quantiles samples)]
  (mx/eval! mean-arr std-arr)
  (let [m (mx/->clj mean-arr)
        s (mx/->clj std-arr)]
    (println)
    (println "Results:")
    (println (str "  w = " (first m) " +/- " (first s)
                  "  (true: 2.0)"))
    (println (str "  b = " (second m) " +/- " (second s)
                  "  (true: 1.0)"))
    (println)
    (println "95% credible intervals:")
    (println (str "  w: [" (:q025 (first quantiles)) ", "
                  (:q975 (first quantiles)) "]"))
    (println (str "  b: [" (:q025 (second quantiles)) ", "
                  (:q975 (second quantiles)) "]"))
    (println)
    (println (str "Acceptance rate: " (:acceptance-rate (meta samples))))
    (println (str "Time: " elapsed "ms (" (/ elapsed 1200) " ms/sample)"))))
