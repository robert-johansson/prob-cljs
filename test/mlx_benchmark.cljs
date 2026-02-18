(ns mlx-benchmark
  "Benchmark: measure actual GPU utilization.
   Compares small vs large problem sizes to show where GPU matters."
  (:require [prob.mlx.core :as mx]
            [prob.mlx.dist :as md]
            [prob.mlx.inference :as infer]))

(defn bench [name f]
  (let [t0 (js/Date.now)
        result (f)
        elapsed (- (js/Date.now) t0)]
    (println (str "  " name ": " elapsed "ms"))
    [result elapsed]))

;; ═══════════════════════════════════════════════════════════════════════
;; Benchmark 1: Small model (2 params, 10 data points)
;; This is where GPU overhead dominates -- should be similar to or
;; slower than what pure JS would achieve.
;; ═══════════════════════════════════════════════════════════════════════

(println "\n=== Benchmark 1: Small Model (2 params, 10 data) ===")
(let [xs (mx/array [1 2 3 4 5 6 7 8 9 10])
      ys (mx/array [3.1 4.9 7.2 8.8 11.1 12.7 15.2 17.1 18.8 21.3])
      log-density (fn [params]
                    (let [w (mx/index params 0)
                          b (mx/index params 1)
                          y-hat (mx/add (mx/multiply w xs) b)
                          residuals (mx/subtract ys y-hat)
                          sigma (mx/scalar 0.5)
                          obs-lp (mx/multiply (mx/scalar -0.5)
                                              (mx/sum (mx/divide
                                                        (mx/multiply residuals residuals)
                                                        (mx/multiply sigma sigma))))
                          prior-w (mx/multiply (mx/scalar -0.005)
                                               (mx/multiply w w))
                          prior-b (mx/multiply (mx/scalar -0.005)
                                               (mx/multiply b b))]
                      (mx/add obs-lp (mx/add prior-w prior-b))))

      [samples elapsed] (bench "HMC 500 samples"
                           #(infer/hmc
                              {:samples 500 :step-size 0.005
                               :leapfrog-steps 20 :burn 100}
                              log-density (mx/zeros [2])))
      mean-arr (infer/sample-mean samples)]
  (mx/eval! mean-arr)
  (println "  result: w=" (first (mx/->clj mean-arr)) "b=" (second (mx/->clj mean-arr)))
  (println "  ms/sample:" (/ elapsed 600.0))
  (println "  acceptance:" (:acceptance-rate (meta samples))))

;; ═══════════════════════════════════════════════════════════════════════
;; Benchmark 2: Medium model (20 params, 1000 data points)
;; This should start showing GPU benefits -- the likelihood
;; computation operates on 1000-element arrays.
;; ═══════════════════════════════════════════════════════════════════════

(println "\n=== Benchmark 2: Medium Model (20 params, 1000 data) ===")
(let [;; Generate synthetic polynomial regression data
      n-data 1000
      n-features 20
      ;; X: [n-data, n-features] random design matrix
      X (mx/random-normal [n-data n-features])
      ;; True weights
      true-w (mx/divide (mx/arange 1 (inc n-features)) (mx/scalar n-features))
      ;; y = X @ w + noise
      y-true (mx/flatten (mx/matmul X (mx/reshape true-w [n-features 1])))
      noise (mx/multiply (mx/scalar 0.5) (mx/random-normal [n-data]))
      ys (mx/add y-true noise)
      _ (mx/eval! X ys true-w)

      log-density (fn [params]
                    (let [y-hat (mx/flatten (mx/matmul X (mx/reshape params [n-features 1])))
                          residuals (mx/subtract ys y-hat)
                          sigma-sq (mx/scalar 0.25)
                          obs-lp (mx/multiply (mx/scalar -0.5)
                                              (mx/sum (mx/divide
                                                        (mx/multiply residuals residuals)
                                                        sigma-sq)))
                          ;; N(0, 10) prior on each weight
                          prior-lp (mx/multiply (mx/scalar -0.005)
                                                (mx/sum (mx/multiply params params)))]
                      (mx/add obs-lp prior-lp)))

      [samples elapsed] (bench "HMC 200 samples"
                           #(infer/hmc
                              {:samples 200 :step-size 0.0005
                               :leapfrog-steps 20 :burn 50}
                              log-density (mx/zeros [n-features])))
      mean-arr (infer/sample-mean samples)]
  (mx/eval! mean-arr)
  (let [m (mx/->clj mean-arr)
        tw (mx/->clj true-w)
        mse (/ (reduce + (map (fn [a b] (* (- a b) (- a b))) m tw))
               n-features)]
    (println "  MSE vs true weights:" mse)
    (println "  ms/sample:" (/ elapsed 250.0))
    (println "  acceptance:" (:acceptance-rate (meta samples)))))

;; ═══════════════════════════════════════════════════════════════════════
;; Benchmark 3: Large model (100 params, 10000 data points)
;; This is where GPU should clearly win -- matmul on [10000, 100].
;; ═══════════════════════════════════════════════════════════════════════

(println "\n=== Benchmark 3: Large Model (100 params, 10000 data) ===")
(let [n-data 10000
      n-features 100
      X (mx/random-normal [n-data n-features])
      true-w (mx/divide (mx/random-normal [n-features]) (mx/scalar 10))
      y-true (mx/flatten (mx/matmul X (mx/reshape true-w [n-features 1])))
      noise (mx/multiply (mx/scalar 0.5) (mx/random-normal [n-data]))
      ys (mx/add y-true noise)
      _ (mx/eval! X ys true-w)

      log-density (fn [params]
                    (let [y-hat (mx/flatten (mx/matmul X (mx/reshape params [n-features 1])))
                          residuals (mx/subtract ys y-hat)
                          sigma-sq (mx/scalar 0.25)
                          obs-lp (mx/multiply (mx/scalar -0.5)
                                              (mx/sum (mx/divide
                                                        (mx/multiply residuals residuals)
                                                        sigma-sq)))
                          prior-lp (mx/multiply (mx/scalar -0.005)
                                                (mx/sum (mx/multiply params params)))]
                      (mx/add obs-lp prior-lp)))

      [samples elapsed] (bench "HMC 100 samples"
                           #(infer/hmc
                              {:samples 100 :step-size 0.0001
                               :leapfrog-steps 10 :burn 20}
                              log-density (mx/zeros [n-features])))
      mean-arr (infer/sample-mean samples)]
  (mx/eval! mean-arr)
  (let [m (mx/->clj mean-arr)
        tw (mx/->clj true-w)
        mse (/ (reduce + (map (fn [a b] (* (- a b) (- a b))) m tw))
               n-features)]
    (println "  MSE vs true weights:" mse)
    (println "  ms/sample:" (/ elapsed 120.0))
    (println "  acceptance:" (:acceptance-rate (meta samples)))))

;; ═══════════════════════════════════════════════════════════════════════
;; Benchmark 4: Pure GPU compute benchmark (no inference overhead)
;; Shows raw MLX throughput to calibrate expectations.
;; ═══════════════════════════════════════════════════════════════════════

(println "\n=== Benchmark 4: Raw MLX Compute ===")
;; Matmul benchmark: how fast is [10000, 100] @ [100, 1]?
(let [X (mx/random-normal [10000 100])
      w (mx/random-normal [100 1])
      _ (mx/eval! X w)]
  (bench "matmul [10000,100] @ [100,1] x 1000"
    (fn []
      (dotimes [_ 1000]
        (let [r (mx/matmul X w)]
          (mx/eval! r)
          (mx/dispose! r))))))

;; Gradient benchmark
(let [X (mx/random-normal [10000 100])
      y (mx/random-normal [10000])
      _ (mx/eval! X y)
      f (fn [w]
          (let [y-hat (mx/flatten (mx/matmul X (mx/reshape w [100 1])))
                r (mx/subtract y y-hat)]
            (mx/sum (mx/multiply r r))))
      gf (mx/compile-fn (mx/grad f))]
  (bench "compiled grad (10000x100 regression) x 100"
    (fn []
      (dotimes [_ 100]
        (let [w (mx/zeros [100])
              g (gf w)]
          (mx/eval! g)
          (mx/dispose! g)
          (mx/dispose! w))))))

(println "\n=== Benchmarks complete ===")
