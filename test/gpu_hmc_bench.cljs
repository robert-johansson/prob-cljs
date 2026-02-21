(ns gpu-hmc-bench
  "Benchmark HMC per-step performance."
  (:require [promesa.core :as p]
            [prob.gpu.device :as dev]
            [prob.gpu.tensor :as t]
            [prob.gpu.inference :as infer]))

(defn- bench-hmc [label opts log-density init]
  (let [warmup 20
        n      100
        total  (+ warmup n)]
    (p/let [_ (infer/hmc (assoc opts :samples warmup :burn 0) log-density init)]
      ;; Timed run
      (let [start (js/Date.now)]
        (p/let [result (infer/hmc (assoc opts :samples n :burn 0) log-density init)]
          (let [elapsed (- (js/Date.now) start)
                per-step (/ elapsed n)]
            (println (str label ": " (.toFixed per-step 2) "ms/step"
                         " (" n " steps in " elapsed "ms)"
                         " accept=" (.toFixed (:acceptance-rate result) 3)))))))))

(println "\n=== GPU HMC Benchmark ===\n")

(p/let [_ (dev/init!)

        ;; 1D Gaussian with autograd
        _ (do (t/set-rng-seed! 42)
              (let [log-density (fn [x]
                                  (t/multiply (t/scalar-cached -0.5)
                                              (t/square (t/subtract x (t/scalar-cached 3.0)))))]
                (bench-hmc "1D autograd" {:step-size 0.1 :leapfrog-steps 10}
                           log-density (t/scalar 0.0))))

        ;; 1D Gaussian with user-provided gradient
        _ (do (t/set-rng-seed! 42)
              (let [mu (t/scalar-cached 3.0)
                    log-density (fn [x]
                                  (t/multiply (t/scalar-cached -0.5)
                                              (t/square (t/subtract x mu))))
                    grad-fn (fn [x] (t/negative (t/subtract x mu)))]
                (bench-hmc "1D user-grad" {:step-size 0.1 :leapfrog-steps 10
                                           :grad-fn grad-fn}
                           log-density (t/scalar 0.0))))

        ;; 2D Gaussian with autograd
        _ (do (t/set-rng-seed! 123)
              (let [mu (t/tensor [2 -1])
                    log-density (fn [x]
                                  (t/multiply (t/scalar-cached -0.5)
                                              (t/sum (t/square (t/subtract x mu)))))]
                (bench-hmc "2D autograd" {:step-size 0.1 :leapfrog-steps 10}
                           log-density (t/tensor [0 0]))))

        ;; 2D Gaussian with user-provided gradient
        _ (do (t/set-rng-seed! 123)
              (let [mu (t/tensor [2 -1])
                    log-density (fn [x]
                                  (t/multiply (t/scalar-cached -0.5)
                                              (t/sum (t/square (t/subtract x mu)))))
                    grad-fn (fn [x] (t/negative (t/subtract x mu)))]
                (bench-hmc "2D user-grad" {:step-size 0.1 :leapfrog-steps 10
                                           :grad-fn grad-fn}
                           log-density (t/tensor [0 0]))))]
  (println "\nDone.")
  (js/process.exit 0))
