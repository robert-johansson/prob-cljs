(ns mlx-benchmark-compare
  "Compare OLD (eval per gradient) vs NEW (compile + tidy + value-and-grad).
   Same model, same parameters. Measures the effect of GPU best practices."
  (:require [prob.mlx.core :as mx]
            [prob.mlx.inference :as infer]))

(defonce ^:private mx-core (.-core (js/require "@frost-beta/mlx")))

;; ── Shared model ──
(def xs (mx/array [1 2 3 4 5 6 7 8 9 10]))
(def ys (mx/array [3.1 4.9 7.2 8.8 11.1 12.7 15.2 17.1 18.8 21.3]))

(defn log-density [params]
  (let [w (mx/index params 0)
        b (mx/index params 1)
        y-hat (mx/add (mx/multiply w xs) b)
        residuals (mx/subtract ys y-hat)
        sigma (mx/scalar 0.5)
        obs-lp (mx/multiply (mx/scalar -0.5)
                            (mx/sum (mx/divide
                                      (mx/multiply residuals residuals)
                                      (mx/multiply sigma sigma))))
        prior-w (mx/multiply (mx/scalar -0.005) (mx/multiply w w))
        prior-b (mx/multiply (mx/scalar -0.005) (mx/multiply b b))]
    (mx/add obs-lp (mx/add prior-w prior-b))))

;; ═══════════════════════════════════════════════════
;; OLD: eval after every gradient, no compile, no tidy
;; (67 eval! calls per sample)
;; ═══════════════════════════════════════════════════

(defn old-leapfrog [grad-U q p step-size]
  (let [eps (mx/scalar step-size)
        half-eps (mx/scalar (* 0.5 step-size))
        grad-q (grad-U q)
        _ (mx/eval! grad-q)
        p (mx/subtract p (mx/multiply half-eps grad-q))
        q (mx/add q (mx/multiply eps p))
        grad-q (grad-U q)
        _ (mx/eval! grad-q)
        p (mx/subtract p (mx/multiply half-eps grad-q))]
    (mx/eval! q p)
    [q p]))

(defn old-hmc [n-samples log-density init-params step-size n-leapfrog burn]
  (let [grad-neg-ld (mx/grad (fn [q] (mx/negative (log-density q))))]
    (loop [i 0, q init-params, collected []]
      (if (>= (count collected) n-samples)
        collected
        (let [p0 (mx/random-normal (mx/shape q))
              _ (mx/eval! p0)
              current-U (mx/negative (log-density q))
              current-K (mx/multiply (mx/scalar 0.5) (mx/sum (mx/multiply p0 p0)))
              _ (mx/eval! current-U current-K)
              [pq pp] (loop [j 0, q q, p p0]
                        (if (>= j n-leapfrog)
                          [q p]
                          (let [[q' p'] (old-leapfrog grad-neg-ld q p step-size)]
                            (recur (inc j) q' p'))))
              proposed-U (mx/negative (log-density pq))
              proposed-K (mx/multiply (mx/scalar 0.5) (mx/sum (mx/multiply pp pp)))
              _ (mx/eval! proposed-U proposed-K)
              current-H (+ (mx/item current-U) (mx/item current-K))
              proposed-H (+ (mx/item proposed-U) (mx/item proposed-K))
              log-accept (- current-H proposed-H)
              accept? (or (> log-accept 0) (< (js/Math.log (js/Math.random)) log-accept))
              new-q (if accept? pq q)]
          (mx/dispose! p0)
          (mx/dispose! current-U)
          (mx/dispose! current-K)
          (mx/dispose! proposed-U)
          (mx/dispose! proposed-K)
          (when (and (not accept?) (not= pq q))
            (mx/dispose! pq))
          (recur (inc i) new-q
                 (if (>= i burn) (conj collected new-q) collected)))))))

;; ═══════════════════════════════════════════════════
;; Run comparison
;; ═══════════════════════════════════════════════════

(println "\n=== A/B Comparison: OLD vs NEW GPU Utilization ===")
(println "Model: y = 2x + 1, 10 data points, 2 parameters")
(println "HMC: 500 samples, 100 burn-in, 20 leapfrog steps, eps=0.005\n")

(println "--- OLD (3 evals per leapfrog step, no compile, no tidy) ---")
(let [t0 (js/Date.now)
      samples (old-hmc 500 log-density (mx/zeros [2]) 0.005 20 100)
      elapsed (- (js/Date.now) t0)
      m (mx/->clj (mx/mean (mx/stack samples) [0]))]
  (println (str "  Time: " elapsed " ms"))
  (println (str "  ms/sample: " (.toFixed (/ elapsed 600.0) 1)))
  (println (str "  eval! per sample: ~67 (3 per leapfrog step + overhead)"))
  (println (str "  Result: w=" (first m) " b=" (second m))))

(println)

(println "--- NEW (compile + tidy + value-and-grad, L+2 evals) ---")
(let [t0 (js/Date.now)
      samples (infer/hmc
                {:samples 500 :step-size 0.005 :leapfrog-steps 20 :burn 100}
                log-density (mx/zeros [2]))
      elapsed (- (js/Date.now) t0)
      mean-arr (infer/sample-mean samples)]
  (mx/eval! mean-arr)
  (let [m (mx/->clj mean-arr)]
    (println (str "  Time: " elapsed " ms"))
    (println (str "  ms/sample: " (.toFixed (/ elapsed 600.0) 1)))
    (println (str "  eval! per sample: 22 (1 per leapfrog step + 2 overhead)"))
    (println (str "  Result: w=" (first m) " b=" (second m)))))

(println "\n=== Done ===")
