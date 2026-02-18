(ns mlx-dist-test
  "Test prob.mlx.dist distributions."
  (:require [prob.mlx.core :as mx]
            [prob.mlx.dist :as md]
            [prob.dist :as dist]))

(defn pass [name] (println (str "  PASS: " name)))
(defn fail [name msg] (println (str "  FAIL: " name " - " msg)))
(defn approx= [a b] (< (js/Math.abs (- a b)) 0.1))

;; ── Gaussian ──
(println "\n=== MLX Gaussian ===")
(let [g (md/mlx-gaussian 0 1)]
  ;; IDistribution
  (let [s (dist/sample* g)
        lp (dist/observe* g 0.0)]
    (println "  sample:" s)
    (if (approx= lp -0.9189)
      (pass (str "observe(0)=" lp))
      (fail "observe" lp)))
  ;; IDifferentiable
  (let [lp (md/log-prob g (mx/scalar 0.0))]
    (mx/eval! lp)
    (if (approx= (mx/item lp) -0.9189)
      (pass (str "log-prob(0)=" (mx/item lp)))
      (fail "log-prob" (mx/item lp))))
  ;; Reparameterized sample
  (let [s (md/sample-reparam g)]
    (mx/eval! s)
    (println "  reparam sample:" (mx/item s))
    (pass "sample-reparam")))

;; ── Gaussian gradient ──
(println "\n=== Gaussian Gradient ===")
(let [;; d/dmu log N(x=1 | mu, sigma=1) = (x - mu) / sigma^2 = (1 - mu)
      ;; At mu=0: gradient = 1.0
      f (fn [mu]
          (let [sigma (mx/scalar 1.0)
                g (md/mlx-gaussian mu sigma)]
            (md/log-prob g (mx/scalar 1.0))))
      df (mx/grad f)
      result (df (mx/scalar 0.0))]
  (mx/eval! result)
  (let [v (mx/item result)]
    (if (approx= v 1.0)
      (pass (str "d/dmu logN(1|0,1) = " v))
      (fail "gaussian grad" (str "expected 1.0, got " v)))))

;; ── Gaussian log-prob function ──
(println "\n=== Gaussian log-prob function ===")
(let [lp (md/gaussian-log-prob (mx/scalar 0) (mx/scalar 1) (mx/scalar 0))]
  (mx/eval! lp)
  (if (approx= (mx/item lp) -0.9189)
    (pass (str "gaussian-log-prob=" (mx/item lp)))
    (fail "gaussian-log-prob" (mx/item lp))))

;; ── Sum of Gaussian log-probs ──
(println "\n=== Sum Gaussian log-probs ===")
(let [mu (mx/zeros [3])
      sigma (mx/ones [3])
      values (mx/array [0 0 0])
      lp (md/sum-gaussian-log-probs mu sigma values)]
  (mx/eval! lp)
  (let [expected (* 3 -0.9189)]
    (if (approx= (mx/item lp) expected)
      (pass (str "sum-log-probs=" (mx/item lp)))
      (fail "sum-log-probs" (str "expected " expected " got " (mx/item lp))))))

;; ── Uniform ──
(println "\n=== MLX Uniform ===")
(let [u (md/mlx-uniform 0 1)]
  (let [s (dist/sample* u)
        lp-in (dist/observe* u 0.5)
        lp-out (dist/observe* u 1.5)]
    (println "  sample:" s)
    (if (and (approx= lp-in 0.0) (= lp-out ##-Inf))
      (pass (str "observe in=" lp-in " out=" lp-out))
      (fail "uniform observe" (str "in=" lp-in " out=" lp-out)))))

;; ── Exponential ──
(println "\n=== MLX Exponential ===")
(let [e (md/mlx-exponential 2.0)]
  (let [s (dist/sample* e)
        lp (dist/observe* e 1.0)]
    (println "  sample:" s)
    ;; log(2) - 2*1 = 0.693 - 2 = -1.307
    (if (approx= lp -1.307)
      (pass (str "observe(1)=" lp))
      (fail "exponential observe" lp))))

;; ── Multivariate Normal ──
(println "\n=== MLX Multivariate Normal ===")
(let [mu (mx/array [0 0])
      cov (mx/reshape (mx/array [1 0.5 0.5 1]) [2 2])
      mvn (md/mlx-multivariate-normal mu cov)]
  (let [s (dist/sample* mvn)]
    (println "  sample:" s)
    (pass "mvn sample"))
  (let [lp (dist/observe* mvn [0 0])]
    (println "  observe([0,0]):" lp)
    ;; -0.5 * (k*log(2pi) + log(det(cov)) + 0) = -0.5 * (2*1.8378 + log(0.75))
    ;; = -0.5 * (3.6757 + (-0.2877)) = -0.5 * 3.388 = -1.694
    (if (approx= lp -1.694)
      (pass (str "mvn observe=" lp))
      (fail "mvn observe" lp))))

;; ── Protocol bridge: use MLX dist in prob-cljs inference ──
(println "\n=== Protocol Bridge ===")
(let [g (md/mlx-gaussian 0 1)]
  (if (dist/dist? g)
    (pass "MLXGaussian satisfies IDistribution")
    (fail "protocol bridge" "not a dist")))

;; ── Gradient through Bayesian model ──
(println "\n=== Bayesian Model Gradient ===")
(let [;; Simple model: prior N(0,10), likelihood N(obs=5|mu, 1)
      ;; log-posterior(mu) = logN(mu|0,10) + logN(5|mu,1)
      log-posterior (fn [mu]
                      (mx/add
                        (md/gaussian-log-prob (mx/scalar 0) (mx/scalar 10) mu)
                        (md/gaussian-log-prob mu (mx/scalar 1) (mx/scalar 5))))
      df (mx/grad log-posterior)
      ;; At mu=0: grad = -0/100 + (5-0)/1 = 5.0
      result (df (mx/scalar 0.0))]
  (mx/eval! result)
  (let [v (mx/item result)]
    (if (approx= v 5.0)
      (pass (str "posterior grad at mu=0: " v))
      (fail "posterior grad" v))))

(println "\n=== All MLX distribution tests complete ===")
