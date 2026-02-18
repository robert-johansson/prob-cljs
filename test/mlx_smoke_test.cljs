(ns mlx-smoke-test
  "Smoke test: verify MLX tensor ops, autograd, random work from nbb.")

(def mlx (js/require "@frost-beta/mlx"))
(def mx (.-core mlx))
(def mrng (.-random mx))
(def mla (.-linalg mx))

(defn pass [name] (println (str "  PASS: " name)))
(defn fail [name expected actual]
  (println (str "  FAIL: " name " (expected " expected ", got " actual ")")))

(defn approx= [a b & [tol]]
  (< (js/Math.abs (- a b)) (or tol 0.01)))

;; ── Test 1: Array creation and basic arithmetic ──
(println "\n=== Test 1: Array Creation & Arithmetic ===")
(let [a (.array mx #js [1 2 3 4 5] (.-float32 mx))
      b (.array mx #js [10 20 30 40 50] (.-float32 mx))
      c (.add mx a b)]
  (.eval mx c)
  (println "  a + b:" (.toString c))
  (println "  shape:" (js->clj (.-shape c)))
  (println "  dtype:" (.-dtype c))
  (pass "array creation & add"))

;; ── Test 2: Matrix operations ──
(println "\n=== Test 2: Matrix Operations ===")
(let [m1 (.reshape mx (.array mx #js [1 2 3 4] (.-float32 mx)) #js [2 2])
      m2 (.reshape mx (.array mx #js [5 6 7 8] (.-float32 mx)) #js [2 2])
      prod (.matmul mx m1 m2)]
  (.eval mx prod)
  (println "  2x2 matmul:" (.toString prod))
  (pass "matmul"))

;; ── Test 3: Reductions ──
(println "\n=== Test 3: Reductions ===")
(let [x (.array mx #js [1 2 3 4 5] (.-float32 mx))
      s (.item (.sum mx x))
      m (.item (.mean mx x))]
  (if (and (== s 15) (approx= m 3.0))
    (pass (str "sum=" s " mean=" m))
    (fail "reductions" "sum=15 mean=3" (str "sum=" s " mean=" m))))

;; ── Test 4: Random number generation ──
(println "\n=== Test 4: Random Number Generation ===")
(let [lo (.array mx 0 (.-float32 mx))
      hi (.array mx 1 (.-float32 mx))
      r-uniform (.uniform mrng lo hi #js [5])
      r-normal (.normal mrng #js [5])]
  (.eval mx r-uniform r-normal)
  (println "  uniform [0,1):" (.toString r-uniform))
  (println "  normal N(0,1):" (.toString r-normal))
  (pass "random generation"))

;; ── Test 5: Lazy evaluation ──
(println "\n=== Test 5: Lazy Evaluation ===")
(let [a (.array mx #js [1 2 3] (.-float32 mx))
      b (.multiply mx a a)
      c (.add mx b a)]
  ;; c = a^2 + a = [2, 6, 12]
  (.eval mx c)
  (let [v (.item (.sum mx c))]
    (if (== v 20)
      (pass (str "lazy eval: sum=" v))
      (fail "lazy eval" 20 v))))

;; ── Test 6: Autograd ──
(println "\n=== Test 6: Autograd (gradient) ===")
(let [f (fn [x] (.multiply mx x x))
      grad-f (.grad mx f)
      x (.array mx 3.0 (.-float32 mx))
      df-dx (grad-f x)]
  (.eval mx df-dx)
  (let [v (.item df-dx)]
    (if (approx= v 6.0)
      (pass (str "f(x)=x^2, f'(3)=" v))
      (fail "grad" 6.0 v))))

;; ── Test 7: valueAndGrad ──
(println "\n=== Test 7: valueAndGrad ===")
(let [f (fn [x] (.sum mx (.multiply mx x x)))
      vg (.valueAndGrad mx f)
      x (.array mx #js [1 2 3] (.-float32 mx))
      result (vg x)
      val (aget result 0)
      grad-arr (aget result 1)]
  (.eval mx val grad-arr)
  (let [v (.item val)
        g (.toString grad-arr)]
    (if (approx= v 14.0)
      (pass (str "valueAndGrad: val=" v " grad=" g))
      (fail "valueAndGrad" 14.0 v))))

;; ── Test 8: Math operations ──
(println "\n=== Test 8: Math Operations ===")
(let [x (.array mx #js [1 2 3] (.-float32 mx))
      ex (.exp mx x)
      lx (.log mx x)]
  (.eval mx ex lx)
  (println "  exp([1,2,3]):" (.toString ex))
  (println "  log([1,2,3]):" (.toString lx))
  (pass "exp/log"))

;; ── Test 9: tidy (memory management) ──
(println "\n=== Test 9: Memory Management (tidy) ===")
(let [result (.tidy mx (fn []
                         (let [a (.array mx #js [1 2 3] (.-float32 mx))
                               b (.multiply mx a a)]
                           (.add mx b a))))]
  (.eval mx result)
  (println "  tidy result:" (.toString result))
  (pass "tidy"))

;; ── Test 10: compile (JIT) ──
(println "\n=== Test 10: Compile (JIT) ===")
(let [f (fn [x] (.add mx (.multiply mx x x) x))
      compiled-f (.compile mx f)
      x (.array mx 5.0 (.-float32 mx))
      result (compiled-f x)]
  (.eval mx result)
  (let [v (.item result)]
    (if (approx= v 30.0)
      (pass (str "compiled f(5)=" v))
      (fail "compile" 30.0 v))))

;; ── Test 11: Second derivative ──
(println "\n=== Test 11: Second Derivative ===")
(let [f (fn [x] (.multiply mx (.multiply mx x x) x))
      df (.grad mx f)
      ddf (.grad mx df)
      x (.array mx 2.0 (.-float32 mx))
      result (ddf x)]
  (.eval mx result)
  (let [v (.item result)]
    (if (approx= v 12.0)
      (pass (str "f(x)=x^3, f''(2)=" v))
      (fail "second deriv" 12.0 v))))

;; ── Test 12: Cholesky decomposition ──
(println "\n=== Test 12: Cholesky ===")
(let [cpu-stream (.newStream mx (.-cpu mx))
      ;; Positive definite matrix [[2 1] [1 2]]
      m (.reshape mx (.array mx #js [2 1 1 2] (.-float32 mx)) #js [2 2])
      L (.cholesky mla m false cpu-stream)]
  (.eval mx L)
  (println "  cholesky of [[2,1],[1,2]]:" (.toString L))
  ;; Verify L * L^T = m
  (let [recon (.matmul mx L (.transpose mx L))
        diff (.item (.sum mx (.abs mx (.subtract mx recon m))))]
    (if (< diff 0.001)
      (pass (str "L*L^T reconstruction error=" diff))
      (fail "cholesky" "< 0.001" diff))))

;; ── Test 13: Solve linear system ──
(println "\n=== Test 13: Linear Solve ===")
(let [cpu-stream (.newStream mx (.-cpu mx))
      A (.reshape mx (.array mx #js [3 1 1 2] (.-float32 mx)) #js [2 2])
      b (.reshape mx (.array mx #js [9 8] (.-float32 mx)) #js [2 1])
      x (.solve mla A b cpu-stream)]
  (.eval mx x)
  (println "  solve [[3,1],[1,2]] x = [9,8]:" (.toString x))
  ;; x should be [2, 3]
  (let [x0 (.item (.take mx (.flatten mx x) (.array mx 0 (.-int32 mx))))
        x1 (.item (.take mx (.flatten mx x) (.array mx 1 (.-int32 mx))))]
    (if (and (approx= x0 2.0) (approx= x1 3.0))
      (pass (str "x=[" x0 "," x1 "]"))
      (fail "solve" "[2,3]" (str "[" x0 "," x1 "]")))))

;; ── Test 14: Negative/subtract ──
(println "\n=== Test 14: Negative/Subtract ===")
(let [a (.array mx #js [1 2 3] (.-float32 mx))
      neg (.negative mx a)
      sub (.subtract mx a (.array mx #js [1 1 1] (.-float32 mx)))]
  (.eval mx neg sub)
  (println "  negative:" (.toString neg))
  (println "  subtract:" (.toString sub))
  (pass "negative/subtract"))

;; ── Test 15: where (conditional) ──
(println "\n=== Test 15: Where (conditional) ===")
(let [cond-arr (.array mx #js [true false true])
      a (.array mx #js [1 2 3] (.-float32 mx))
      b (.array mx #js [10 20 30] (.-float32 mx))
      result (.where mx cond-arr a b)]
  (.eval mx result)
  (println "  where:" (.toString result))
  (pass "where"))

;; ── Test 16: Multivariate normal ──
;; multivariateNormal uses SVD internally, which needs a CPU stream.
;; We'll implement our own MVN via Cholesky in the distribution layer,
;; so just test that we can sample normal + do matmul (which is the
;; Cholesky approach: mean + L @ z where z ~ N(0,I)).
(println "\n=== Test 16: Multivariate Normal via Cholesky ===")
(let [cpu-stream (.newStream mx (.-cpu mx))
      ;; Covariance [[1 0.5] [0.5 1]], mean [0 0]
      mean-vec (.array mx #js [0 0] (.-float32 mx))
      cov (.reshape mx (.array mx #js [1 0.5 0.5 1] (.-float32 mx)) #js [2 2])
      L (.cholesky mla cov false cpu-stream)
      ;; Sample z ~ N(0, I) of shape [100, 2]
      z (.normal mrng #js [100 2])
      ;; samples = mean + z @ L^T
      samples (.add mx mean-vec (.matmul mx z (.transpose mx L)))]
  (.eval mx samples)
  (println "  samples shape:" (js->clj (.-shape samples)))
  (let [sample-mean (.mean mx samples #js [0])]
    (.eval mx sample-mean)
    (println "  sample mean (near [0,0]):" (.toString sample-mean)))
  (pass "multivariate normal via Cholesky"))

;; ── Test 17: stopGradient ──
(println "\n=== Test 17: stopGradient ===")
(let [f (fn [x]
          ;; f(x) = x^2 + stopGrad(x^2)
          ;; grad should be 2x (not 4x) because second term is stopped
          (.add mx (.multiply mx x x) (.stopGradient mx (.multiply mx x x))))
      grad-f (.grad mx f)
      x (.array mx 3.0 (.-float32 mx))
      result (grad-f x)]
  (.eval mx result)
  (let [v (.item result)]
    (if (approx= v 6.0)
      (pass (str "stopGradient: grad=" v " (not 12)"))
      (fail "stopGradient" 6.0 v))))

(println "\n=== All smoke tests complete ===")
