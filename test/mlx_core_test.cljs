(ns mlx-core-test
  "Test prob.mlx.core wrapper."
  (:require [prob.mlx.core :as mx]))

(defn pass [name] (println (str "  PASS: " name)))
(defn fail [name msg] (println (str "  FAIL: " name " - " msg)))
(defn approx= [a b] (< (js/Math.abs (- a b)) 0.01))

;; ── Array creation ──
(println "\n=== Array Creation ===")
(let [a (mx/array [1 2 3 4 5])
      z (mx/zeros [3])
      o (mx/ones [2 2])
      f (mx/full [3] 7.0)
      e (mx/eye 3)
      r (mx/arange 5)]
  (mx/eval! a z o f e r)
  (println "  array:" (mx/->clj a))
  (println "  zeros:" (mx/->clj z))
  (println "  ones:" (mx/->clj o))
  (println "  full:" (mx/->clj f))
  (println "  eye:" (mx/->clj e))
  (println "  arange:" (mx/->clj r))
  (if (= (mx/->clj a) [1 2 3 4 5])
    (pass "array creation")
    (fail "array creation" (str (mx/->clj a)))))

;; ── Arithmetic ──
(println "\n=== Arithmetic ===")
(let [a (mx/array [1 2 3])
      b (mx/array [10 20 30])
      s (mx/add a b)
      p (mx/multiply a b)
      sq (mx/square a)]
  (mx/eval! s p sq)
  (if (= (mx/->clj s) [11 22 33])
    (pass "add")
    (fail "add" (mx/->clj s)))
  (if (= (mx/->clj p) [10 40 90])
    (pass "multiply")
    (fail "multiply" (mx/->clj p))))

;; ── Reductions ──
(println "\n=== Reductions ===")
(let [a (mx/array [1 2 3 4 5])]
  (let [s (mx/item (mx/sum a))
        m (mx/item (mx/mean a))]
    (if (and (== s 15) (approx= m 3.0))
      (pass (str "sum=" s " mean=" m))
      (fail "reductions" (str "sum=" s " mean=" m)))))

;; ── Shape ──
(println "\n=== Shape ===")
(let [a (mx/array [1 2 3])
      m (mx/reshape a [1 3])]
  (if (= (mx/shape a) [3])
    (pass (str "shape=" (mx/shape a)))
    (fail "shape" (mx/shape a)))
  (if (= (mx/shape m) [1 3])
    (pass (str "reshape=" (mx/shape m)))
    (fail "reshape" (mx/shape m))))

;; ── Matmul ──
(println "\n=== Matmul ===")
(let [a (mx/reshape (mx/array [1 2 3 4]) [2 2])
      b (mx/reshape (mx/array [5 6 7 8]) [2 2])
      c (mx/matmul a b)]
  (mx/eval! c)
  (if (= (mx/->clj c) [[19 22] [43 50]])
    (pass "matmul")
    (fail "matmul" (mx/->clj c))))

;; ── Grad ──
(println "\n=== Autograd ===")
(let [f (fn [x] (mx/multiply x x))
      df (mx/grad f)
      x (mx/scalar 3.0)
      result (df x)]
  (mx/eval! result)
  (let [v (mx/item result)]
    (if (approx= v 6.0)
      (pass (str "grad f(x)=x^2, f'(3)=" v))
      (fail "grad" v))))

;; ── value-and-grad ──
(println "\n=== value-and-grad ===")
(let [f (fn [x] (mx/sum (mx/multiply x x)))
      vg (mx/value-and-grad f)
      [val grad-arr] (vg (mx/array [1 2 3]))]
  (mx/eval! val grad-arr)
  (let [v (mx/item val)
        g (mx/->clj grad-arr)]
    (if (and (approx= v 14.0) (= g [2 4 6]))
      (pass (str "val=" v " grad=" g))
      (fail "value-and-grad" (str "val=" v " grad=" g)))))

;; ── Cholesky ──
(println "\n=== Cholesky ===")
(let [m (mx/reshape (mx/array [2 1 1 2]) [2 2])
      L (mx/cholesky m)]
  (mx/eval! L)
  (let [recon (mx/matmul L (mx/transpose L))
        diff (mx/item (mx/sum (mx/abs (mx/subtract recon m))))]
    (if (< diff 0.001)
      (pass (str "cholesky error=" diff))
      (fail "cholesky" diff))))

;; ── Random ──
(println "\n=== Random ===")
(let [u (mx/random-uniform [5])
      n (mx/random-normal [5])]
  (mx/eval! u n)
  (println "  uniform:" (mx/->clj u))
  (println "  normal:" (mx/->clj n))
  (pass "random"))

;; ── Tidy ──
(println "\n=== Tidy ===")
(let [result (mx/tidy (fn []
                        (let [a (mx/array [1 2 3])
                              b (mx/multiply a a)]
                          (mx/add b a))))]
  (mx/eval! result)
  (if (= (mx/->clj result) [2 6 12])
    (pass "tidy")
    (fail "tidy" (mx/->clj result))))

;; ── Compile ──
(println "\n=== Compile ===")
(let [f (fn [x] (mx/add (mx/multiply x x) x))
      cf (mx/compile-fn f)
      result (cf (mx/scalar 5.0))]
  (mx/eval! result)
  (let [v (mx/item result)]
    (if (approx= v 30.0)
      (pass (str "compiled f(5)=" v))
      (fail "compile" v))))

;; ── Second derivative ──
(println "\n=== Second Derivative ===")
(let [f (fn [x] (mx/multiply (mx/multiply x x) x))
      ddf (mx/grad (mx/grad f))
      x (mx/scalar 2.0)
      result (ddf x)]
  (mx/eval! result)
  (let [v (mx/item result)]
    (if (approx= v 12.0)
      (pass (str "f''(2)=" v))
      (fail "second deriv" v))))

;; ── logsumexp ──
(println "\n=== LogSumExp ===")
(let [a (mx/array [1 2 3])
      result (mx/logsumexp a)]
  (mx/eval! result)
  (let [v (mx/item result)
        expected (js/Math.log (+ (js/Math.exp 1) (js/Math.exp 2) (js/Math.exp 3)))]
    (if (approx= v expected)
      (pass (str "logsumexp=" v))
      (fail "logsumexp" (str "got " v " expected " expected)))))

;; ── Stop gradient ──
(println "\n=== Stop Gradient ===")
(let [f (fn [x] (mx/add (mx/multiply x x) (mx/stop-gradient (mx/multiply x x))))
      df (mx/grad f)
      result (df (mx/scalar 3.0))]
  (mx/eval! result)
  (let [v (mx/item result)]
    (if (approx= v 6.0)
      (pass (str "stop-gradient: grad=" v))
      (fail "stop-gradient" v))))

(println "\n=== All core wrapper tests complete ===")
