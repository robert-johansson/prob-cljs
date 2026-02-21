(ns gpu-autograd-test
  "Autograd tests: analytic gradients verified against finite differences."
  (:require [promesa.core :as p]
            [prob.gpu.device :as dev]
            [prob.gpu.tensor :as t]
            [prob.gpu.autograd :as ag]))

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
;; Finite difference helper
;; ---------------------------------------------------------------------------

(defn- finite-diff
  "Compute numerical gradient via central differences.
   f: (fn [flat-vec] -> Promise<number>) — takes flat clj vec, returns scalar.
   x-vec: flat clj vector of input values.
   Returns Promise<vector> of gradients."
  [f x-vec eps]
  (let [n (count x-vec)]
    (p/loop [i 0
             grads []]
      (if (>= i n)
        grads
        (p/let [x-plus  (assoc x-vec i (+ (nth x-vec i) eps))
                x-minus (assoc x-vec i (- (nth x-vec i) eps))
                f-plus  (f x-plus)
                f-minus (f x-minus)
                gi      (/ (- f-plus f-minus) (* 2 eps))]
          (p/recur (inc i) (conj grads gi)))))))

(defn- make-scalar-fn
  "Wrap a tensor function (tracked-tensor -> tracked-scalar) for finite-diff.
   Returns (fn [flat-clj-vec] -> Promise<number>)."
  [tensor-fn input-shape]
  (fn [x-vec]
    (let [x-t (if (empty? input-shape)
                (t/scalar (first x-vec))
                (t/tensor (if (= 1 (count input-shape))
                            x-vec
                            ;; reshape flat vec into nested
                            (let [[rows cols] input-shape]
                              (mapv (fn [r] (subvec x-vec (* r cols) (* (inc r) cols)))
                                    (range rows))))))]
      (p/let [result (tensor-fn x-t)
              v      (t/to-number result)]
        v))))

;; ---------------------------------------------------------------------------
;; Test: unary gradients
;; ---------------------------------------------------------------------------

(defn- test-unary-grads []
  (println "\n--- Unary Gradients ---")
  (p/let [;; f(x) = sum(square(x)) at x=[1,2,3] → grad = [2,4,6]
          x1 (t/tensor [1 2 3])
          g1 ((ag/grad (fn [x] (t/sum (t/square x)))) x1)
          vg1 (t/to-clj g1)
          _ (check "square grad" (arr-approx= vg1 [2 4 6] 0.01)
                   (str "expected [2 4 6], got " vg1))

          ;; f(x) = sum(exp(x)) at x=[0,1] → grad = [1, e]
          x2 (t/tensor [0 1])
          g2 ((ag/grad (fn [x] (t/sum (t/exp x)))) x2)
          vg2 (t/to-clj g2)
          _ (check "exp grad" (arr-approx= vg2 [1.0 2.71828] 0.01)
                   (str "expected [1.0, 2.718], got " vg2))

          ;; f(x) = sum(log(x)) at x=[1,2] → grad = [1, 0.5]
          x3 (t/tensor [1 2])
          g3 ((ag/grad (fn [x] (t/sum (t/log x)))) x3)
          vg3 (t/to-clj g3)
          _ (check "log grad" (arr-approx= vg3 [1.0 0.5] 0.01)
                   (str "expected [1.0, 0.5], got " vg3))

          ;; f(x) = sum(sqrt(x)) at x=[1,4] → grad = [0.5, 0.25]
          x4 (t/tensor [1 4])
          g4 ((ag/grad (fn [x] (t/sum (t/sqrt x)))) x4)
          vg4 (t/to-clj g4)
          _ (check "sqrt grad" (arr-approx= vg4 [0.5 0.25] 0.01)
                   (str "expected [0.5, 0.25], got " vg4))

          ;; f(x) = sum(negative(x)) at x=[1,2,3] → grad = [-1,-1,-1]
          x5 (t/tensor [1 2 3])
          g5 ((ag/grad (fn [x] (t/sum (t/negative x)))) x5)
          vg5 (t/to-clj g5)
          _ (check "negative grad" (arr-approx= vg5 [-1 -1 -1] 0.01)
                   (str "expected [-1 -1 -1], got " vg5))

          ;; f(x) = sum(abs(x)) at x=[-2,1,3] → grad = [-1,1,1]
          x6 (t/tensor [-2 1 3])
          g6 ((ag/grad (fn [x] (t/sum (t/abs x)))) x6)
          vg6 (t/to-clj g6)
          _ (check "abs grad" (arr-approx= vg6 [-1 1 1] 0.01)
                   (str "expected [-1 1 1], got " vg6))]
    nil))

;; ---------------------------------------------------------------------------
;; Test: binary gradients
;; ---------------------------------------------------------------------------

(defn- test-binary-grads []
  (println "\n--- Binary Gradients ---")
  (p/let [;; f(x) = sum(add(x, x)) → grad = [2,2,2] (fan-out test)
          x1 (t/tensor [1 2 3])
          g1 ((ag/grad (fn [x] (t/sum (t/add x x)))) x1)
          vg1 (t/to-clj g1)
          _ (check "add fan-out grad" (arr-approx= vg1 [2 2 2] 0.01)
                   (str "expected [2 2 2], got " vg1))

          ;; f(x) = sum(multiply(x, x)) → grad = 2x (fan-out)
          x2 (t/tensor [1 2 3])
          g2 ((ag/grad (fn [x] (t/sum (t/multiply x x)))) x2)
          vg2 (t/to-clj g2)
          _ (check "multiply fan-out grad" (arr-approx= vg2 [2 4 6] 0.01)
                   (str "expected [2 4 6], got " vg2))

          ;; f(x) = sum(subtract(x, const)) → grad = [1,1,1]
          c1 (t/tensor [10 20 30])
          x3 (t/tensor [1 2 3])
          g3 ((ag/grad (fn [x] (t/sum (t/subtract x c1)))) x3)
          vg3 (t/to-clj g3)
          _ (check "subtract grad" (arr-approx= vg3 [1 1 1] 0.01)
                   (str "expected [1 1 1], got " vg3))

          ;; f(x) = sum(divide(x, const)) where const=[2,4,5] → grad = [0.5, 0.25, 0.2]
          c2 (t/tensor [2 4 5])
          x4 (t/tensor [6 8 10])
          g4 ((ag/grad (fn [x] (t/sum (t/divide x c2)))) x4)
          vg4 (t/to-clj g4)
          _ (check "divide grad" (arr-approx= vg4 [0.5 0.25 0.2] 0.01)
                   (str "expected [0.5 0.25 0.2], got " vg4))]
    nil))

;; ---------------------------------------------------------------------------
;; Test: broadcast gradients
;; ---------------------------------------------------------------------------

(defn- test-broadcast-grads []
  (println "\n--- Broadcast Gradients ---")
  (p/let [;; f(scalar) = sum(add(const_vec, scalar)) where vec is [4] → scalar grad = 4
          c1 (t/tensor [1 2 3 4])
          x1 (t/scalar 10)
          g1 ((ag/grad (fn [s] (t/sum (t/add c1 s)))) x1)
          vg1 (t/to-number g1)
          _ (check "broadcast scalar grad (add)" (approx= vg1 4 0.01)
                   (str "expected 4, got " vg1))

          ;; f(scalar) = sum(multiply(const_vec, scalar)) → grad = sum(const_vec)
          c2 (t/tensor [1 2 3 4])
          x2 (t/scalar 2)
          g2 ((ag/grad (fn [s] (t/sum (t/multiply c2 s)))) x2)
          vg2 (t/to-number g2)
          _ (check "broadcast scalar grad (multiply)" (approx= vg2 10 0.01)
                   (str "expected 10, got " vg2))]
    nil))

;; ---------------------------------------------------------------------------
;; Test: composition (chain rule)
;; ---------------------------------------------------------------------------

(defn- test-composition-grads []
  (println "\n--- Composition Gradients ---")
  (p/let [;; f(x) = sum(exp(multiply(x, scalar(2)))) at x=[0,1]
          ;; → grad = 2*exp(2x) = [2, 2*e^2]
          x1 (t/tensor [0 1])
          g1 ((ag/grad (fn [x]
                         (let [two (t/scalar 2.0)]
                           (t/sum (t/exp (t/multiply x two)))))) x1)
          vg1 (t/to-clj g1)
          _ (check "chain rule exp(2x)" (arr-approx= vg1 [2.0 (* 2 (js/Math.exp 2))] 0.05)
                   (str "expected [2, " (* 2 (js/Math.exp 2)) "], got " vg1))

          ;; f(x) = sum(square(add(x, ones))) at x=[0,1,2]
          ;; → grad = 2*(x+1) = [2, 4, 6]
          x2 (t/tensor [0 1 2])
          g2 ((ag/grad (fn [x]
                         (let [o (t/ones [3])]
                           (t/sum (t/square (t/add x o)))))) x2)
          vg2 (t/to-clj g2)
          _ (check "chain rule square(x+1)" (arr-approx= vg2 [2 4 6] 0.01)
                   (str "expected [2 4 6], got " vg2))]
    nil))

;; ---------------------------------------------------------------------------
;; Test: matmul gradients
;; ---------------------------------------------------------------------------

(defn- test-matmul-grads []
  (println "\n--- Matmul Gradients ---")
  (p/let [;; f(X) = sum(matmul(X, W)) for fixed W[2,3], X[2,2]
          ;; grad_X = ones[2,3] @ W^T[3,2] = [[sum(W_col0) sum(W_col1)] ...]
          ;; W = [[1 2 3] [4 5 6]], W^T = [[1 4] [2 5] [3 6]]
          ;; ones[2,3] @ W^T = [[1+2+3 4+5+6] [1+2+3 4+5+6]] = [[6 15] [6 15]]
          w (t/tensor [[1 2 3] [4 5 6]])
          x1 (t/tensor [[1 0] [0 1]])
          g1 ((ag/grad (fn [x] (t/sum (t/matmul x w)))) x1)
          vg1 (t/to-clj g1)
          _ (check "matmul grad X shape" (= (t/shape g1) [2 2])
                   (str "expected [2 2], got " (t/shape g1)))
          _ (check "matmul grad X row0" (arr-approx= (first vg1) [6 15] 0.1)
                   (str "expected [6 15], got " (first vg1)))
          _ (check "matmul grad X row1" (arr-approx= (second vg1) [6 15] 0.1)
                   (str "expected [6 15], got " (second vg1)))

          ;; f(W) = sum(matmul(X, W)) for fixed X[2,2], W[2,3]
          ;; grad_W = X^T[2,2] @ ones[2,3]
          ;; X = [[1 0] [0 1]], X^T = [[1 0] [0 1]]
          ;; X^T @ ones = [[1 1 1] [1 1 1]]
          x2 (t/tensor [[1 0] [0 1]])
          w2 (t/tensor [[1 2 3] [4 5 6]])
          g2 ((ag/grad (fn [ww] (t/sum (t/matmul x2 ww)))) w2)
          vg2 (t/to-clj g2)
          _ (check "matmul grad W shape" (= (t/shape g2) [2 3])
                   (str "expected [2 3], got " (t/shape g2)))
          _ (check "matmul grad W row0" (arr-approx= (first vg2) [1 1 1] 0.1)
                   (str "expected [1 1 1], got " (first vg2)))
          _ (check "matmul grad W row1" (arr-approx= (second vg2) [1 1 1] 0.1)
                   (str "expected [1 1 1], got " (second vg2)))]
    nil))

;; ---------------------------------------------------------------------------
;; Test: transpose gradient
;; ---------------------------------------------------------------------------

(defn- test-transpose-grads []
  (println "\n--- Transpose Gradients ---")
  (p/let [;; f(X) = sum(transpose(X)) → grad = ones with original shape
          x1 (t/tensor [[1 2 3] [4 5 6]])
          g1 ((ag/grad (fn [x] (t/sum (t/transpose x)))) x1)
          vg1 (t/to-clj g1)
          _ (check "transpose grad shape" (= (t/shape g1) [2 3])
                   (str "expected [2 3], got " (t/shape g1)))
          _ (check "transpose grad row0" (arr-approx= (first vg1) [1 1 1] 0.01)
                   (str "expected [1 1 1], got " (first vg1)))
          _ (check "transpose grad row1" (arr-approx= (second vg1) [1 1 1] 0.01)
                   (str "expected [1 1 1], got " (second vg1)))]
    nil))

;; ---------------------------------------------------------------------------
;; Test: reshape gradient
;; ---------------------------------------------------------------------------

(defn- test-reshape-grads []
  (println "\n--- Reshape Gradients ---")
  (p/let [;; f(x) = sum(square(reshape(x, [2,2]))) at x=[1,2,3,4]
          ;; → grad same as without reshape: [2,4,6,8]
          x1 (t/tensor [1 2 3 4])
          g1 ((ag/grad (fn [x] (t/sum (t/square (t/reshape x [2 2]))))) x1)
          vg1 (t/to-clj g1)
          _ (check "reshape grad" (arr-approx= vg1 [2 4 6 8] 0.01)
                   (str "expected [2 4 6 8], got " vg1))
          _ (check "reshape grad shape" (= (t/shape g1) [4])
                   (str "expected [4], got " (t/shape g1)))]
    nil))

;; ---------------------------------------------------------------------------
;; Test: reduction gradients
;; ---------------------------------------------------------------------------

(defn- test-reduction-grads []
  (println "\n--- Reduction Gradients ---")
  (p/let [;; f(x) = sum(x) → grad = [1,1,...,1]
          x1 (t/tensor [3 5 7])
          g1 ((ag/grad (fn [x] (t/sum x))) x1)
          vg1 (t/to-clj g1)
          _ (check "sum grad" (arr-approx= vg1 [1 1 1] 0.01)
                   (str "expected [1 1 1], got " vg1))

          ;; f(x) = mean(x) → grad = [1/n, 1/n, ...]
          x2 (t/tensor [3 5 7 9])
          g2 ((ag/grad (fn [x] (t/mean x))) x2)
          vg2 (t/to-clj g2)
          _ (check "mean grad" (arr-approx= vg2 [0.25 0.25 0.25 0.25] 0.01)
                   (str "expected [0.25 0.25 0.25 0.25], got " vg2))]
    nil))

;; ---------------------------------------------------------------------------
;; Test: value-and-grad
;; ---------------------------------------------------------------------------

(defn- test-value-and-grad []
  (println "\n--- Value and Grad ---")
  (p/let [x (t/tensor [1 2 3])
          vg-fn (ag/value-and-grad (fn [x] (t/sum (t/square x))))
          [loss grad] (vg-fn x)
          v-loss (t/to-number loss)
          v-grad (t/to-clj grad)
          _ (check "value-and-grad loss" (approx= v-loss 14 0.01)
                   (str "expected 14, got " v-loss))
          _ (check "value-and-grad grad" (arr-approx= v-grad [2 4 6] 0.01)
                   (str "expected [2 4 6], got " v-grad))]
    nil))

;; ---------------------------------------------------------------------------
;; Test: finite difference verification
;; ---------------------------------------------------------------------------

(defn- test-finite-diff []
  (println "\n--- Finite Difference Verification ---")
  (let [eps 1e-3
        tol 0.05]
    (p/let [;; square
            x-vec1 [1 2 3]
            f1 (fn [x] (t/sum (t/square x)))
            analytic1 (p/let [g ((ag/grad f1) (t/tensor x-vec1))] (t/to-clj g))
            numeric1  (finite-diff (make-scalar-fn f1 [3]) x-vec1 eps)
            _ (check "finite-diff square" (arr-approx= analytic1 numeric1 tol)
                     (str "analytic=" analytic1 " numeric=" numeric1))

            ;; exp
            x-vec2 [0 0.5 1]
            f2 (fn [x] (t/sum (t/exp x)))
            analytic2 (p/let [g ((ag/grad f2) (t/tensor x-vec2))] (t/to-clj g))
            numeric2  (finite-diff (make-scalar-fn f2 [3]) x-vec2 eps)
            _ (check "finite-diff exp" (arr-approx= analytic2 numeric2 tol)
                     (str "analytic=" analytic2 " numeric=" numeric2))

            ;; log
            x-vec3 [1 2 3]
            f3 (fn [x] (t/sum (t/log x)))
            analytic3 (p/let [g ((ag/grad f3) (t/tensor x-vec3))] (t/to-clj g))
            numeric3  (finite-diff (make-scalar-fn f3 [3]) x-vec3 eps)
            _ (check "finite-diff log" (arr-approx= analytic3 numeric3 tol)
                     (str "analytic=" analytic3 " numeric=" numeric3))

            ;; multiply
            x-vec4 [1 2 3]
            c-mul  (t/tensor [4 5 6])
            f4 (fn [x] (t/sum (t/multiply x c-mul)))
            analytic4 (p/let [g ((ag/grad f4) (t/tensor x-vec4))] (t/to-clj g))
            numeric4  (finite-diff (make-scalar-fn f4 [3]) x-vec4 eps)
            _ (check "finite-diff multiply" (arr-approx= analytic4 numeric4 tol)
                     (str "analytic=" analytic4 " numeric=" numeric4))

            ;; divide
            x-vec5 [2 4 6]
            c-div  (t/tensor [2 4 3])
            f5 (fn [x] (t/sum (t/divide x c-div)))
            analytic5 (p/let [g ((ag/grad f5) (t/tensor x-vec5))] (t/to-clj g))
            numeric5  (finite-diff (make-scalar-fn f5 [3]) x-vec5 eps)
            _ (check "finite-diff divide" (arr-approx= analytic5 numeric5 tol)
                     (str "analytic=" analytic5 " numeric=" numeric5))

            ;; composition: sum(exp(2*x))
            x-vec6 [0 0.5]
            f6 (fn [x] (let [two (t/scalar 2.0)]
                          (t/sum (t/exp (t/multiply x two)))))
            analytic6 (p/let [g ((ag/grad f6) (t/tensor x-vec6))] (t/to-clj g))
            numeric6  (finite-diff (make-scalar-fn f6 [2]) x-vec6 eps)
            _ (check "finite-diff exp(2x)" (arr-approx= analytic6 numeric6 tol)
                     (str "analytic=" analytic6 " numeric=" numeric6))

            ;; sqrt
            x-vec7 [1 4 9]
            f7 (fn [x] (t/sum (t/sqrt x)))
            analytic7 (p/let [g ((ag/grad f7) (t/tensor x-vec7))] (t/to-clj g))
            numeric7  (finite-diff (make-scalar-fn f7 [3]) x-vec7 eps)
            _ (check "finite-diff sqrt" (arr-approx= analytic7 numeric7 tol)
                     (str "analytic=" analytic7 " numeric=" numeric7))

            ;; mean
            x-vec8 [2 4 6 8]
            f8 (fn [x] (t/mean x))
            analytic8 (p/let [g ((ag/grad f8) (t/tensor x-vec8))] (t/to-clj g))
            numeric8  (finite-diff (make-scalar-fn f8 [4]) x-vec8 eps)
            _ (check "finite-diff mean" (arr-approx= analytic8 numeric8 tol)
                     (str "analytic=" analytic8 " numeric=" numeric8))]
      nil)))

;; ---------------------------------------------------------------------------
;; Main
;; ---------------------------------------------------------------------------

(println "\n=== GPU Autograd Tests ===")

(p/let [_ (dev/init!)
        _ (test-unary-grads)
        _ (test-binary-grads)
        _ (test-broadcast-grads)
        _ (test-composition-grads)
        _ (test-matmul-grads)
        _ (test-transpose-grads)
        _ (test-reshape-grads)
        _ (test-reduction-grads)
        _ (test-value-and-grad)
        _ (test-finite-diff)]
  (println (str "\n" @passed " passed, " @failed " failed"))
  (when (pos? @failed)
    (js/process.exit 1)))
