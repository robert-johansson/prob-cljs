(ns gpu-tensor-test
  "GPU tensor runtime tests.
   Tests creation, arithmetic, comparisons, reductions, shape ops, RNG, and disposal."
  (:require [promesa.core :as p]
            [prob.gpu.device :as dev]
            [prob.gpu.tensor :as t]))

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
;; 1. Creation + readback
;; ---------------------------------------------------------------------------

(defn- test-creation []
  (println "\n--- Creation + Readback ---")
  (p/let [;; flat tensor
          a  (t/tensor [1 2 3 4])
          va (t/to-clj a)
          _  (check "tensor flat" (arr-approx= va [1 2 3 4] 0.001)
                     (str "expected [1 2 3 4], got " va))

          ;; nested tensor
          b  (t/tensor [[1 2] [3 4]])
          vb (t/to-clj b)
          _  (check "tensor nested" (and (arr-approx= (first vb) [1 2] 0.001)
                                         (arr-approx= (second vb) [3 4] 0.001))
                     (str "expected [[1 2] [3 4]], got " vb))

          ;; scalar
          s  (t/scalar 42)
          vs (t/to-number s)
          _  (check "scalar" (approx= vs 42 0.001)
                     (str "expected 42, got " vs))

          ;; zeros
          z  (t/zeros [3])
          vz (t/to-clj z)
          _  (check "zeros" (arr-approx= vz [0 0 0] 0.001)
                     (str "expected [0 0 0], got " vz))

          ;; ones
          o  (t/ones [4])
          vo (t/to-clj o)
          _  (check "ones" (arr-approx= vo [1 1 1 1] 0.001)
                     (str "expected [1 1 1 1], got " vo))

          ;; full
          f  (t/full [3] 7.0)
          vf (t/to-clj f)
          _  (check "full" (arr-approx= vf [7 7 7] 0.001)
                     (str "expected [7 7 7], got " vf))

          ;; shape / ndim / size / tensor?
          _  (check "shape" (= (t/shape a) [4]) (str "expected [4], got " (t/shape a)))
          _  (check "shape nested" (= (t/shape b) [2 2]) (str "expected [2 2], got " (t/shape b)))
          _  (check "ndim" (= (t/ndim a) 1) (str "expected 1, got " (t/ndim a)))
          _  (check "ndim nested" (= (t/ndim b) 2) (str "expected 2, got " (t/ndim b)))
          _  (check "size" (= (t/size a) 4) (str "expected 4, got " (t/size a)))
          _  (check "tensor?" (t/tensor? a) "expected true")
          _  (check "tensor? false" (not (t/tensor? 42)) "expected false")]
    ;; cleanup
    (doseq [x [a b s z o f]] (t/dispose! x))))

;; ---------------------------------------------------------------------------
;; 2. Binary arithmetic
;; ---------------------------------------------------------------------------

(defn- test-binary-arithmetic []
  (println "\n--- Binary Arithmetic ---")
  (p/let [a (t/tensor [1 2 3 4])
          b (t/tensor [10 20 30 40])
          s (t/scalar 2)

          ;; same-size ops
          r-add (t/add a b)
          v-add (t/to-clj r-add)
          _ (check "add" (arr-approx= v-add [11 22 33 44] 0.001)
                   (str "expected [11 22 33 44], got " v-add))

          r-sub (t/subtract b a)
          v-sub (t/to-clj r-sub)
          _ (check "subtract" (arr-approx= v-sub [9 18 27 36] 0.001)
                   (str "expected [9 18 27 36], got " v-sub))

          r-mul (t/multiply a b)
          v-mul (t/to-clj r-mul)
          _ (check "multiply" (arr-approx= v-mul [10 40 90 160] 0.001)
                   (str "expected [10 40 90 160], got " v-mul))

          r-div (t/divide b a)
          v-div (t/to-clj r-div)
          _ (check "divide" (arr-approx= v-div [10 10 10 10] 0.001)
                   (str "expected [10 10 10 10], got " v-div))

          ;; scalar broadcast
          r-add-s (t/add a s)
          v-add-s (t/to-clj r-add-s)
          _ (check "add scalar broadcast" (arr-approx= v-add-s [3 4 5 6] 0.001)
                   (str "expected [3 4 5 6], got " v-add-s))

          r-mul-s (t/multiply a s)
          v-mul-s (t/to-clj r-mul-s)
          _ (check "multiply scalar broadcast" (arr-approx= v-mul-s [2 4 6 8] 0.001)
                   (str "expected [2 4 6 8], got " v-mul-s))

          ;; scalar-scalar
          s2 (t/scalar 3)
          r-ss (t/add s s2)
          v-ss (t/to-number r-ss)
          _ (check "add scalar-scalar" (approx= v-ss 5 0.001)
                   (str "expected 5, got " v-ss))]

    (doseq [x [a b s r-add r-sub r-mul r-div r-add-s r-mul-s s2 r-ss]]
      (t/dispose! x))))

;; ---------------------------------------------------------------------------
;; 3. Unary ops
;; ---------------------------------------------------------------------------

(defn- test-unary-ops []
  (println "\n--- Unary Ops ---")
  (p/let [a (t/tensor [1 2 3 4])

          r-neg (t/negative a)
          v-neg (t/to-clj r-neg)
          _ (check "negative" (arr-approx= v-neg [-1 -2 -3 -4] 0.001)
                   (str "expected [-1 -2 -3 -4], got " v-neg))

          b (t/tensor [0 1 2 3])
          r-exp (t/exp b)
          v-exp (t/to-clj r-exp)
          _ (check "exp" (arr-approx= v-exp [1.0 2.71828 7.38906 20.08554] 0.01)
                   (str "got " v-exp))

          c (t/tensor [1 2.71828 7.38906])
          r-log (t/log c)
          v-log (t/to-clj r-log)
          _ (check "log" (arr-approx= v-log [0 1.0 2.0] 0.01)
                   (str "got " v-log))

          d (t/tensor [1 4 9 16])
          r-sqrt (t/sqrt d)
          v-sqrt (t/to-clj r-sqrt)
          _ (check "sqrt" (arr-approx= v-sqrt [1 2 3 4] 0.001)
                   (str "expected [1 2 3 4], got " v-sqrt))

          e (t/tensor [2 3 4 5])
          r-sq (t/square e)
          v-sq (t/to-clj r-sq)
          _ (check "square" (arr-approx= v-sq [4 9 16 25] 0.001)
                   (str "expected [4 9 16 25], got " v-sq))

          f (t/tensor [-3 -1 0 2])
          r-abs (t/abs f)
          v-abs (t/to-clj r-abs)
          _ (check "abs" (arr-approx= v-abs [3 1 0 2] 0.001)
                   (str "expected [3 1 0 2], got " v-abs))]

    (doseq [x [a b c d e f r-neg r-exp r-log r-sqrt r-sq r-abs]]
      (t/dispose! x))))

;; ---------------------------------------------------------------------------
;; 4. Comparisons + where
;; ---------------------------------------------------------------------------

(defn- test-comparisons []
  (println "\n--- Comparisons + Where ---")
  (p/let [a (t/tensor [1 5 3 7])
          b (t/tensor [2 4 3 8])

          r-gt (t/greater a b)
          v-gt (t/to-clj r-gt)
          _ (check "greater" (arr-approx= v-gt [0 1 0 0] 0.001)
                   (str "expected [0 1 0 0], got " v-gt))

          r-lt (t/less a b)
          v-lt (t/to-clj r-lt)
          _ (check "less" (arr-approx= v-lt [1 0 0 1] 0.001)
                   (str "expected [1 0 0 1], got " v-lt))

          r-ge (t/greater-equal a b)
          v-ge (t/to-clj r-ge)
          _ (check "greater-equal" (arr-approx= v-ge [0 1 1 0] 0.001)
                   (str "expected [0 1 1 0], got " v-ge))

          r-le (t/less-equal a b)
          v-le (t/to-clj r-le)
          _ (check "less-equal" (arr-approx= v-le [1 0 1 1] 0.001)
                   (str "expected [1 0 1 1], got " v-le))

          ;; where: cond > 0 -> take a, else b
          cond-t (t/tensor [1 0 1 0])
          x (t/tensor [10 20 30 40])
          y (t/tensor [100 200 300 400])
          r-where (t/where cond-t x y)
          v-where (t/to-clj r-where)
          _ (check "where" (arr-approx= v-where [10 200 30 400] 0.001)
                   (str "expected [10 200 30 400], got " v-where))

          ;; where with broadcast
          s-cond (t/scalar 1)
          r-where-b (t/where s-cond x y)
          v-where-b (t/to-clj r-where-b)
          _ (check "where broadcast" (arr-approx= v-where-b [10 20 30 40] 0.001)
                   (str "expected [10 20 30 40], got " v-where-b))]

    (doseq [x' [a b r-gt r-lt r-ge r-le cond-t x y r-where s-cond r-where-b]]
      (t/dispose! x'))))

;; ---------------------------------------------------------------------------
;; 5. Reductions
;; ---------------------------------------------------------------------------

(defn- test-reductions []
  (println "\n--- Reductions ---")
  (p/let [;; sum small
          a (t/tensor [1 2 3 4])
          r-sum (t/sum a)
          v-sum (t/to-number r-sum)
          _ (check "sum small" (approx= v-sum 10 0.001)
                   (str "expected 10, got " v-sum))

          ;; sum single element
          s (t/scalar 42)
          r-sum-s (t/sum s)
          v-sum-s (t/to-number r-sum-s)
          _ (check "sum single" (approx= v-sum-s 42 0.001)
                   (str "expected 42, got " v-sum-s))

          ;; sum large (multi-pass: 1024 > 64)
          big (t/ones [1024])
          r-sum-big (t/sum big)
          v-sum-big (t/to-number r-sum-big)
          _ (check "sum large (1024)" (approx= v-sum-big 1024 0.1)
                   (str "expected 1024, got " v-sum-big))

          ;; mean
          b (t/tensor [2 4 6 8])
          r-mean (t/mean b)
          v-mean (t/to-number r-mean)
          _ (check "mean" (approx= v-mean 5 0.001)
                   (str "expected 5, got " v-mean))]

    (doseq [x [a r-sum s r-sum-s big r-sum-big b r-mean]]
      (t/dispose! x))))

;; ---------------------------------------------------------------------------
;; 6. Shape ops
;; ---------------------------------------------------------------------------

(defn- test-shape-ops []
  (println "\n--- Shape Ops ---")
  (p/let [a (t/tensor [1 2 3 4 5 6])
          _ (check "original shape" (= (t/shape a) [6]) (str "got " (t/shape a)))

          ;; reshape to 2x3
          b (t/reshape a [2 3])
          _ (check "reshape shape" (= (t/shape b) [2 3]) (str "got " (t/shape b)))
          _ (check "reshape ndim" (= (t/ndim b) 2) (str "got " (t/ndim b)))
          _ (check "reshape size" (= (t/size b) 6) (str "got " (t/size b)))

          ;; readback preserves data
          vb (t/to-clj b)
          _ (check "reshape data" (and (arr-approx= (first vb) [1 2 3] 0.001)
                                       (arr-approx= (second vb) [4 5 6] 0.001))
                   (str "got " vb))

          ;; flatten (reshape to [n])
          c (t/reshape b [6])
          vc (t/to-clj c)
          _ (check "flatten" (arr-approx= vc [1 2 3 4 5 6] 0.001)
                   (str "got " vc))

          ;; reshape round-trip
          d (t/reshape (t/reshape a [3 2]) [6])
          vd (t/to-clj d)
          _ (check "reshape round-trip" (arr-approx= vd [1 2 3 4 5 6] 0.001)
                   (str "got " vd))]

    ;; b, c, d share a's buffer so only dispose a
    (t/dispose! a)))

;; ---------------------------------------------------------------------------
;; 7. RNG
;; ---------------------------------------------------------------------------

(defn- test-rng []
  (println "\n--- RNG ---")
  (p/let [;; uniform shape check
          u (t/rand-uniform [100])
          _ (check "uniform shape" (= (t/shape u) [100]) (str "got " (t/shape u)))
          _ (check "uniform size" (= (t/size u) 100) (str "got " (t/size u)))

          ;; uniform range [0, 1)
          vu (t/to-clj u)
          _ (check "uniform range" (every? #(and (>= % 0.0) (< % 1.0)) vu)
                   "some values outside [0, 1)")

          ;; normal shape check
          n (t/randn [100])
          _ (check "randn shape" (= (t/shape n) [100]) (str "got " (t/shape n)))

          ;; statistical check with more samples
          big-n (t/randn [10000])
          r-mean (t/mean big-n)
          v-mean (t/to-number r-mean)
          _ (check "randn mean ~0" (< (js/Math.abs v-mean) 0.1)
                   (str "expected ~0, got " v-mean))

          ;; std dev check: mean of squares should be ~1
          sq (t/square big-n)
          r-var (t/mean sq)
          v-var (t/to-number r-var)
          _ (check "randn std ~1" (< (js/Math.abs (- v-var 1.0)) 0.15)
                   (str "expected variance ~1, got " v-var))]

    (doseq [x [u n big-n r-mean sq r-var]]
      (t/dispose! x))))

;; ---------------------------------------------------------------------------
;; 8. Larger arrays
;; ---------------------------------------------------------------------------

(defn- test-larger-arrays []
  (println "\n--- Larger Arrays ---")
  (p/let [;; 1024-element add
          a (t/full [1024] 3.0)
          b (t/full [1024] 7.0)
          r (t/add a b)
          r-sum (t/sum r)
          v-sum (t/to-number r-sum)
          _ (check "1024 add+sum" (approx= v-sum (* 1024 10.0) 1.0)
                   (str "expected " (* 1024 10.0) ", got " v-sum))

          ;; exp-then-log round-trip
          c (t/tensor [0.5 1.0 2.0 3.0])
          r-exp (t/exp c)
          r-log (t/log r-exp)
          v-rt (t/to-clj r-log)
          _ (check "exp-log round-trip" (arr-approx= v-rt [0.5 1.0 2.0 3.0] 0.001)
                   (str "got " v-rt))]

    (doseq [x [a b r r-sum c r-exp r-log]]
      (t/dispose! x))))

;; ---------------------------------------------------------------------------
;; 9. Dispose
;; ---------------------------------------------------------------------------

(defn- test-dispose []
  (println "\n--- Dispose ---")
  (let [a (t/tensor [1 2 3])]
    (try
      (t/dispose! a)
      (pass "dispose no-throw")
      (catch :default e
        (fail "dispose no-throw" (str e))))))

;; ---------------------------------------------------------------------------
;; 10. Matmul
;; ---------------------------------------------------------------------------

(defn- test-matmul []
  (println "\n--- Matmul ---")
  (p/let [;; 2x2 matmul
          a (t/tensor [[1 2] [3 4]])
          b (t/tensor [[5 6] [7 8]])
          r (t/matmul a b)
          vr (t/to-clj r)
          _ (check "matmul 2x2 shape" (= (t/shape r) [2 2]) (str "got " (t/shape r)))
          _ (check "matmul 2x2 row0" (arr-approx= (first vr) [19 22] 0.01)
                   (str "expected [19 22], got " (first vr)))
          _ (check "matmul 2x2 row1" (arr-approx= (second vr) [43 50] 0.01)
                   (str "expected [43 50], got " (second vr)))

          ;; Non-square [2,3] × [3,2]
          c (t/tensor [[1 2 3] [4 5 6]])
          d (t/tensor [[7 8] [9 10] [11 12]])
          r2 (t/matmul c d)
          vr2 (t/to-clj r2)
          _ (check "matmul non-square shape" (= (t/shape r2) [2 2]) (str "got " (t/shape r2)))
          _ (check "matmul non-square row0" (arr-approx= (first vr2) [58 64] 0.01)
                   (str "expected [58 64], got " (first vr2)))
          _ (check "matmul non-square row1" (arr-approx= (second vr2) [139 154] 0.01)
                   (str "expected [139 154], got " (second vr2)))

          ;; Identity multiply
          eye (t/tensor [[1 0] [0 1]])
          r3 (t/matmul a eye)
          vr3 (t/to-clj r3)
          _ (check "matmul identity row0" (arr-approx= (first vr3) [1 2] 0.01)
                   (str "expected [1 2], got " (first vr3)))
          _ (check "matmul identity row1" (arr-approx= (second vr3) [3 4] 0.01)
                   (str "expected [3 4], got " (second vr3)))

          ;; 1D dot product → scalar
          v1 (t/tensor [1 2 3])
          v2 (t/tensor [4 5 6])
          r4 (t/matmul v1 v2)
          vr4 (t/to-number r4)
          _ (check "matmul dot product" (approx= vr4 32 0.01)
                   (str "expected 32, got " vr4))
          _ (check "matmul dot shape" (= (t/shape r4) []) (str "got " (t/shape r4)))

          ;; Matrix-vector [2,3] × [3] → [2]
          r5 (t/matmul c v1)
          vr5 (t/to-clj r5)
          _ (check "matmul mat-vec" (arr-approx= vr5 [14 32] 0.01)
                   (str "expected [14 32], got " vr5))
          _ (check "matmul mat-vec shape" (= (t/shape r5) [2]) (str "got " (t/shape r5)))

          ;; Vector-matrix [3] × [3,2] → [2]
          r6 (t/matmul v1 d)
          vr6 (t/to-clj r6)
          _ (check "matmul vec-mat" (arr-approx= vr6 [58 64] 0.01)
                   (str "expected [58 64], got " vr6))

          ;; Non-tile-aligned [7,5] × [5,3]
          big-a (t/tensor [[1 0 0 0 0] [0 1 0 0 0] [0 0 1 0 0] [0 0 0 1 0]
                           [0 0 0 0 1] [1 1 0 0 0] [0 0 1 1 1]])
          big-b (t/tensor [[1 2 3] [4 5 6] [7 8 9] [10 11 12] [13 14 15]])
          r7 (t/matmul big-a big-b)
          vr7 (t/to-clj r7)
          _ (check "matmul non-aligned shape" (= (t/shape r7) [7 3]) (str "got " (t/shape r7)))
          _ (check "matmul non-aligned row0" (arr-approx= (first vr7) [1 2 3] 0.01)
                   (str "expected [1 2 3], got " (first vr7)))
          _ (check "matmul non-aligned row5" (arr-approx= (nth vr7 5) [5 7 9] 0.01)
                   (str "expected [5 7 9], got " (nth vr7 5)))
          _ (check "matmul non-aligned row6" (arr-approx= (nth vr7 6) [30 33 36] 0.01)
                   (str "expected [30 33 36], got " (nth vr7 6)))]

    (doseq [x [a b r c d r2 eye r3 v1 v2 r4 r5 r6 big-a big-b r7]]
      (t/dispose! x))))

;; ---------------------------------------------------------------------------
;; 11. Transpose
;; ---------------------------------------------------------------------------

(defn- test-transpose []
  (println "\n--- Transpose ---")
  (p/let [;; 2x3 → 3x2
          a (t/tensor [[1 2 3] [4 5 6]])
          r (t/transpose a)
          vr (t/to-clj r)
          _ (check "transpose shape" (= (t/shape r) [3 2]) (str "got " (t/shape r)))
          _ (check "transpose row0" (arr-approx= (first vr) [1 4] 0.01)
                   (str "expected [1 4], got " (first vr)))
          _ (check "transpose row1" (arr-approx= (second vr) [2 5] 0.01)
                   (str "expected [2 5], got " (second vr)))
          _ (check "transpose row2" (arr-approx= (nth vr 2) [3 6] 0.01)
                   (str "expected [3 6], got " (nth vr 2)))

          ;; 1x4 → 4x1
          b (t/tensor [[1 2 3 4]])
          rb (t/transpose b)
          vrb (t/to-clj rb)
          _ (check "transpose 1x4 shape" (= (t/shape rb) [4 1]) (str "got " (t/shape rb)))
          _ (check "transpose 1x4 vals" (arr-approx= (map first vrb) [1 2 3 4] 0.01)
                   (str "got " vrb))

          ;; 1D no-op
          c (t/tensor [1 2 3])
          rc (t/transpose c)
          vrc (t/to-clj rc)
          _ (check "transpose 1D no-op" (arr-approx= vrc [1 2 3] 0.01)
                   (str "got " vrc))
          _ (check "transpose 1D shape" (= (t/shape rc) [3]) (str "got " (t/shape rc)))

          ;; Scalar no-op
          s (t/scalar 42)
          rs (t/transpose s)
          vrs (t/to-number rs)
          _ (check "transpose scalar" (approx= vrs 42 0.01) (str "got " vrs))]

    (doseq [x [a r b rb c rc s rs]]
      (t/dispose! x))))

;; ---------------------------------------------------------------------------
;; 12. Slice
;; ---------------------------------------------------------------------------

(defn- test-slice []
  (println "\n--- Slice ---")
  (p/let [;; 1D sub-range
          a (t/tensor [10 20 30 40 50])
          r (t/slice a 0 1 4)
          vr (t/to-clj r)
          _ (check "slice 1D" (arr-approx= vr [20 30 40] 0.01)
                   (str "expected [20 30 40], got " vr))
          _ (check "slice 1D shape" (= (t/shape r) [3]) (str "got " (t/shape r)))

          ;; 2D row slice (dim-0 fast path)
          b (t/tensor [[1 2 3] [4 5 6] [7 8 9]])
          rb (t/slice b 0 1 3)
          vrb (t/to-clj rb)
          _ (check "slice 2D rows shape" (= (t/shape rb) [2 3]) (str "got " (t/shape rb)))
          _ (check "slice 2D rows row0" (arr-approx= (first vrb) [4 5 6] 0.01)
                   (str "expected [4 5 6], got " (first vrb)))
          _ (check "slice 2D rows row1" (arr-approx= (second vrb) [7 8 9] 0.01)
                   (str "expected [7 8 9], got " (second vrb)))

          ;; 2D column slice (dim-1, shader path)
          rc (t/slice b 1 0 2)
          vrc (t/to-clj rc)
          _ (check "slice 2D cols shape" (= (t/shape rc) [3 2]) (str "got " (t/shape rc)))
          _ (check "slice 2D cols row0" (arr-approx= (first vrc) [1 2] 0.01)
                   (str "expected [1 2], got " (first vrc)))
          _ (check "slice 2D cols row1" (arr-approx= (second vrc) [4 5] 0.01)
                   (str "expected [4 5], got " (second vrc)))
          _ (check "slice 2D cols row2" (arr-approx= (nth vrc 2) [7 8] 0.01)
                   (str "expected [7 8], got " (nth vrc 2)))

          ;; Invalid bounds error
          _ (check "slice invalid bounds"
                   (try (t/slice a 0 3 1) false
                        (catch :default e true))
                   "expected error for start > end")]

    (doseq [x [a r b rb rc]]
      (t/dispose! x))))

;; ---------------------------------------------------------------------------
;; 13. Concat + Stack
;; ---------------------------------------------------------------------------

(defn- test-concat-stack []
  (println "\n--- Concat + Stack ---")
  (p/let [;; 1D concat
          a (t/tensor [1 2 3])
          b (t/tensor [4 5 6])
          r (t/concat-tensors 0 [a b])
          vr (t/to-clj r)
          _ (check "concat 1D" (arr-approx= vr [1 2 3 4 5 6] 0.01)
                   (str "expected [1 2 3 4 5 6], got " vr))
          _ (check "concat 1D shape" (= (t/shape r) [6]) (str "got " (t/shape r)))

          ;; 2D row concat (dim-0 fast path)
          c (t/tensor [[1 2] [3 4]])
          d (t/tensor [[5 6] [7 8]])
          r2 (t/concat-tensors 0 [c d])
          vr2 (t/to-clj r2)
          _ (check "concat 2D rows shape" (= (t/shape r2) [4 2]) (str "got " (t/shape r2)))
          _ (check "concat 2D rows row0" (arr-approx= (first vr2) [1 2] 0.01)
                   (str "expected [1 2], got " (first vr2)))
          _ (check "concat 2D rows row2" (arr-approx= (nth vr2 2) [5 6] 0.01)
                   (str "expected [5 6], got " (nth vr2 2)))

          ;; 2D column concat (dim-1, shader path)
          r3 (t/concat-tensors 1 [c d])
          vr3 (t/to-clj r3)
          _ (check "concat 2D cols shape" (= (t/shape r3) [2 4]) (str "got " (t/shape r3)))
          _ (check "concat 2D cols row0" (arr-approx= (first vr3) [1 2 5 6] 0.01)
                   (str "expected [1 2 5 6], got " (first vr3)))
          _ (check "concat 2D cols row1" (arr-approx= (second vr3) [3 4 7 8] 0.01)
                   (str "expected [3 4 7 8], got " (second vr3)))

          ;; 3-tensor concat
          e (t/tensor [7 8 9])
          r4 (t/concat-tensors 0 [a b e])
          vr4 (t/to-clj r4)
          _ (check "concat 3-tensor" (arr-approx= vr4 [1 2 3 4 5 6 7 8 9] 0.01)
                   (str "expected [1..9], got " vr4))

          ;; Stack 1D → 2D
          r5 (t/stack 0 [a b])
          vr5 (t/to-clj r5)
          _ (check "stack shape" (= (t/shape r5) [2 3]) (str "got " (t/shape r5)))
          _ (check "stack row0" (arr-approx= (first vr5) [1 2 3] 0.01)
                   (str "expected [1 2 3], got " (first vr5)))
          _ (check "stack row1" (arr-approx= (second vr5) [4 5 6] 0.01)
                   (str "expected [4 5 6], got " (second vr5)))]

    (doseq [x [a b r c d r2 r3 e r4 r5]]
      (t/dispose! x))))

;; ---------------------------------------------------------------------------
;; 14. Arange
;; ---------------------------------------------------------------------------

(defn- test-arange []
  (println "\n--- Arange ---")
  (p/let [r (t/arange 5)
          vr (t/to-clj r)
          _ (check "arange values" (arr-approx= vr [0 1 2 3 4] 0.01)
                   (str "expected [0 1 2 3 4], got " vr))
          _ (check "arange shape" (= (t/shape r) [5]) (str "got " (t/shape r)))]
    (t/dispose! r)))

;; ---------------------------------------------------------------------------
;; Main
;; ---------------------------------------------------------------------------

(println "\n=== GPU Tensor Runtime Tests ===")

(p/let [_ (dev/init!)
        _ (test-creation)
        _ (test-binary-arithmetic)
        _ (test-unary-ops)
        _ (test-comparisons)
        _ (test-reductions)
        _ (test-shape-ops)
        _ (test-rng)
        _ (test-larger-arrays)
        _ (test-dispose)
        _ (test-matmul)
        _ (test-transpose)
        _ (test-slice)
        _ (test-concat-stack)
        _ (test-arange)]
  (println (str "\n" @passed " passed, " @failed " failed"))
  (when (pos? @failed)
    (js/process.exit 1)))
