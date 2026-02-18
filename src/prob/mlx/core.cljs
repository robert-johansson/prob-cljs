(ns prob.mlx.core
  "Thin ClojureScript wrapper over node-mlx (@frost-beta/mlx).
   Provides idiomatic ClojureScript access to MLX tensor operations,
   autograd, random number generation, and linear algebra.

   All operations are lazy by default -- call (eval!) to materialize.
   Use (with-mlx ...) for automatic memory cleanup via mx.tidy.

   Requires: npm install @frost-beta/mlx (Apple Silicon only, nbb only).")

;; ---------------------------------------------------------------------------
;; Module loading
;; ---------------------------------------------------------------------------

(defonce ^:private mlx-module (js/require "@frost-beta/mlx"))
(defonce ^:private mx    (.-core mlx-module))
(defonce ^:private mrng  (.-random mx))
(defonce ^:private mla   (.-linalg mx))
(defonce ^:private mfft  (.-fft mx))

;; CPU stream needed for linalg ops (cholesky, solve, SVD, eigh)
(defonce ^:private cpu-stream (.newStream mx (.-cpu mx)))

;; ---------------------------------------------------------------------------
;; Dtypes
;; ---------------------------------------------------------------------------

(def float16  (.-float16 mx))
(def float32  (.-float32 mx))
(def float64  (.-float64 mx))
(def bfloat16 (.-bfloat16 mx))
(def int8     (.-int8 mx))
(def int16    (.-int16 mx))
(def int32    (.-int32 mx))
(def int64    (.-int64 mx))
(def uint8    (.-uint8 mx))
(def uint16   (.-uint16 mx))
(def uint32   (.-uint32 mx))
(def uint64   (.-uint64 mx))
(def bool-dt  (.-bool_ mx))

(def default-dtype float32)

;; ---------------------------------------------------------------------------
;; Array creation
;; ---------------------------------------------------------------------------

(defn array
  "Create an MLX array from JS data.
   (array [1 2 3])           ;; float32
   (array [1 2 3] float64)   ;; explicit dtype
   (array 3.14)              ;; scalar"
  ([data]
   (array data default-dtype))
  ([data dtype]
   (if (number? data)
     (.array mx data dtype)
     (.array mx (clj->js data) dtype))))

(defn scalar
  "Create a scalar MLX array."
  ([v] (.array mx v default-dtype))
  ([v dtype] (.array mx v dtype)))

(defn zeros
  "Array of zeros. (zeros [3]) or (zeros [2 3] float64)"
  ([shape] (.zeros mx (clj->js shape)))
  ([shape dtype] (.zeros mx (clj->js shape) dtype)))

(defn ones
  "Array of ones."
  ([shape] (.ones mx (clj->js shape)))
  ([shape dtype] (.ones mx (clj->js shape) dtype)))

(defn full
  "Array filled with a value."
  ([shape val] (.full mx (clj->js shape) val))
  ([shape val dtype] (.full mx (clj->js shape) val dtype)))

(defn eye
  "Identity matrix."
  ([n] (.eye mx n))
  ([n dtype] (.eye mx n dtype)))

(defn arange
  "Range of values."
  ([stop] (.arange mx stop))
  ([start stop] (.arange mx start stop))
  ([start stop step] (.arange mx start stop step)))

(defn linspace
  "Evenly spaced values."
  ([start stop num] (.linspace mx start stop num)))

;; ---------------------------------------------------------------------------
;; Evaluation / materialization
;; ---------------------------------------------------------------------------

(defn eval!
  "Force evaluation of lazy MLX arrays. Accepts one or more arrays."
  [& arrays]
  (apply (.-eval mx) arrays))

(defn item
  "Extract scalar value from a 0-d or 1-element array as a JS number."
  [arr]
  (.item arr))

(defn ->clj
  "Convert an MLX array to a ClojureScript data structure.
   Scalars become numbers, 1-d become vectors, 2-d become vector of vectors."
  [arr]
  (.eval mx arr)
  (let [shape (js->clj (.-shape arr))
        ndim (count shape)]
    (cond
      (zero? ndim) (.item arr)
      (== ndim 1) (vec (for [i (range (first shape))]
                         (.item (.take mx arr (.array mx i int32)))))
      :else (let [flat (->clj (.flatten mx arr))
                  cols (last shape)]
              (vec (map vec (partition cols flat)))))))

(defn shape
  "Return the shape of an MLX array as a ClojureScript vector."
  [arr]
  (vec (js->clj (.-shape arr))))

(defn ndim
  "Number of dimensions."
  [arr]
  (count (.-shape arr)))

(defn dtype
  "Return the dtype of an array."
  [arr]
  (.-dtype arr))

(defn size
  "Total number of elements."
  [arr]
  (.-size arr))

;; ---------------------------------------------------------------------------
;; Memory management
;; ---------------------------------------------------------------------------

(defn tidy
  "Execute f inside mx.tidy for automatic memory cleanup.
   Returns the result of f (any arrays returned are preserved)."
  [f]
  (.tidy mx f))

(defmacro with-mlx
  "Execute body inside mx.tidy for automatic memory cleanup."
  [& body]
  `(tidy (fn [] ~@body)))

(defn dispose!
  "Manually dispose of an MLX array."
  [arr]
  (.dispose mx arr))

;; ---------------------------------------------------------------------------
;; Arithmetic (element-wise)
;; ---------------------------------------------------------------------------

(defn add      [a b] (.add mx a b))
(defn subtract [a b] (.subtract mx a b))
(defn multiply [a b] (.multiply mx a b))
(defn divide   [a b] (.divide mx a b))
(defn power    [a b] (.power mx a b))
(defn negative [a]   (.negative mx a))
(defn abs      [a]   (.abs mx a))
(defn sign     [a]   (.sign mx a))
(defn square   [a]   (.square mx a))
(defn sqrt     [a]   (.sqrt mx a))
(defn rsqrt    [a]   (.rsqrt mx a))
(defn reciprocal [a] (.reciprocal mx a))
(defn remainder [a b] (.remainder mx a b))
(defn floor-divide [a b] (.floorDivide mx a b))
(defn maximum  [a b] (.maximum mx a b))
(defn minimum  [a b] (.minimum mx a b))
(defn clip     [a lo hi] (.clip mx a lo hi))

;; ---------------------------------------------------------------------------
;; Math functions
;; ---------------------------------------------------------------------------

(defn exp      [a] (.exp mx a))
(defn expm1    [a] (.expm1 mx a))
(defn log      [a] (.log mx a))
(defn log2     [a] (.log2 mx a))
(defn log10    [a] (.log10 mx a))
(defn log1p    [a] (.log1p mx a))
(defn logaddexp [a b] (.logaddexp mx a b))

(defn sin      [a] (.sin mx a))
(defn cos      [a] (.cos mx a))
(defn tan      [a] (.tan mx a))
(defn arcsin   [a] (.arcsin mx a))
(defn arccos   [a] (.arccos mx a))
(defn arctan   [a] (.arctan mx a))
(defn arctan2  [a b] (.arctan2 mx a b))
(defn sinh     [a] (.sinh mx a))
(defn cosh     [a] (.cosh mx a))
(defn tanh     [a] (.tanh mx a))

(defn sigmoid  [a] (.sigmoid mx a))
(defn erf      [a] (.erf mx a))
(defn erfinv   [a] (.erfinv mx a))

(defn floor    [a] (.floor mx a))
(defn ceil     [a] (.ceil mx a))
(defn round    [a] (.round mx a))

;; ---------------------------------------------------------------------------
;; Reductions
;; ---------------------------------------------------------------------------

(defn sum
  "Sum of array elements."
  ([a] (.sum mx a))
  ([a axes] (.sum mx a (clj->js axes)))
  ([a axes keepdims] (.sum mx a (clj->js axes) keepdims)))

(defn prod
  "Product of array elements."
  ([a] (.prod mx a))
  ([a axes] (.prod mx a (clj->js axes))))

(defn mean
  "Mean of array elements."
  ([a] (.mean mx a))
  ([a axes] (.mean mx a (clj->js axes))))

(defn variance
  "Variance of array elements."
  ([a] (.variance mx a))
  ([a axes] (.variance mx a (clj->js axes))))

(defn std
  "Standard deviation."
  ([a] (.std mx a))
  ([a axes] (.std mx a (clj->js axes))))

(defn amax
  "Maximum value."
  ([a] (.max mx a))
  ([a axes] (.max mx a (clj->js axes))))

(defn amin
  "Minimum value."
  ([a] (.min mx a))
  ([a axes] (.min mx a (clj->js axes))))

(defn argmax
  "Index of maximum value."
  ([a] (.argmax mx a))
  ([a axis] (.argmax mx a axis)))

(defn argmin
  "Index of minimum value."
  ([a] (.argmin mx a))
  ([a axis] (.argmin mx a axis)))

(defn logsumexp
  "Log-sum-exp reduction (numerically stable)."
  ([a] (.logsumexp mx a))
  ([a axes] (.logsumexp mx a (clj->js axes))))

(defn cumsum
  "Cumulative sum."
  ([a] (.cumsum mx a))
  ([a axis] (.cumsum mx a axis)))

(defn all
  "All elements are true."
  ([a] (.all mx a))
  ([a axes] (.all mx a (clj->js axes))))

(defn any
  "Any element is true."
  ([a] (.any mx a))
  ([a axes] (.any mx a (clj->js axes))))

;; ---------------------------------------------------------------------------
;; Comparison
;; ---------------------------------------------------------------------------

(defn equal        [a b] (.equal mx a b))
(defn not-equal    [a b] (.notEqual mx a b))
(defn greater      [a b] (.greater mx a b))
(defn greater-equal [a b] (.greaterEqual mx a b))
(defn less         [a b] (.less mx a b))
(defn less-equal   [a b] (.lessEqual mx a b))
(defn where        [cond a b] (.where mx cond a b))
(defn isnan        [a] (.isnan mx a))
(defn isinf        [a] (.isinf mx a))
(defn isfinite     [a] (.isfinite mx a))
(defn allclose     [a b & {:keys [rtol atol] :or {rtol 1e-5 atol 1e-8}}]
  (.allclose mx a b rtol atol))

;; ---------------------------------------------------------------------------
;; Shape manipulation
;; ---------------------------------------------------------------------------

(defn reshape    [a shape] (.reshape mx a (clj->js shape)))
(defn flatten    [a] (.flatten mx a))
(defn squeeze    [a] (.squeeze mx a))
(defn expand-dims [a axis] (.expandDims mx a axis))
(defn transpose
  "Transpose. (transpose a) reverses axes, (transpose a [1 0]) specifies order."
  ([a] (.transpose mx a))
  ([a axes] (.transpose mx a (clj->js axes))))
(defn stack      [arrays & {:keys [axis] :or {axis 0}}]
  (.stack mx (clj->js arrays) axis))
(defn concat
  "Concatenate arrays."
  ([arrays] (.concatenate mx (clj->js arrays)))
  ([arrays axis] (.concatenate mx (clj->js arrays) axis)))
(defn split      [a indices-or-sections & {:keys [axis] :or {axis 0}}]
  (js->clj (.split mx a indices-or-sections axis)))
(defn tile       [a reps] (.tile mx a (clj->js reps)))
(defn repeat-arr [a repeats axis] (.repeat mx a repeats axis))
(defn broadcast-to [a shape] (.broadcastTo mx a (clj->js shape)))

;; ---------------------------------------------------------------------------
;; Indexing
;; ---------------------------------------------------------------------------

(defn take-idx
  "Take elements at indices along axis 0."
  ([a indices] (.take mx a indices))
  ([a indices axis] (.take mx a indices axis)))

(defn take-along-axis [a indices axis]
  (.takeAlongAxis mx a indices axis))

(defn index
  "Index into array with integer index at axis 0."
  [a i]
  (.take mx a (scalar i int32)))

(defn slice
  "Slice an array."
  [a & start-stop-pairs]
  ;; Use the low-level slice API
  (let [starts (clj->js (take-nth 2 start-stop-pairs))
        stops  (clj->js (take-nth 2 (rest start-stop-pairs)))]
    (.slice mx a starts stops)))

;; ---------------------------------------------------------------------------
;; Matrix operations
;; ---------------------------------------------------------------------------

(defn matmul    [a b] (.matmul mx a b))
(defn inner     [a b] (.inner mx a b))
(defn outer     [a b] (.outer mx a b))
(defn dot       [a b] (.inner mx a b))
(defn diag      [a] (.diag mx a))
(defn trace-mat [a] (.trace mx a))
(defn einsum    [subscripts & arrays]
  (apply (.-einsum mx) subscripts (seq arrays)))

;; ---------------------------------------------------------------------------
;; Linear algebra (runs on CPU stream)
;; ---------------------------------------------------------------------------

(defn cholesky
  "Cholesky decomposition. Returns lower-triangular L such that A = L @ L^T."
  [a]
  (.cholesky mla a false cpu-stream))

(defn solve
  "Solve linear system A @ x = b."
  [a b]
  (.solve mla a b cpu-stream))

(defn solve-triangular
  "Solve triangular linear system."
  [a b upper]
  (.solveTriangular mla a b upper cpu-stream))

(defn inv
  "Matrix inverse."
  [a]
  (.inv mla a cpu-stream))

(defn tri-inv
  "Triangular matrix inverse."
  [a upper]
  (.triInv mla a upper cpu-stream))

(defn qr
  "QR decomposition. Returns [Q R]."
  [a]
  (let [result (.qr mla a cpu-stream)]
    [(aget result 0) (aget result 1)]))

(defn svd
  "SVD decomposition. Returns [U S Vt]."
  [a]
  (let [result (.svd mla a cpu-stream)]
    [(aget result 0) (aget result 1) (aget result 2)]))

(defn eigh
  "Eigendecomposition of symmetric matrix. Returns [eigenvalues eigenvectors]."
  [a]
  (let [result (.eigh mla a cpu-stream)]
    [(aget result 0) (aget result 1)]))

(defn eigvalsh
  "Eigenvalues of symmetric matrix."
  [a]
  (.eigvalsh mla a cpu-stream))

(defn norm
  "Matrix or vector norm."
  ([a] (.norm mla a))
  ([a ord] (.norm mla a ord)))

(defn lu-factor
  "LU factorization. Returns [LU pivots]."
  [a]
  (let [result (.luFactor mla a cpu-stream)]
    [(aget result 0) (aget result 1)]))

(defn det
  "Matrix determinant via eigenvalues of symmetric matrix."
  [a]
  (prod (eigvalsh a)))

(defn log-det
  "Log absolute determinant via eigenvalues."
  [a]
  (sum (log (abs (eigvalsh a)))))

;; ---------------------------------------------------------------------------
;; Random number generation
;; ---------------------------------------------------------------------------

(defn random-seed!
  "Set the random seed."
  [seed]
  (.seed mrng seed))

(defn random-key
  "Get a random key."
  [seed]
  (.key mrng seed))

(defn random-uniform
  "Uniform random in [low, high)."
  ([shape] (.uniform mrng (scalar 0) (scalar 1) (clj->js shape)))
  ([low high shape] (.uniform mrng (scalar low) (scalar high) (clj->js shape))))

(defn random-normal
  "Standard normal random."
  ([shape] (.normal mrng (clj->js shape)))
  ([shape dtype] (.normal mrng (clj->js shape) dtype)))

(defn random-bernoulli
  "Bernoulli random (boolean array)."
  ([p shape] (.bernoulli mrng (scalar p) (clj->js shape))))

(defn random-categorical
  "Categorical sampling from log-probabilities."
  ([logits] (.categorical mrng logits))
  ([logits num-samples] (.categorical mrng logits num-samples)))

(defn random-randint
  "Random integers in [low, high)."
  [low high shape]
  (.randint mrng low high (clj->js shape)))

(defn random-truncated-normal
  "Truncated normal random."
  [low high shape]
  (.truncatedNormal mrng (scalar low) (scalar high) (clj->js shape)))

(defn random-gumbel
  "Gumbel random."
  [shape]
  (.gumbel mrng (clj->js shape)))

(defn random-laplace
  "Laplace random."
  [shape]
  (.laplace mrng (clj->js shape)))

(defn random-permutation
  "Random permutation."
  [n]
  (.permutation mrng n))

;; ---------------------------------------------------------------------------
;; Autograd
;; ---------------------------------------------------------------------------

(defn grad
  "Create a gradient function. f must take MLX arrays and return a scalar.
   Returns a function that computes df/dx.
   (grad f)         ;; gradient w.r.t. first arg
   (grad f [0 1])   ;; gradient w.r.t. first and second args"
  ([f] (.grad mx f))
  ([f argnums] (.grad mx f (clj->js argnums))))

(defn value-and-grad
  "Returns a function that computes both [f(x), grad(f)(x)].
   (value-and-grad f)        ;; w.r.t. first arg
   (value-and-grad f [0 1])  ;; w.r.t. multiple args"
  ([f]
   (let [vg (.valueAndGrad mx f)]
     (fn [& args]
       (let [result (apply vg args)]
         [(aget result 0) (aget result 1)]))))
  ([f argnums]
   (let [vg (.valueAndGrad mx f (clj->js argnums))]
     (fn [& args]
       (let [result (apply vg args)]
         [(aget result 0) (aget result 1)])))))

(defn jvp
  "Jacobian-vector product (forward-mode AD).
   Returns [primals, tangents]."
  [f primals tangents]
  (let [result (.jvp mx f (clj->js primals) (clj->js tangents))]
    [(aget result 0) (aget result 1)]))

(defn vjp
  "Vector-Jacobian product (reverse-mode AD).
   Returns [primals, vjp-fn]."
  [f primals cotangents]
  (let [result (.vjp mx f (clj->js primals) (clj->js cotangents))]
    [(aget result 0) (aget result 1)]))

(defn stop-gradient
  "Stop gradient propagation through this array."
  [a]
  (.stopGradient mx a))

;; ---------------------------------------------------------------------------
;; Transforms
;; ---------------------------------------------------------------------------

(defn compile-fn
  "JIT compile a function for repeated execution.
   (compile-fn f)             ;; with shape tracking
   (compile-fn f :shapeless)  ;; shapeless mode"
  ([f] (.compile mx f))
  ([f mode]
   (if (= mode :shapeless)
     (.compile mx f true)
     (.compile mx f))))

(defn vmap
  "Vectorized map -- apply f across a batch dimension.
   (vmap f)                   ;; vectorize over axis 0
   (vmap f [0 0] [0])         ;; explicit in_axes, out_axes"
  ([f] (.vmap mx f))
  ([f in-axes] (.vmap mx f (clj->js in-axes)))
  ([f in-axes out-axes] (.vmap mx f (clj->js in-axes) (clj->js out-axes))))

;; ---------------------------------------------------------------------------
;; Async
;; ---------------------------------------------------------------------------

(defn async-eval!
  "Asynchronously evaluate arrays. Returns a Promise."
  [& arrays]
  (apply (.-asyncEval mx) arrays))

;; ---------------------------------------------------------------------------
;; Device / Stream
;; ---------------------------------------------------------------------------

(defn default-device [] (.defaultDevice mx))
(defn set-default-device! [d] (.setDefaultDevice mx d))
(def cpu (.-cpu mx))
(def gpu (.-gpu mx))

;; ---------------------------------------------------------------------------
;; Constants
;; ---------------------------------------------------------------------------

(def pi    (.-pi mx))
(def e-val (.-e mx))
(def inf   (.-inf mx))
(def nan   (.-nan mx))

;; ---------------------------------------------------------------------------
;; Softmax
;; ---------------------------------------------------------------------------

(defn softmax
  "Softmax along given axis."
  ([a] (.softmax mx a))
  ([a axis] (.softmax mx a axis)))

;; ---------------------------------------------------------------------------
;; Utilities
;; ---------------------------------------------------------------------------

(defn array?
  "Is x an MLX array?"
  [x]
  ;; Check if it has the shape property and item method typical of mx.array
  (and (some? x)
       (object? x)
       (some? (.-shape x))
       (fn? (.-item x))))
