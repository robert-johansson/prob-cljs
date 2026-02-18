(ns prob.mlx.dist
  "MLX-backed probability distributions for GPU-accelerated inference.
   Each distribution implements prob.dist/IDistribution for backward compatibility
   with existing prob-cljs inference (mh-query, enumeration-query, etc.).

   Additionally implements IDifferentiable for gradient-based inference (HMC, VI):
   - (log-prob d params value) returns differentiable log-probability as MLX array
   - (sample-reparam d params) returns reparameterized sample (for VI gradients)

   All log-prob computations stay as MLX arrays for autograd.
   Use mx/item to extract scalar values at boundaries."
  (:require [prob.mlx.core :as mx]
            [prob.dist :as dist]))

;; ---------------------------------------------------------------------------
;; Protocol for differentiable distributions
;; ---------------------------------------------------------------------------

(defprotocol IDifferentiable
  (log-prob [this value]
    "Differentiable log-probability as an MLX array. Stays on GPU.")
  (sample-reparam [this]
    "Reparameterized sample (for gradient flow through sampling)."))

;; ---------------------------------------------------------------------------
;; Constants
;; ---------------------------------------------------------------------------

(def ^:private log-2pi (js/Math.log (* 2.0 js/Math.PI)))

;; ---------------------------------------------------------------------------
;; MLX Gaussian
;; ---------------------------------------------------------------------------

(defrecord MLXGaussian [mu sigma]
  dist/IDistribution
  (sample* [_]
    (let [z (mx/random-normal [])
          sample (mx/add mu (mx/multiply sigma z))]
      (mx/eval! sample)
      (mx/item sample)))
  (observe* [_ value]
    (let [z (mx/divide (mx/subtract (mx/scalar value) mu) sigma)
          lp (mx/multiply
               (mx/scalar -0.5)
               (mx/add (mx/scalar log-2pi)
                       (mx/add (mx/multiply (mx/scalar 2.0) (mx/log sigma))
                               (mx/multiply z z))))]
      (mx/eval! lp)
      (mx/item lp)))

  IDifferentiable
  (log-prob [_ value]
    (let [v (if (mx/array? value) value (mx/scalar value))
          z (mx/divide (mx/subtract v mu) sigma)]
      (mx/multiply
        (mx/scalar -0.5)
        (mx/add (mx/scalar log-2pi)
                (mx/add (mx/multiply (mx/scalar 2.0) (mx/log sigma))
                        (mx/multiply z z))))))
  (sample-reparam [_]
    (let [z (mx/random-normal [])]
      (mx/add mu (mx/multiply sigma z)))))

(defn mlx-gaussian
  "MLX-backed Gaussian distribution.
   mu and sigma are MLX scalars or arrays."
  ([mu sigma]
   (->MLXGaussian (if (mx/array? mu) mu (mx/scalar mu))
                  (if (mx/array? sigma) sigma (mx/scalar sigma)))))

;; ---------------------------------------------------------------------------
;; MLX Beta
;; ---------------------------------------------------------------------------

(defrecord MLXBeta [alpha beta-param]
  dist/IDistribution
  (sample* [_]
    ;; Beta via gamma sampling not easily differentiable
    ;; Fall back to CLJS beta for now
    (let [a (mx/item alpha)
          b (mx/item beta-param)]
      (prob.dist/sample* (prob.dist/beta-dist a b))))
  (observe* [_ value]
    (let [lp (log-prob (->MLXBeta alpha beta-param) value)]
      (mx/eval! lp)
      (mx/item lp)))

  IDifferentiable
  (log-prob [_ value]
    (let [v (if (mx/array? value) value (mx/scalar value))
          a alpha
          b beta-param
          ;; log Beta(a,b) = logGamma(a) + logGamma(b) - logGamma(a+b)
          ;; Approximate with lgamma as JS/MLX scalar since MLX doesn't have lgamma
          ;; Use the log-prob formula directly:
          ;; (a-1)*log(x) + (b-1)*log(1-x) - logBeta(a,b)
          log-beta-val (mx/scalar (prob.math/log-beta-fn (mx/item a) (mx/item b)))]
      (mx/subtract
        (mx/add (mx/multiply (mx/subtract a (mx/scalar 1)) (mx/log v))
                (mx/multiply (mx/subtract b (mx/scalar 1))
                             (mx/log (mx/subtract (mx/scalar 1) v))))
        log-beta-val)))
  (sample-reparam [_]
    ;; No simple reparameterization for Beta; use implicit reparam
    ;; or reject. For now, just sample (non-differentiable).
    (let [a (mx/item alpha)
          b (mx/item beta-param)
          v (prob.dist/sample* (prob.dist/beta-dist a b))]
      (mx/scalar v))))

;; ---------------------------------------------------------------------------
;; MLX Gamma
;; ---------------------------------------------------------------------------

(defrecord MLXGamma [shape-param scale]
  dist/IDistribution
  (sample* [_]
    (let [s (mx/item shape-param)
          sc (mx/item scale)]
      (prob.dist/sample* (prob.dist/gamma-dist s sc))))
  (observe* [_ value]
    (let [lp (log-prob (->MLXGamma shape-param scale) value)]
      (mx/eval! lp)
      (mx/item lp)))

  IDifferentiable
  (log-prob [_ value]
    (let [v (if (mx/array? value) value (mx/scalar value))
          k shape-param
          theta scale
          log-gamma-k (mx/scalar (prob.math/log-gamma-fn (mx/item k)))]
      ;; (k-1)*log(x) - x/theta - k*log(theta) - lgamma(k)
      (mx/subtract
        (mx/subtract
          (mx/multiply (mx/subtract k (mx/scalar 1)) (mx/log v))
          (mx/divide v theta))
        (mx/add (mx/multiply k (mx/log theta)) log-gamma-k))))
  (sample-reparam [_]
    (let [s (mx/item shape-param)
          sc (mx/item scale)
          v (prob.dist/sample* (prob.dist/gamma-dist s sc))]
      (mx/scalar v))))

;; ---------------------------------------------------------------------------
;; MLX Multivariate Normal (via Cholesky)
;; ---------------------------------------------------------------------------

(defrecord MLXMultivariateNormal [mean-vec cov-matrix cholesky-L]
  dist/IDistribution
  (sample* [this]
    (let [s (sample-reparam this)]
      (mx/eval! s)
      (mx/->clj s)))
  (observe* [_ value]
    (let [lp (log-prob (->MLXMultivariateNormal mean-vec cov-matrix cholesky-L) value)]
      (mx/eval! lp)
      (mx/item lp)))

  IDifferentiable
  (log-prob [_ value]
    (let [v (if (mx/array? value) value (mx/array value))
          k (first (mx/shape mean-vec))
          diff (mx/subtract v mean-vec)
          ;; Solve L @ y = diff => y = L^{-1} @ diff
          ;; Then mahalanobis = y^T @ y
          y (mx/solve-triangular cholesky-L (mx/reshape diff [k 1]) false)
          y-flat (mx/flatten y)
          mahal (mx/sum (mx/multiply y-flat y-flat))
          ;; log det(Sigma) = 2 * sum(log(diag(L)))
          log-det-sigma (mx/multiply (mx/scalar 2.0)
                                     (mx/sum (mx/log (mx/diag cholesky-L))))]
      (mx/multiply
        (mx/scalar -0.5)
        (mx/add (mx/scalar (* k log-2pi))
                (mx/add log-det-sigma mahal)))))
  (sample-reparam [_]
    (let [k (first (mx/shape mean-vec))
          z (mx/random-normal [k])]
      ;; sample = mean + L @ z
      (mx/add mean-vec
              (mx/flatten (mx/matmul cholesky-L (mx/reshape z [k 1])))))))

(defn mlx-multivariate-normal
  "MLX-backed multivariate normal distribution.
   mean-vec: [k] array, cov-matrix: [k k] positive definite array.
   Cholesky decomposition is computed once at construction."
  [mean-vec cov-matrix]
  (let [mu (if (mx/array? mean-vec) mean-vec (mx/array mean-vec))
        cov (if (mx/array? cov-matrix) cov-matrix (mx/array cov-matrix))
        cov-2d (if (= 1 (mx/ndim cov))
                 (let [k (first (mx/shape mu))]
                   (mx/reshape cov [k k]))
                 cov)
        L (mx/cholesky cov-2d)]
    (mx/eval! L)
    (->MLXMultivariateNormal mu cov-2d L)))

;; ---------------------------------------------------------------------------
;; MLX Uniform
;; ---------------------------------------------------------------------------

(defrecord MLXUniform [lo hi]
  dist/IDistribution
  (sample* [_]
    (let [s (mx/add lo (mx/multiply (mx/subtract hi lo) (mx/random-uniform [])))]
      (mx/eval! s)
      (mx/item s)))
  (observe* [_ value]
    (let [v value]
      (if (or (< v (mx/item lo)) (> v (mx/item hi)))
        ##-Inf
        (- (js/Math.log (- (mx/item hi) (mx/item lo)))))))

  IDifferentiable
  (log-prob [_ value]
    (let [v (if (mx/array? value) value (mx/scalar value))
          range-val (mx/subtract hi lo)]
      ;; -log(hi - lo) when in range
      ;; Use where to handle out-of-range (returns -inf)
      (let [in-range (mx/greater-equal v lo)
            in-range (mx/multiply in-range (mx/less-equal v hi))
            lp (mx/negative (mx/log range-val))]
        (mx/where in-range lp (mx/scalar ##-Inf)))))
  (sample-reparam [_]
    (let [u (mx/random-uniform [])]
      (mx/add lo (mx/multiply (mx/subtract hi lo) u)))))

(defn mlx-uniform
  "MLX-backed continuous uniform distribution."
  [lo hi]
  (->MLXUniform (if (mx/array? lo) lo (mx/scalar lo))
                (if (mx/array? hi) hi (mx/scalar hi))))

;; ---------------------------------------------------------------------------
;; MLX Exponential
;; ---------------------------------------------------------------------------

(defrecord MLXExponential [rate]
  dist/IDistribution
  (sample* [_]
    ;; Inverse CDF: -log(1-u)/rate
    (let [u (mx/random-uniform [])
          s (mx/divide (mx/negative (mx/log (mx/subtract (mx/scalar 1) u))) rate)]
      (mx/eval! s)
      (mx/item s)))
  (observe* [_ value]
    (if (< value 0)
      ##-Inf
      (- (js/Math.log (mx/item rate)) (* (mx/item rate) value))))

  IDifferentiable
  (log-prob [_ value]
    (let [v (if (mx/array? value) value (mx/scalar value))]
      ;; log(rate) - rate * x
      (mx/subtract (mx/log rate) (mx/multiply rate v))))
  (sample-reparam [_]
    (let [u (mx/random-uniform [])]
      (mx/divide (mx/negative (mx/log (mx/subtract (mx/scalar 1) u))) rate))))

(defn mlx-exponential
  "MLX-backed exponential distribution."
  [rate]
  (->MLXExponential (if (mx/array? rate) rate (mx/scalar rate))))

;; ---------------------------------------------------------------------------
;; Convenience: log-prob as a function (for use with mx/grad)
;; ---------------------------------------------------------------------------

(defn log-prob-fn
  "Returns a function params -> log-prob suitable for use with mx/grad.
   model-fn takes params (MLX array) and returns a scalar log-probability."
  [model-fn]
  model-fn)

;; ---------------------------------------------------------------------------
;; Gaussian log-prob as differentiable function
;; ---------------------------------------------------------------------------

(defn gaussian-log-prob
  "Differentiable Gaussian log-probability.
   All args are MLX arrays. Returns MLX scalar."
  [mu sigma value]
  (let [z (mx/divide (mx/subtract value mu) sigma)]
    (mx/multiply
      (mx/scalar -0.5)
      (mx/add (mx/scalar log-2pi)
              (mx/add (mx/multiply (mx/scalar 2.0) (mx/log sigma))
                      (mx/multiply z z))))))

(defn sum-gaussian-log-probs
  "Sum of independent Gaussian log-probabilities.
   mu, sigma, values are all MLX arrays of the same shape."
  [mu sigma values]
  (let [z (mx/divide (mx/subtract values mu) sigma)
        element-lp (mx/multiply
                     (mx/scalar -0.5)
                     (mx/add (mx/scalar log-2pi)
                             (mx/add (mx/multiply (mx/scalar 2.0) (mx/log sigma))
                                     (mx/multiply z z))))]
    (mx/sum element-lp)))
