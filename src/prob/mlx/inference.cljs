(ns prob.mlx.inference
  "Gradient-based inference algorithms using MLX autograd.
   - HMC (Hamiltonian Monte Carlo)
   - NUTS (No-U-Turn Sampler)

   These operate on differentiable log-density functions:
   log-density-fn: (fn [params] ...) -> MLX scalar

   params is a single MLX array (vector of parameters).
   Returns MLX scalar log-density (unnormalized log-posterior).

   GPU utilization strategy:
   - compile-fn JIT-compiles the gradient function (Metal program cached)
   - value-and-grad avoids redundant forward passes
   - One eval! per leapfrog step (bounds graph size, avoids OOM)
   - Constants pre-allocated outside loops
   - tidy wraps each HMC step for automatic memory cleanup

   Why eval per leapfrog step (not per trajectory):
   Autograd creates computation graphs that grow with each step. With L
   fully-lazy steps, the graph has O(L * model_size) nodes. For large L or
   complex models this exceeds Metal's buffer limit. Evaluating per step
   keeps the graph bounded while compile-fn ensures the Metal program is
   cached and replayed (no rebuild cost)."
  (:require [prob.mlx.core :as mx]))

(defonce ^:private mx-core (.-core (js/require "@frost-beta/mlx")))

;; ---------------------------------------------------------------------------
;; Leapfrog integrator
;; ---------------------------------------------------------------------------

(defn- leapfrog-step
  "Single leapfrog step. Evaluates the gradient once (bounded graph).
   With compile-fn on grad-U, the Metal program is cached after first call.

   grad-U: compiled gradient of negative log-density
   q: position (MLX array, evaluated)
   p: momentum (MLX array, evaluated)
   eps: step-size scalar (pre-allocated)
   half-eps: half step-size scalar (pre-allocated)
   Returns [q' p'] -- evaluated arrays."
  [grad-U q p eps half-eps]
  (.tidy mx-core
    (fn []
      (let [;; Half step for momentum
            g (grad-U q)
            p (mx/subtract p (mx/multiply half-eps g))
            ;; Full step for position
            q (mx/add q (mx/multiply eps p))
            ;; Half step for momentum
            g (grad-U q)
            p (mx/subtract p (mx/multiply half-eps g))]
        ;; Single eval materializes: 1 gradient computation + arithmetic
        ;; tidy frees all intermediates except returned [q, p]
        (mx/eval! q p)
        #js [q p]))))

(defn- leapfrog-trajectory
  "Run L leapfrog steps. Each step is a single eval! with compiled gradient.
   Total: L eval! calls, each with bounded graph size.
   With compile-fn, the Metal program is cached after the first step."
  [grad-U q p eps half-eps L]
  (loop [i 0, q q, p p]
    (if (>= i L)
      [q p]
      (let [result (leapfrog-step grad-U q p eps half-eps)]
        (recur (inc i) (aget result 0) (aget result 1))))))

;; ---------------------------------------------------------------------------
;; HMC kernel
;; ---------------------------------------------------------------------------

(defn- hmc-step
  "Single HMC step.
   Eval pattern: 1 (momentum) + L (leapfrog steps) + 1 (Hamiltonian) = L+2.
   With compiled grad-U, each eval replays a cached Metal program.

   value-and-grad-neg-ld: returns [neg-log-density, gradient] in one pass
   grad-neg-ld: compiled gradient of negative log-density
   current-q: current position (evaluated)
   eps/half-eps/half-scalar: pre-allocated constants
   n-leapfrog: number of leapfrog steps
   Returns [new-q accepted?]."
  [value-and-grad-neg-ld grad-neg-ld current-q eps half-eps half-scalar n-leapfrog]
  ;; Sample momentum -- eval to materialize random values
  (let [p0 (mx/random-normal (mx/shape current-q))
        _ (mx/eval! p0)

        ;; Current Hamiltonian via value-and-grad (single forward pass)
        [current-neg-U current-grad] (value-and-grad-neg-ld current-q)
        current-K (mx/multiply half-scalar (mx/sum (mx/multiply p0 p0)))
        _ (mx/eval! current-neg-U current-K)
        current-H (+ (mx/item current-neg-U) (mx/item current-K))

        ;; Leapfrog trajectory: L eval! calls with compiled Metal programs
        [proposed-q proposed-p] (leapfrog-trajectory
                                  grad-neg-ld current-q p0
                                  eps half-eps n-leapfrog)

        ;; Proposed Hamiltonian via value-and-grad
        [proposed-neg-U _proposed-grad] (value-and-grad-neg-ld proposed-q)
        proposed-K (mx/multiply half-scalar
                                (mx/sum (mx/multiply proposed-p proposed-p)))
        _ (mx/eval! proposed-neg-U proposed-K)
        proposed-H (+ (mx/item proposed-neg-U) (mx/item proposed-K))

        ;; Metropolis accept/reject
        log-accept (- current-H proposed-H)]

    ;; Clean up
    (mx/dispose! p0)
    (mx/dispose! current-neg-U)
    (mx/dispose! current-K)
    (mx/dispose! current-grad)
    (mx/dispose! proposed-K)
    (mx/dispose! proposed-neg-U)
    (when (some? proposed-p)
      (mx/dispose! proposed-p))

    (if (or (> log-accept 0)
            (< (js/Math.log (js/Math.random)) log-accept))
      [proposed-q true]
      (do
        (mx/dispose! proposed-q)
        [current-q false]))))

;; ---------------------------------------------------------------------------
;; HMC sampler
;; ---------------------------------------------------------------------------

(defn hmc
  "Hamiltonian Monte Carlo sampling.

   opts: {:samples N          ;; number of samples to collect
          :step-size eps      ;; leapfrog step size (default 0.01)
          :leapfrog-steps L   ;; leapfrog steps per proposal (default 20)
          :burn N              ;; burn-in samples to discard (default 0)
          :thin N              ;; keep every Nth sample (default 1)
          :compile? bool       ;; JIT-compile gradient fn (default true)
          :callback fn}        ;; (fn [{:iter i :value v :accepted? b}])

   log-density: (fn [params]) -> MLX scalar log-posterior
   init-params: MLX array, initial parameter values

   Returns vector of MLX arrays (samples).

   GPU utilization: L+2 eval! calls per sample with compiled Metal programs.
   Each eval replays a cached kernel -- no graph rebuilding after first step."
  [opts log-density init-params]
  (let [{:keys [samples step-size leapfrog-steps burn thin compile? callback]
         :or {step-size 0.01
              leapfrog-steps 20
              burn 0
              thin 1
              compile? true}} opts

        ;; Pre-allocate constants (never recreated in the loop)
        eps (mx/scalar step-size)
        half-eps (mx/scalar (* 0.5 step-size))
        half-scalar (mx/scalar 0.5)

        ;; Negative log-density (potential energy)
        neg-log-density (fn [q] (mx/negative (log-density q)))

        ;; Gradient function: optionally JIT-compiled
        ;; After first call, Metal program is cached and replayed
        grad-neg-ld (let [g (mx/grad neg-log-density)]
                      (if compile? (mx/compile-fn g) g))

        ;; value-and-grad: single forward+backward pass
        value-and-grad-neg-ld (mx/value-and-grad neg-log-density)

        total-iters (+ burn (* samples thin))]

    (loop [i 0
           current-q init-params
           collected (transient [])
           n-collected 0
           n-accepted 0]
      (if (>= n-collected samples)
        (with-meta
          (persistent! collected)
          {:acceptance-rate (/ n-accepted total-iters)})
        (let [[new-q accepted?]
              (hmc-step value-and-grad-neg-ld grad-neg-ld
                        current-q
                        eps half-eps half-scalar leapfrog-steps)
              in-collection? (>= i burn)
              keep? (and in-collection? (zero? (mod (- i burn) thin)))]
          (when (and callback keep?)
            (callback {:iter n-collected
                       :value (mx/->clj new-q)
                       :accepted? accepted?}))
          (recur (inc i)
                 new-q
                 (if keep? (conj! collected new-q) collected)
                 (if keep? (inc n-collected) n-collected)
                 (if accepted? (inc n-accepted) n-accepted)))))))

;; ---------------------------------------------------------------------------
;; NUTS (No-U-Turn Sampler)
;; ---------------------------------------------------------------------------

(defn- compute-tree-criterion
  "Check NUTS U-turn criterion: (q+ - q-) . p >= 0 for both endpoints."
  [q-minus q-plus p-minus p-plus]
  (let [diff (mx/subtract q-plus q-minus)
        check-fwd (mx/sum (mx/multiply diff p-plus))
        check-bwd (mx/sum (mx/multiply diff p-minus))]
    (mx/eval! check-fwd check-bwd)
    (let [result (and (>= (mx/item check-fwd) 0)
                      (>= (mx/item check-bwd) 0))]
      (mx/dispose! diff)
      (mx/dispose! check-fwd)
      (mx/dispose! check-bwd)
      result)))

(defn- nuts-base-step
  "NUTS base case: single leapfrog step with energy evaluation."
  [neg-log-density grad-neg-ld q p v eps half-eps half-scalar log-u current-H]
  (let [actual-eps (if (pos? v) eps (mx/negative eps))
        actual-half (if (pos? v) half-eps (mx/negative half-eps))
        result (leapfrog-step grad-neg-ld q p actual-eps actual-half)
        q' (aget result 0)
        p' (aget result 1)
        proposed-neg-U (neg-log-density q')
        proposed-K (mx/multiply half-scalar (mx/sum (mx/multiply p' p')))
        _ (mx/eval! proposed-neg-U proposed-K)
        proposed-H (+ (mx/item proposed-neg-U) (mx/item proposed-K))
        n' (if (<= log-u (- proposed-H)) 1 0)
        s' (< (- proposed-H current-H) 1000)
        alpha (min 1.0 (js/Math.exp (- current-H proposed-H)))]
    (mx/dispose! proposed-neg-U)
    (mx/dispose! proposed-K)
    {:q-minus q' :p-minus p' :q-plus q' :p-plus p'
     :q' q' :n' n' :s' s' :alpha alpha :n-alpha 1}))

(defn- build-tree
  "Recursively build NUTS tree."
  [neg-log-density grad-neg-ld q p log-u v eps half-eps half-scalar j current-H]
  (if (zero? j)
    (nuts-base-step neg-log-density grad-neg-ld q p v eps half-eps half-scalar log-u current-H)

    (let [tree1 (build-tree neg-log-density grad-neg-ld
                            q p log-u v eps half-eps half-scalar (dec j) current-H)]
      (if (not (:s' tree1))
        tree1
        (let [[q2 p2] (if (pos? v)
                        [(:q-plus tree1) (:p-plus tree1)]
                        [(:q-minus tree1) (:p-minus tree1)])
              tree2 (build-tree neg-log-density grad-neg-ld
                                q2 p2 log-u v eps half-eps half-scalar (dec j) current-H)
              total-n (+ (:n' tree1) (:n' tree2))
              q' (if (and (pos? total-n)
                          (< (js/Math.random) (/ (:n' tree2) total-n)))
                   (:q' tree2)
                   (:q' tree1))
              q-minus (if (pos? v) (:q-minus tree1) (:q-minus tree2))
              p-minus (if (pos? v) (:p-minus tree1) (:p-minus tree2))
              q-plus  (if (pos? v) (:q-plus tree2)  (:q-plus tree1))
              p-plus  (if (pos? v) (:p-plus tree2)  (:p-plus tree1))
              s' (and (:s' tree2)
                      (compute-tree-criterion q-minus q-plus p-minus p-plus))
              alpha (+ (:alpha tree1) (:alpha tree2))
              n-alpha (+ (:n-alpha tree1) (:n-alpha tree2))]
          {:q-minus q-minus :p-minus p-minus
           :q-plus q-plus :p-plus p-plus
           :q' q' :n' total-n :s' s'
           :alpha alpha :n-alpha n-alpha})))))

(defn- nuts-step
  "Single NUTS step."
  [neg-log-density grad-neg-ld current-q eps half-eps half-scalar max-depth]
  (let [p0 (mx/random-normal (mx/shape current-q))
        current-neg-U (neg-log-density current-q)
        current-K (mx/multiply half-scalar (mx/sum (mx/multiply p0 p0)))
        _ (mx/eval! p0 current-neg-U current-K)
        current-H (+ (mx/item current-neg-U) (mx/item current-K))
        log-u (+ (js/Math.log (js/Math.random)) (- current-H))]
    (mx/dispose! current-neg-U)
    (mx/dispose! current-K)
    (loop [j 0
           q-minus current-q, p-minus p0
           q-plus current-q, p-plus p0
           q' current-q
           depth-n 1
           continue? true]
      (if (or (not continue?) (>= j max-depth))
        q'
        (let [v (if (< (js/Math.random) 0.5) -1 1)
              [q-start p-start] (if (pos? v)
                                  [q-plus p-plus]
                                  [q-minus p-minus])
              tree (build-tree neg-log-density grad-neg-ld
                               q-start p-start
                               log-u v eps half-eps half-scalar j current-H)
              q'' (if (and (:s' tree)
                           (< (js/Math.random) (/ (:n' tree) (max 1 depth-n))))
                    (:q' tree)
                    q')
              q-minus' (if (neg? v) (:q-minus tree) q-minus)
              p-minus' (if (neg? v) (:p-minus tree) p-minus)
              q-plus'  (if (pos? v) (:q-plus tree) q-plus)
              p-plus'  (if (pos? v) (:p-plus tree) p-plus)
              continue?' (and (:s' tree)
                              (compute-tree-criterion q-minus' q-plus'
                                                      p-minus' p-plus'))]
          (recur (inc j)
                 q-minus' p-minus'
                 q-plus' p-plus'
                 q''
                 (+ depth-n (:n' tree))
                 continue?'))))))

(defn nuts
  "No-U-Turn Sampler (NUTS).

   opts: {:samples N          ;; number of samples
          :step-size eps      ;; leapfrog step size (default 0.01)
          :max-depth J        ;; max tree depth (default 10)
          :burn N              ;; burn-in (default 0)
          :thin N              ;; thinning (default 1)
          :compile? bool       ;; JIT-compile gradient fn (default true)
          :callback fn}

   log-density: (fn [params]) -> MLX scalar
   init-params: MLX array

   Returns vector of MLX arrays."
  [opts log-density init-params]
  (let [{:keys [samples step-size max-depth burn thin compile? callback]
         :or {step-size 0.01
              max-depth 10
              burn 0
              thin 1
              compile? true}} opts

        eps (mx/scalar step-size)
        half-eps (mx/scalar (* 0.5 step-size))
        half-scalar (mx/scalar 0.5)

        neg-log-density (fn [q] (mx/negative (log-density q)))
        grad-neg-ld (let [g (mx/grad neg-log-density)]
                      (if compile? (mx/compile-fn g) g))
        total-iters (+ burn (* samples thin))]

    (loop [i 0
           current-q init-params
           collected (transient [])
           n-collected 0]
      (if (>= n-collected samples)
        (persistent! collected)
        (let [new-q (nuts-step neg-log-density grad-neg-ld
                               current-q eps half-eps half-scalar max-depth)
              in-collection? (>= i burn)
              keep? (and in-collection? (zero? (mod (- i burn) thin)))]
          (when (and callback keep?)
            (callback {:iter n-collected :value (mx/->clj new-q)}))
          (recur (inc i)
                 new-q
                 (if keep? (conj! collected new-q) collected)
                 (if keep? (inc n-collected) n-collected)))))))

;; ---------------------------------------------------------------------------
;; Convenience
;; ---------------------------------------------------------------------------

(defn make-log-posterior
  "Build a differentiable log-posterior function from priors and likelihood.

   priors: vector of {:index i :log-prob-fn (fn [param-i]) -> MLX scalar}
   likelihood: (fn [params]) -> MLX scalar (log-likelihood)

   Returns (fn [params]) -> MLX scalar."
  [priors likelihood]
  (fn [params]
    (let [prior-lp (reduce
                     (fn [acc {:keys [index log-prob-fn]}]
                       (mx/add acc (log-prob-fn (mx/index params index))))
                     (mx/scalar 0.0)
                     priors)]
      (mx/add prior-lp (likelihood params)))))

;; ---------------------------------------------------------------------------
;; Summary statistics
;; ---------------------------------------------------------------------------

(defn sample-mean
  "Mean of parameter samples. Returns MLX array."
  [samples]
  (let [stacked (mx/stack samples)]
    (mx/mean stacked [0])))

(defn sample-std
  "Standard deviation of parameter samples. Returns MLX array."
  [samples]
  (let [stacked (mx/stack samples)]
    (mx/std stacked [0])))

(defn sample-quantiles
  "Compute quantiles of samples for each parameter dimension.
   Returns map {:median ... :q025 ... :q975 ...}."
  [samples]
  (let [stacked (mx/stack samples)
        n (first (mx/shape stacked))]
    (mx/eval! stacked)
    (let [sorted-vals (mx/->clj stacked)
          params (if (vector? (first sorted-vals))
                   (count (first sorted-vals))
                   1)]
      (if (= params 1)
        (let [vals (sort (if (vector? (first sorted-vals))
                           (map first sorted-vals)
                           sorted-vals))
              idx-025 (int (* 0.025 n))
              idx-50  (int (* 0.5 n))
              idx-975 (int (* 0.975 n))]
          {:median (nth vals idx-50)
           :q025 (nth vals idx-025)
           :q975 (nth vals idx-975)})
        (let [result (for [p (range params)]
                       (let [vals (sort (map #(nth % p) sorted-vals))
                             idx-025 (int (* 0.025 n))
                             idx-50  (int (* 0.5 n))
                             idx-975 (int (* 0.975 n))]
                         {:median (nth vals idx-50)
                          :q025 (nth vals idx-025)
                          :q975 (nth vals idx-975)}))]
          (vec result))))))
