(ns prob.mlx.inference
  "Gradient-based inference algorithms using MLX autograd.
   - HMC (Hamiltonian Monte Carlo)
   - NUTS (No-U-Turn Sampler)

   These operate on differentiable log-density functions:
   log-density-fn: (fn [params] ...) -> MLX scalar

   params is a single MLX array (vector of parameters).
   Returns MLX scalar log-density (unnormalized log-posterior)."
  (:require [prob.mlx.core :as mx]))

;; ---------------------------------------------------------------------------
;; Leapfrog integrator
;; ---------------------------------------------------------------------------

(defonce ^:private mx-core (.-core (js/require "@frost-beta/mlx")))

(defn- leapfrog
  "Single leapfrog step for HMC.
   grad-U: gradient of negative log-density (potential energy gradient)
   q: position (MLX array)
   p: momentum (MLX array)
   step-size: scalar
   Returns [q' p']. Wrapped in tidy to free intermediates."
  [grad-U q p step-size]
  (.tidy mx-core
    (fn []
      (let [eps (mx/scalar step-size)
            half-eps (mx/scalar (* 0.5 step-size))
            ;; Half step for momentum
            grad-q (grad-U q)
            _ (mx/eval! grad-q)
            p (mx/subtract p (mx/multiply half-eps grad-q))
            ;; Full step for position
            q (mx/add q (mx/multiply eps p))
            ;; Half step for momentum
            grad-q (grad-U q)
            _ (mx/eval! grad-q)
            p (mx/subtract p (mx/multiply half-eps grad-q))]
        (mx/eval! q p)
        #js [q p]))))

(defn- leapfrog-trajectory
  "Run L leapfrog steps. Returns [q' p']."
  [grad-U q p step-size L]
  (loop [i 0, q q, p p]
    (if (>= i L)
      [q p]
      (let [result (leapfrog grad-U q p step-size)
            q' (aget result 0)
            p' (aget result 1)]
        (recur (inc i) q' p')))))

;; ---------------------------------------------------------------------------
;; HMC kernel
;; ---------------------------------------------------------------------------

(defn- kinetic-energy
  "Kinetic energy: 0.5 * p^T @ p"
  [p]
  (mx/multiply (mx/scalar 0.5) (mx/sum (mx/multiply p p))))

(defn- hmc-step
  "Single HMC step.
   log-density: params -> MLX scalar (log-posterior)
   grad-neg-log-density: gradient of -log-density
   current-q: current position
   step-size: leapfrog step size
   n-leapfrog: number of leapfrog steps
   Returns [new-q accepted?]."
  [log-density grad-neg-log-density current-q step-size n-leapfrog]
  (let [;; Sample momentum (outside tidy since p0 is used in trajectory)
        p0 (mx/random-normal (mx/shape current-q))
        _ (mx/eval! p0)

        ;; Current Hamiltonian = -log-density + kinetic
        current-U (mx/negative (log-density current-q))
        current-K (kinetic-energy p0)
        _ (mx/eval! current-U current-K)
        current-H (+ (mx/item current-U) (mx/item current-K))

        ;; Leapfrog integration
        [proposed-q proposed-p] (leapfrog-trajectory
                                  grad-neg-log-density
                                  current-q p0
                                  step-size n-leapfrog)

        ;; Proposed Hamiltonian (in tidy to clean up intermediates)
        proposed-U (mx/negative (log-density proposed-q))
        proposed-K (kinetic-energy (mx/negative proposed-p))
        _ (mx/eval! proposed-U proposed-K)
        proposed-H (+ (mx/item proposed-U) (mx/item proposed-K))

        ;; Accept/reject (Metropolis step)
        log-accept (- current-H proposed-H)]

    ;; Dispose of arrays we no longer need
    (mx/dispose! p0)
    (mx/dispose! current-U)
    (mx/dispose! current-K)
    (mx/dispose! proposed-U)
    (mx/dispose! proposed-K)
    (when (not= proposed-p proposed-q)
      (mx/dispose! proposed-p))

    (if (or (> log-accept 0)
            (< (js/Math.log (js/Math.random)) log-accept))
      (do
        (when (not= current-q proposed-q)
          ;; Don't dispose current-q if it's rejected (we still need it)
          )
        [proposed-q true])
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
          :callback fn}        ;; (fn [{:iter i :value v :accepted? b}])

   log-density: (fn [params]) -> MLX scalar log-posterior
   init-params: MLX array, initial parameter values

   Returns vector of MLX arrays (samples)."
  [opts log-density init-params]
  (let [{:keys [samples step-size leapfrog-steps burn thin callback]
         :or {step-size 0.01
              leapfrog-steps 20
              burn 0
              thin 1}} opts
        ;; Gradient of negative log-density (potential energy gradient)
        grad-neg-log-density (mx/grad (fn [q] (mx/negative (log-density q))))
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
        (let [[new-q accepted?] (hmc-step log-density
                                          grad-neg-log-density
                                          current-q
                                          step-size
                                          leapfrog-steps)
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
    (and (>= (mx/item check-fwd) 0)
         (>= (mx/item check-bwd) 0))))

(defn- build-tree
  "Recursively build NUTS tree.
   log-u is log of the slice variable: log(u) where u ~ Uniform(0, exp(-H0)).
   Returns {:q-minus :p-minus :q-plus :p-plus :q' :n' :s' :alpha :n-alpha}."
  [log-density grad-neg-log-density q p log-u v step-size j current-H]
  (if (zero? j)
    ;; Base case: single leapfrog step
    (let [result (leapfrog grad-neg-log-density q p (* v step-size))
          q' (aget result 0)
          p' (aget result 1)
          proposed-U (mx/negative (log-density q'))
          proposed-K (kinetic-energy p')
          _ (mx/eval! proposed-U proposed-K)
          proposed-H (+ (mx/item proposed-U) (mx/item proposed-K))
          ;; Accept if log-u <= -proposed-H, i.e., proposed-H <= -log-u
          n' (if (<= log-u (- proposed-H)) 1 0)
          ;; Check if the state is not diverging (delta-H < 1000)
          s' (< (- proposed-H current-H) 1000)
          alpha (min 1.0 (js/Math.exp (- current-H proposed-H)))]
      (mx/dispose! proposed-U)
      (mx/dispose! proposed-K)
      {:q-minus q' :p-minus p' :q-plus q' :p-plus p'
       :q' q' :n' n' :s' s' :alpha alpha :n-alpha 1})

    ;; Recursive case: build two half-trees
    (let [tree1 (build-tree log-density grad-neg-log-density
                            q p log-u v step-size (dec j) current-H)]
      (if (not (:s' tree1))
        tree1
        (let [;; Build second half-tree from the appropriate endpoint
              [q2 p2] (if (pos? v)
                        [(:q-plus tree1) (:p-plus tree1)]
                        [(:q-minus tree1) (:p-minus tree1)])
              tree2 (build-tree log-density grad-neg-log-density
                                q2 p2 log-u v step-size (dec j) current-H)
              ;; Combine trees
              total-n (+ (:n' tree1) (:n' tree2))
              ;; Accept new proposal with probability n2/total-n
              q' (if (and (pos? total-n)
                          (< (js/Math.random) (/ (:n' tree2) total-n)))
                   (:q' tree2)
                   (:q' tree1))
              ;; Update endpoints
              q-minus (if (pos? v) (:q-minus tree1) (:q-minus tree2))
              p-minus (if (pos? v) (:p-minus tree1) (:p-minus tree2))
              q-plus  (if (pos? v) (:q-plus tree2)  (:q-plus tree1))
              p-plus  (if (pos? v) (:p-plus tree2)  (:p-plus tree1))
              ;; Check U-turn
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
  [log-density grad-neg-log-density current-q step-size max-depth]
  (let [p0 (mx/random-normal (mx/shape current-q))
        _ (mx/eval! p0)
        current-U (mx/negative (log-density current-q))
        current-K (kinetic-energy p0)
        _ (mx/eval! current-U current-K)
        current-H (+ (mx/item current-U) (mx/item current-K))
        ;; Slice variable: log(u) where u ~ Uniform(0, exp(-H0))
        ;; log(u) = log(rand()) + log(exp(-H0)) = log(rand()) - H0
        log-u (+ (js/Math.log (js/Math.random)) (- current-H))]

    (mx/dispose! current-U)
    (mx/dispose! current-K)

    (loop [j 0
           q-minus current-q, p-minus p0
           q-plus current-q, p-plus p0
           q' current-q
           depth-n 1
           continue? true]
      (if (or (not continue?) (>= j max-depth))
        (do
          (mx/dispose! p0)
          q')
        (let [v (if (< (js/Math.random) 0.5) -1 1)
              [q-start p-start] (if (pos? v)
                                  [q-plus p-plus]
                                  [q-minus p-minus])
              tree (build-tree log-density grad-neg-log-density
                               q-start p-start
                               log-u v step-size j current-H)
              ;; Accept proposal from tree
              q'' (if (and (:s' tree)
                           (< (js/Math.random) (/ (:n' tree) (max 1 depth-n))))
                    (:q' tree)
                    q')
              ;; Update endpoints
              q-minus' (if (neg? v) (:q-minus tree) q-minus)
              p-minus' (if (neg? v) (:p-minus tree) p-minus)
              q-plus'  (if (pos? v) (:q-plus tree) q-plus)
              p-plus'  (if (pos? v) (:p-plus tree) p-plus)
              ;; Check overall U-turn
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
          :callback fn}

   log-density: (fn [params]) -> MLX scalar
   init-params: MLX array

   Returns vector of MLX arrays."
  [opts log-density init-params]
  (let [{:keys [samples step-size max-depth burn thin callback]
         :or {step-size 0.01
              max-depth 10
              burn 0
              thin 1}} opts
        grad-neg-log-density (mx/grad (fn [q] (mx/negative (log-density q))))
        total-iters (+ burn (* samples thin))]

    (loop [i 0
           current-q init-params
           collected (transient [])
           n-collected 0]
      (if (>= n-collected samples)
        (persistent! collected)
        (let [new-q (nuts-step log-density grad-neg-log-density
                               current-q step-size max-depth)
              in-collection? (>= i burn)
              keep? (and in-collection? (zero? (mod (- i burn) thin)))]
          (when (and callback keep?)
            (callback {:iter n-collected :value (mx/->clj new-q)}))
          (recur (inc i)
                 new-q
                 (if keep? (conj! collected new-q) collected)
                 (if keep? (inc n-collected) n-collected)))))))

;; ---------------------------------------------------------------------------
;; Convenience: build log-posterior from model specification
;; ---------------------------------------------------------------------------

(defn make-log-posterior
  "Build a differentiable log-posterior function from priors and likelihood.

   priors: vector of {:index i :log-prob (fn [param-i]) -> MLX scalar}
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
;; Summary statistics for samples
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
    ;; Sort along sample axis and pick indices
    (let [sorted-vals (mx/->clj stacked)
          ;; sorted-vals is a vector of vectors (or vector of numbers)
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
