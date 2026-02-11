(ns prob.dist
  "Distribution protocol and implementations.
   Each distribution supports sampling (sample*) and log-probability scoring (observe*).

   Constructors: bernoulli-dist, gaussian-dist, uniform-dist, beta-dist, gamma-dist,
   exponential-dist, dirichlet-dist, uniform-draw-dist, random-integer-dist,
   multinomial-dist, sample-discrete-dist."
  (:require [prob.erp :as erp]
            [prob.math :as math]))

;; ---------------------------------------------------------------------------
;; Constants
;; ---------------------------------------------------------------------------

(def ^:private log-2pi (js/Math.log (* 2.0 js/Math.PI)))

(defn- safe-mul-log
  "a * log(x), with 0 * log(0) = 0 by convention.
   Avoids NaN from 0 * -Infinity in JavaScript."
  [a x]
  (if (zero? a) 0.0 (* a (js/Math.log x))))

;; ---------------------------------------------------------------------------
;; Protocol
;; ---------------------------------------------------------------------------

(defprotocol IDistribution
  (sample* [this] "Draw a random sample from this distribution.")
  (observe* [this value] "Return the log-probability of observing value."))

(defprotocol IEnumerable
  (enumerate* [this] "Return a vector of all possible values in the support."))

(defprotocol IProposable
  (propose* [this current-value] "Returns [new-value fwd-lp rev-lp].")
  (propose-lp [this from-value to-value] "Log-probability of proposing to-value given from-value."))

(defn dist?
  "Returns true if x implements IDistribution."
  [x]
  (satisfies? IDistribution x))

;; ---------------------------------------------------------------------------
;; Discrete proposal helper
;; ---------------------------------------------------------------------------

(defn- exclude-current-proposal
  "Propose a new value from support by excluding the current value.
   Returns [new-value fwd-lp rev-lp]."
  [support current-value]
  (let [other (filterv #(not= % current-value) support)
        n-other (count other)]
    (if (zero? n-other)
      [current-value 0.0 0.0]
      (let [new-value (nth other (js/Math.floor (* (erp/rand) n-other)))
            n-rev-other (count (filterv #(not= % new-value) support))
            fwd-lp (- (js/Math.log n-other))
            rev-lp (- (js/Math.log n-rev-other))]
        [new-value fwd-lp rev-lp]))))

;; ---------------------------------------------------------------------------
;; Bernoulli (flip)
;; ---------------------------------------------------------------------------

(defrecord Bernoulli [p]
  IDistribution
  (sample* [this]
    (if erp/*trace-state*
      (erp/trace-choice! :flip
        #(binding [erp/*trace-state* nil] (erp/flip p))
        #(if % (js/Math.log p) (js/Math.log (- 1.0 p)))
        [true false]
        this)
      (erp/flip p)))
  (observe* [_ value]
    (if value
      (js/Math.log p)
      (js/Math.log (- 1.0 p))))
  IEnumerable
  (enumerate* [_] [true false])
  IProposable
  (propose* [_ cv] [(not cv) 0.0 0.0])
  (propose-lp [_ _from _to] 0.0))

(defn bernoulli-dist
  "Bernoulli distribution. (bernoulli-dist) defaults to p=0.5."
  ([] (->Bernoulli 0.5))
  ([p] (->Bernoulli p)))

;; ---------------------------------------------------------------------------
;; Gaussian (normal)
;; ---------------------------------------------------------------------------

(defn- gaussian-log-prob [mu sigma x]
  (if (zero? sigma)
    (if (== x mu) 0.0 ##-Inf)
    (let [z (/ (- x mu) sigma)]
      (* -0.5 (+ log-2pi
                 (* 2.0 (js/Math.log sigma))
                 (* z z))))))

(defrecord Gaussian [mu sigma]
  IDistribution
  (sample* [this]
    (if erp/*trace-state*
      (erp/trace-choice! :gaussian
        #(binding [erp/*trace-state* nil] (erp/gaussian mu sigma))
        #(gaussian-log-prob mu sigma %)
        nil
        this)
      (erp/gaussian mu sigma)))
  (observe* [_ x] (gaussian-log-prob mu sigma x))
  IProposable
  (propose* [_ current-value]
    ;; Symmetric drift: propose from N(current, sigma*0.5)
    (let [drift-sigma (* sigma 0.5)
          new-value (erp/gaussian current-value drift-sigma)
          fwd-lp (gaussian-log-prob current-value drift-sigma new-value)
          rev-lp fwd-lp] ;; symmetric
      [new-value fwd-lp rev-lp]))
  (propose-lp [_ from-value to-value]
    (gaussian-log-prob from-value (* sigma 0.5) to-value)))

(defn gaussian-dist
  "Gaussian (normal) distribution. (gaussian-dist) defaults to N(0,1)."
  ([] (->Gaussian 0 1))
  ([mu sigma] (->Gaussian mu sigma)))

;; ---------------------------------------------------------------------------
;; Uniform (continuous)
;; ---------------------------------------------------------------------------

(defrecord Uniform [a b]
  IDistribution
  (sample* [_] (erp/uniform a b))
  (observe* [_ x]
    (cond
      (== a b) (if (== x a) 0.0 ##-Inf)
      (or (< x a) (> x b)) ##-Inf
      :else (- (js/Math.log (- b a))))))

(defn uniform-dist
  "Continuous uniform distribution on [a, b]."
  [a b]
  (->Uniform a b))

;; ---------------------------------------------------------------------------
;; Beta
;; ---------------------------------------------------------------------------

(defn- beta-log-prob [a b x]
  (if (or (< x 0) (> x 1))
    ##-Inf
    (+ (safe-mul-log (dec a) x)
       (safe-mul-log (dec b) (- 1.0 x))
       (- (math/log-beta-fn a b)))))

(defrecord Beta [a b]
  IDistribution
  (sample* [this]
    (if erp/*trace-state*
      (erp/trace-choice! :beta
        #(binding [erp/*trace-state* nil] (erp/beta a b))
        #(beta-log-prob a b %)
        nil
        this)
      (erp/beta a b)))
  (observe* [_ x] (beta-log-prob a b x))
  IProposable
  (propose* [_ current-value]
    ;; Beta drift: propose from Beta(kappa*current, kappa*(1-current))
    ;; kappa=10 for moderate concentration that mixes well
    (let [kappa 10.0
          alpha (max (* kappa current-value) 0.5)
          beta-param (max (* kappa (- 1.0 current-value)) 0.5)
          new-value (erp/beta alpha beta-param)
          fwd-lp (beta-log-prob alpha beta-param new-value)
          rev-alpha (max (* kappa new-value) 0.5)
          rev-beta (max (* kappa (- 1.0 new-value)) 0.5)
          rev-lp (beta-log-prob rev-alpha rev-beta current-value)]
      [new-value fwd-lp rev-lp]))
  (propose-lp [_ from-value to-value]
    (let [kappa 10.0
          alpha (max (* kappa from-value) 0.5)
          beta-param (max (* kappa (- 1.0 from-value)) 0.5)]
      (beta-log-prob alpha beta-param to-value))))

(defn beta-dist
  "Beta distribution with shape parameters a (alpha) and b (beta)."
  [a b]
  (->Beta a b))

;; ---------------------------------------------------------------------------
;; Gamma
;; ---------------------------------------------------------------------------

(defrecord Gamma [shape scale]
  IDistribution
  (sample* [_] (erp/gamma shape scale))
  (observe* [_ x]
    (if (< x 0)
      ##-Inf
      (+ (safe-mul-log (dec shape) x)
         (- (/ x scale))
         (- (* shape (js/Math.log scale)))
         (- (math/log-gamma-fn shape))))))

(defn gamma-dist
  "Gamma distribution with given shape and scale."
  [shape scale]
  (->Gamma shape scale))

;; ---------------------------------------------------------------------------
;; Exponential
;; ---------------------------------------------------------------------------

(defrecord Exponential [rate]
  IDistribution
  (sample* [_] (erp/exponential rate))
  (observe* [_ x]
    (if (< x 0)
      ##-Inf
      (- (js/Math.log rate) (* rate x)))))

(defn exponential-dist
  "Exponential distribution with given rate (inverse scale)."
  [rate]
  (->Exponential rate))

;; ---------------------------------------------------------------------------
;; Dirichlet
;; ---------------------------------------------------------------------------

(defrecord Dirichlet [alpha]
  IDistribution
  (sample* [_] (erp/dirichlet alpha))
  (observe* [_ x]
    (let [x (vec x)]
      (if (or (not= (count x) (count alpha))
              (some #(< % 0) x))
        ##-Inf
        (+ (math/log-gamma-fn (reduce + alpha))
           (- (reduce + (map math/log-gamma-fn alpha)))
           (reduce + (map (fn [ai xi] (safe-mul-log (dec ai) xi))
                          alpha x)))))))

(defn dirichlet-dist
  "Dirichlet distribution with concentration parameter vector alpha."
  [alpha]
  (->Dirichlet (vec alpha)))

;; ---------------------------------------------------------------------------
;; Uniform draw (discrete uniform over items)
;; ---------------------------------------------------------------------------

(defrecord UniformDraw [items]
  IDistribution
  (sample* [this]
    (if erp/*trace-state*
      (erp/trace-choice! :uniform-draw
        #(binding [erp/*trace-state* nil] (erp/uniform-draw items))
        #(let [n (count items)
               k (count (filter (fn [i] (= i %)) items))]
           (if (zero? k) ##-Inf (js/Math.log (/ (double k) (double n)))))
        (vec (distinct items))
        this)
      (erp/uniform-draw items)))
  (observe* [_ value]
    (let [n (count items)
          k (count (filter #(= % value) items))]
      (if (zero? k)
        ##-Inf
        (js/Math.log (/ (double k) (double n))))))
  IEnumerable
  (enumerate* [_] (vec (distinct items)))
  IProposable
  (propose* [_ cv] (exclude-current-proposal (vec (distinct items)) cv))
  (propose-lp [_ _from _to] 0.0))

(defn uniform-draw-dist
  "Discrete uniform distribution over items."
  [items]
  (->UniformDraw (vec items)))

;; ---------------------------------------------------------------------------
;; Random integer [0, n)
;; ---------------------------------------------------------------------------

(defrecord RandomInteger [n]
  IDistribution
  (sample* [this]
    (if erp/*trace-state*
      (erp/trace-choice! :random-integer
        #(binding [erp/*trace-state* nil] (erp/random-integer n))
        #(if (and (integer? %) (>= % 0) (< % n))
           (- (js/Math.log n))
           ##-Inf)
        (vec (range n))
        this)
      (erp/random-integer n)))
  (observe* [_ x]
    (if (and (integer? x) (>= x 0) (< x n))
      (- (js/Math.log n))
      ##-Inf))
  IEnumerable
  (enumerate* [_] (vec (range n)))
  IProposable
  (propose* [_ cv]
    (let [support (vec (range n))]
      (exclude-current-proposal support cv)))
  (propose-lp [_ _from _to] 0.0))

(defn random-integer-dist
  "Discrete uniform distribution over integers [0, n)."
  [n]
  (->RandomInteger n))

;; ---------------------------------------------------------------------------
;; Multinomial (weighted discrete choice over labeled items)
;; ---------------------------------------------------------------------------

(defrecord Multinomial [items probs]
  IDistribution
  (sample* [this]
    (if erp/*trace-state*
      (erp/trace-choice! :multinomial
        #(binding [erp/*trace-state* nil] (erp/multinomial items probs))
        #(let [total (reduce + 0.0 probs)
               value-prob (reduce + 0.0
                            (map (fn [item prob]
                                   (if (= item %) prob 0.0))
                                 items probs))]
           (if (zero? value-prob) ##-Inf (js/Math.log (/ value-prob total))))
        (vec (distinct items))
        this)
      (erp/multinomial items probs)))
  (observe* [_ value]
    (let [total (reduce + 0.0 probs)
          value-prob (reduce + 0.0
                       (map (fn [item prob]
                              (if (= item value) prob 0.0))
                            items probs))]
      (if (zero? value-prob)
        ##-Inf
        (js/Math.log (/ value-prob total)))))
  IEnumerable
  (enumerate* [_] (vec (distinct items)))
  IProposable
  (propose* [_ cv] (exclude-current-proposal (vec (distinct items)) cv))
  (propose-lp [_ _from _to] 0.0))

(defn multinomial-dist
  "Weighted discrete distribution over labeled items."
  [items probs]
  (->Multinomial (vec items) (vec probs)))

;; ---------------------------------------------------------------------------
;; Sample-discrete (weighted index selection)
;; ---------------------------------------------------------------------------

(defrecord SampleDiscrete [weights]
  IDistribution
  (sample* [_] (erp/sample-discrete weights))
  (observe* [_ idx]
    (if (or (not (integer? idx))
            (neg? idx)
            (>= idx (count weights)))
      ##-Inf
      (let [total (reduce + 0.0 weights)]
        (js/Math.log (/ (nth weights idx) total)))))
  IEnumerable
  (enumerate* [_] (vec (range (count weights)))))

(defn sample-discrete-dist
  "Weighted discrete distribution over indices."
  [weights]
  (->SampleDiscrete (vec weights)))

;; ---------------------------------------------------------------------------
;; Binomial
;; ---------------------------------------------------------------------------

(defrecord Binomial [n p]
  IDistribution
  (sample* [_] (erp/binomial n p))
  (observe* [_ k]
    (if (or (not (integer? k)) (neg? k) (> k n))
      ##-Inf
      (+ (- (math/log-fact n) (math/log-fact k) (math/log-fact (- n k)))
         (if (pos? k) (* k (js/Math.log p)) 0.0)
         (if (< k n) (* (- n k) (js/Math.log (- 1.0 p))) 0.0))))
  IEnumerable
  (enumerate* [_] (vec (range (inc n)))))

(defn binomial-dist
  "Binomial distribution: number of successes in n trials with probability p."
  [n p]
  (->Binomial n p))

;; ---------------------------------------------------------------------------
;; Poisson
;; ---------------------------------------------------------------------------

(defrecord Poisson [lambda]
  IDistribution
  (sample* [_] (erp/poisson lambda))
  (observe* [_ k]
    (if (or (not (integer? k)) (neg? k))
      ##-Inf
      (- (* k (js/Math.log lambda)) lambda (math/log-fact k)))))

(defn poisson-dist
  "Poisson distribution with rate parameter lambda."
  [lambda]
  (->Poisson lambda))

;; ---------------------------------------------------------------------------
;; Categorical (weighted discrete over labeled categories)
;; ---------------------------------------------------------------------------

(defrecord Categorical [categories weights]
  IDistribution
  (sample* [this]
    (if erp/*trace-state*
      (erp/trace-choice! :categorical
        #(binding [erp/*trace-state* nil] (erp/categorical categories weights))
        #(let [total (reduce + 0.0 weights)
               value-prob (reduce + 0.0
                            (map (fn [cat w]
                                   (if (= cat %) w 0.0))
                                 categories weights))]
           (if (zero? value-prob) ##-Inf (js/Math.log (/ value-prob total))))
        (vec (distinct categories))
        this)
      (erp/categorical categories weights)))
  (observe* [_ value]
    (let [total (reduce + 0.0 weights)
          value-prob (reduce + 0.0
                       (map (fn [cat w]
                              (if (= cat value) w 0.0))
                            categories weights))]
      (if (zero? value-prob)
        ##-Inf
        (js/Math.log (/ value-prob total)))))
  IEnumerable
  (enumerate* [_] (vec (distinct categories)))
  IProposable
  (propose* [_ cv] (exclude-current-proposal (vec (distinct categories)) cv))
  (propose-lp [_ _from _to] 0.0))

(defn categorical-dist
  "Categorical distribution. Accepts either a map {category weight ...}
   or two sequences [categories] [weights]."
  ([weight-map]
   (let [cats (vec (keys weight-map))
         ws (vec (vals weight-map))]
     (->Categorical cats ws)))
  ([categories weights]
   (->Categorical (vec categories) (vec weights))))

;; ---------------------------------------------------------------------------
;; Delta (point mass)
;; ---------------------------------------------------------------------------

(defrecord Delta [v]
  IDistribution
  (sample* [_] v)
  (observe* [_ x] (if (= x v) 0.0 ##-Inf))
  IEnumerable
  (enumerate* [_] [v]))

(defn delta-dist
  "Delta (point mass) distribution. Always returns v."
  [v]
  (->Delta v))

;; ---------------------------------------------------------------------------
;; Cauchy
;; ---------------------------------------------------------------------------

(defn- cauchy-log-prob [location scale x]
  (let [z (/ (- x location) scale)]
    (- (- (js/Math.log js/Math.PI))
       (js/Math.log scale)
       (js/Math.log (+ 1.0 (* z z))))))

(defrecord Cauchy [location scale]
  IDistribution
  (sample* [this]
    (let [raw-sample (fn []
                       (+ location (* scale (js/Math.tan (* js/Math.PI (- (erp/rand) 0.5))))))]
      (if erp/*trace-state*
        (erp/trace-choice! :cauchy raw-sample #(cauchy-log-prob location scale %))
        (raw-sample))))
  (observe* [_ x] (cauchy-log-prob location scale x)))

(defn cauchy-dist
  "Cauchy distribution with given location and scale."
  [location scale]
  (->Cauchy location scale))

;; ---------------------------------------------------------------------------
;; Laplace
;; ---------------------------------------------------------------------------

(defn- laplace-log-prob [location scale x]
  (- (- (js/Math.log (* 2.0 scale)))
     (/ (js/Math.abs (- x location)) scale)))

(defrecord Laplace [location scale]
  IDistribution
  (sample* [this]
    (let [raw-sample (fn []
                       (let [u (- (erp/rand) 0.5)]
                         (- location (* scale (js/Math.sign u)
                                       (js/Math.log (- 1.0 (* 2.0 (js/Math.abs u))))))))]
      (if erp/*trace-state*
        (erp/trace-choice! :laplace raw-sample #(laplace-log-prob location scale %))
        (raw-sample))))
  (observe* [_ x] (laplace-log-prob location scale x)))

(defn laplace-dist
  "Laplace distribution with given location and scale."
  [location scale]
  (->Laplace location scale))

;; ---------------------------------------------------------------------------
;; LogNormal
;; ---------------------------------------------------------------------------

(defn- lognormal-log-prob [mu sigma x]
  (if (<= x 0)
    ##-Inf
    (- (gaussian-log-prob mu sigma (js/Math.log x))
       (js/Math.log x))))

(defrecord LogNormal [mu sigma]
  IDistribution
  (sample* [this]
    (let [raw-sample (fn []
                       (binding [erp/*trace-state* nil]
                         (js/Math.exp (erp/gaussian mu sigma))))]
      (if erp/*trace-state*
        (erp/trace-choice! :lognormal raw-sample #(lognormal-log-prob mu sigma %))
        (raw-sample))))
  (observe* [_ x] (lognormal-log-prob mu sigma x)))

(defn lognormal-dist
  "Log-normal distribution. exp(Gaussian(mu, sigma))."
  [mu sigma]
  (->LogNormal mu sigma))

;; ---------------------------------------------------------------------------
;; Student-t
;; ---------------------------------------------------------------------------

(defn- student-t-log-prob [df location scale x]
  (let [z (/ (- x location) scale)]
    (+ (math/log-gamma-fn (/ (inc df) 2.0))
       (- (math/log-gamma-fn (/ df 2.0)))
       (* -0.5 (js/Math.log (* df js/Math.PI)))
       (- (js/Math.log scale))
       (* (/ (- (inc df)) 2.0) (js/Math.log (+ 1.0 (/ (* z z) df)))))))

(defrecord StudentT [df location scale]
  IDistribution
  (sample* [this]
    (let [raw-sample (fn []
                       (binding [erp/*trace-state* nil]
                         (let [z (erp/gaussian 0 1)
                               v (erp/gamma (/ df 2.0) (/ 2.0 df))]
                           (+ location (* scale (/ z (js/Math.sqrt v)))))))]
      (if erp/*trace-state*
        (erp/trace-choice! :student-t raw-sample #(student-t-log-prob df location scale %))
        (raw-sample))))
  (observe* [_ x] (student-t-log-prob df location scale x)))

(defn student-t-dist
  "Student's t-distribution with df degrees of freedom, location, and scale."
  ([df] (->StudentT df 0 1))
  ([df location scale] (->StudentT df location scale)))

;; ---------------------------------------------------------------------------
;; Mixture
;; ---------------------------------------------------------------------------

(defn- mixture-log-prob [dists weights x]
  (let [log-total (js/Math.log (reduce + 0.0 weights))
        log-terms (mapv (fn [d w]
                          (+ (js/Math.log (double w)) (observe* d x)))
                        dists weights)]
    (- (math/log-sum-exp log-terms) log-total)))

(defrecord Mixture [dists weights]
  IDistribution
  (sample* [this]
    (let [raw-sample (fn []
                       (binding [erp/*trace-state* nil]
                         (let [total (reduce + 0.0 weights)
                               r (* (erp/rand) total)]
                           (loop [i 0, cumulative 0.0]
                             (if (>= i (count dists))
                               (sample* (peek dists))
                               (let [cumulative (+ cumulative (nth weights i))]
                                 (if (< r cumulative)
                                   (sample* (nth dists i))
                                   (recur (inc i) cumulative))))))))]
      (if erp/*trace-state*
        (erp/trace-choice! :mixture raw-sample #(mixture-log-prob dists weights %))
        (raw-sample))))
  (observe* [_ x] (mixture-log-prob dists weights x)))

(defn mixture-dist
  "Mixture distribution: weighted combination of component distributions.
   dists is a seq of distributions, weights is a seq of non-negative weights."
  [dists weights]
  (->Mixture (vec dists) (vec weights)))

;; ---------------------------------------------------------------------------
;; Marginal (cached marginal distribution via inner inference)
;; ---------------------------------------------------------------------------

(defn- ensure-marginal-cache!
  "Compute and cache the marginal distribution on first use.
   Inner inference runs isolated from any outer trace."
  [marginal]
  (let [c @(:cache marginal)]
    (if c
      c
      (let [result (binding [erp/*trace-state* nil]
                     ((:method marginal) (:thunk marginal)))]
        (vreset! (:cache marginal) result)
        result))))

(defrecord Marginal [method thunk cache]
  IDistribution
  (sample* [this]
    (let [[values probs] (ensure-marginal-cache! this)
          vs (vec values)
          ps (vec probs)]
      (erp/multinomial vs ps)))
  (observe* [this value]
    (let [[values probs] (ensure-marginal-cache! this)
          prob-map (zipmap (seq values) (seq probs))
          p (get prob-map value 0.0)]
      (if (zero? p) ##-Inf (js/Math.log p))))
  IEnumerable
  (enumerate* [this]
    (let [[values _probs] (ensure-marginal-cache! this)]
      (vec values))))

(defn marginal-dist
  "Create a marginal distribution by running inner inference.
   method is an inference function like enumeration-query-fn.
   thunk is a zero-arg function defining the model."
  [method thunk]
  (->Marginal method thunk (volatile! nil)))
