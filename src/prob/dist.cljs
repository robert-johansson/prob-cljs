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

(defn dist?
  "Returns true if x implements IDistribution."
  [x]
  (satisfies? IDistribution x))

;; ---------------------------------------------------------------------------
;; Bernoulli (flip)
;; ---------------------------------------------------------------------------

(defrecord Bernoulli [p]
  IDistribution
  (sample* [_] (erp/flip p))
  (observe* [_ value]
    (if value
      (js/Math.log p)
      (js/Math.log (- 1.0 p)))))

(defn bernoulli-dist
  "Bernoulli distribution. (bernoulli-dist) defaults to p=0.5."
  ([] (->Bernoulli 0.5))
  ([p] (->Bernoulli p)))

;; ---------------------------------------------------------------------------
;; Gaussian (normal)
;; ---------------------------------------------------------------------------

(defrecord Gaussian [mu sigma]
  IDistribution
  (sample* [_] (erp/gaussian mu sigma))
  (observe* [_ x]
    (if (zero? sigma)
      (if (== x mu) 0.0 ##-Inf)
      (let [z (/ (- x mu) sigma)]
        (* -0.5 (+ log-2pi
                   (* 2.0 (js/Math.log sigma))
                   (* z z)))))))

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

(defrecord Beta [a b]
  IDistribution
  (sample* [_] (erp/beta a b))
  (observe* [_ x]
    (if (or (< x 0) (> x 1))
      ##-Inf
      (+ (safe-mul-log (dec a) x)
         (safe-mul-log (dec b) (- 1.0 x))
         (- (math/log-beta-fn a b))))))

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
  (sample* [_] (erp/uniform-draw items))
  (observe* [_ value]
    (let [n (count items)
          k (count (filter #(= % value) items))]
      (if (zero? k)
        ##-Inf
        (js/Math.log (/ (double k) (double n)))))))

(defn uniform-draw-dist
  "Discrete uniform distribution over items."
  [items]
  (->UniformDraw (vec items)))

;; ---------------------------------------------------------------------------
;; Random integer [0, n)
;; ---------------------------------------------------------------------------

(defrecord RandomInteger [n]
  IDistribution
  (sample* [_] (erp/random-integer n))
  (observe* [_ x]
    (if (and (integer? x) (>= x 0) (< x n))
      (- (js/Math.log n))
      ##-Inf)))

(defn random-integer-dist
  "Discrete uniform distribution over integers [0, n)."
  [n]
  (->RandomInteger n))

;; ---------------------------------------------------------------------------
;; Multinomial (weighted discrete choice over labeled items)
;; ---------------------------------------------------------------------------

(defrecord Multinomial [items probs]
  IDistribution
  (sample* [_] (erp/multinomial items probs))
  (observe* [_ value]
    (let [total (reduce + 0.0 probs)
          value-prob (reduce + 0.0
                       (map (fn [item prob]
                              (if (= item value) prob 0.0))
                            items probs))]
      (if (zero? value-prob)
        ##-Inf
        (js/Math.log (/ value-prob total))))))

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
        (js/Math.log (/ (nth weights idx) total))))))

(defn sample-discrete-dist
  "Weighted discrete distribution over indices."
  [weights]
  (->SampleDiscrete (vec weights)))
