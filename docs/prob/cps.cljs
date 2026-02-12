(ns prob.cps
  "CPS transformation for Sequential Monte Carlo inference.

   Checkpoint records represent paused computations:
   - Sample: particle wants to sample from a distribution
   - Observe: particle hit an observation point
   - Factor: particle accumulated a log-weight
   - Result: particle finished execution

   cps-of-expr transforms model code so that at each probabilistic
   operation, execution yields a checkpoint record containing the
   continuation. The SMC driver processes these checkpoints across
   all particles."
  (:require [prob.dist :as dist]
            [prob.erp :as erp]
            [prob.cps-transform :as ct]))

;; ---------------------------------------------------------------------------
;; Checkpoint records
;; ---------------------------------------------------------------------------

(defrecord Sample [addr dist cont state])
(defrecord Observe [dist value cont state])
(defrecord Factor [score cont state])
(defrecord Result [value state])

(defn sample? [x] (instance? Sample x))
(defn observe? [x] (instance? Observe x))
(defn factor? [x] (instance? Factor x))
(defn result? [x] (instance? Result x))
(defn checkpoint? [x] (or (sample? x) (observe? x) (factor? x) (result? x)))

;; ---------------------------------------------------------------------------
;; CPS runtime helpers (called by generated code)
;; ---------------------------------------------------------------------------

(defn cps-sample
  "CPS version of sample*. Emits a Sample checkpoint.
   The SMC driver will draw from dist and call cont with the value."
  [dist cont state]
  (let [addr (:addr state 0)
        state (update state :addr (fnil inc 0))]
    (->Sample addr dist cont state)))

(defn cps-observe
  "CPS version of observe. Emits an Observe checkpoint.
   The SMC driver will accumulate the log-weight and call cont."
  [dist value cont state]
  (->Observe dist value cont state))

(defn cps-factor
  "CPS version of factor. Emits a Factor checkpoint."
  [score cont state]
  (->Factor score cont state))

(defn cps-condition
  "CPS version of condition. Hard constraint as -Infinity weight."
  [pred cont state]
  (if pred
    (cont nil state)
    (->Factor ##-Inf cont state)))

;; ---------------------------------------------------------------------------
;; Re-export CPS transform from shared .cljc module
;; ---------------------------------------------------------------------------

(def cps-of-expr
  "Transform a form into CPS. Delegates to prob.cps-transform/cps-of-expr."
  ct/cps-of-expr)
