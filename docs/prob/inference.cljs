(ns prob.inference
  "Inference algorithms: rejection sampling, trace-based single-site MH,
   approximate enumeration, and Sequential Monte Carlo (SMC).

   MH uses persistent hash-maps as execution traces. Each trace maps
   sequential integer addresses to {:value :log-prob :erp-id} entries.
   Proposals change one address at a time; structural sharing makes
   this O(log32 n) per step.

   SMC uses CPS-transformed models with checkpoint records. Particles
   are paused at observe points, resampled by weight, and resumed."
  (:require [prob.erp :as erp]
            [prob.dist :as dist]
            [prob.math :as math]
            [prob.cps :as cps]))

;; ---------------------------------------------------------------------------
;; Condition / Factor support
;; ---------------------------------------------------------------------------

(def ^:private rejection-sentinel ::rejection)

(def ^:dynamic *forward-mode*
  "When true, condition and factor are no-ops (forward sampling mode)."
  false)

(defn condition [pred]
  (when-not *forward-mode*
    (when-not pred
      (throw (ex-info "condition failed" {::type rejection-sentinel}))))
  nil)

(defn factor [log-weight]
  (when-not *forward-mode*
    (if erp/*trace-state*
      ;; MH mode: accumulate score deterministically
      (vswap! erp/*trace-state* update :score + log-weight)
      ;; Rejection mode: probabilistic rejection
      (when (neg? log-weight)
        (when (> (js/Math.log (erp/rand)) log-weight)
          (throw (ex-info "factor rejected" {::type rejection-sentinel}))))))
  nil)

(defn- rejection? [e]
  (= (::type (ex-data e)) rejection-sentinel))

(defn observe
  "Condition on observing value from distribution dist.
   Equivalent to (factor (observe* dist value))."
  [dist value]
  (factor (dist/observe* dist value)))

;; ---------------------------------------------------------------------------
;; Rejection Sampling
;; ---------------------------------------------------------------------------

(def ^:private max-rejection-attempts 100000)

(defn rejection-query-fn
  "Run a thunk repeatedly until it doesn't throw a rejection.
   Returns the value of the thunk on the first successful run."
  [thunk]
  (loop [attempts 0]
    (if (>= attempts max-rejection-attempts)
      (throw (ex-info "rejection-query: exceeded maximum attempts" {:attempts max-rejection-attempts}))
      (let [result (try
                     {:value (thunk)}
                     (catch :default e
                       (if (rejection? e)
                         ::rejected
                         (throw e))))]
        (if (= result ::rejected)
          (recur (inc attempts))
          (:value result))))))

;; ---------------------------------------------------------------------------
;; Forward Sampling
;; ---------------------------------------------------------------------------

(defn forward-query-fn
  "Forward sampling: run thunk n times from the prior.
   factor and observe are no-ops, condition is a no-op.
   Returns a list of n prior samples."
  [n thunk]
  (binding [*forward-mode* true]
    (vec (repeatedly n thunk))))

;; ---------------------------------------------------------------------------
;; Traced execution (for MH)
;; ---------------------------------------------------------------------------

(defn- make-fresh-state
  "Create trace state for an initial (unguided) execution."
  []
  (volatile! {:counter 0
              :old-trace {}
              :proposal-addr -1
              :new-trace {}
              :score 0.0
              :mem-cache {}}))

(defn- make-replay-state
  "Create trace state for replaying with a proposal at one address."
  ([old-trace proposal-addr]
   (make-replay-state old-trace proposal-addr {} nil))
  ([old-trace proposal-addr old-mem-cache]
   (make-replay-state old-trace proposal-addr old-mem-cache nil))
  ([old-trace proposal-addr old-mem-cache propose-fn]
   (volatile! {:counter 0
               :old-trace old-trace
               :proposal-addr proposal-addr
               :new-trace {}
               :score 0.0
               :mem-cache old-mem-cache
               :propose-fn propose-fn})))

(defn- execute-traced
  "Execute thunk with tracing active.
   Returns {:trace :score :value :size :mem-cache} on success, nil if condition fails."
  [thunk trace-state]
  (binding [erp/*trace-state* trace-state]
    (try
      (let [value (thunk)
            {:keys [new-trace score mem-cache]} @trace-state]
        {:trace new-trace
         :score score
         :value value
         :size (count new-trace)
         :mem-cache mem-cache})
      (catch :default e
        (if (rejection? e)
          nil
          (throw e))))))

;; ---------------------------------------------------------------------------
;; Single-site Metropolis-Hastings
;; ---------------------------------------------------------------------------

(defn- drift-propose-fn
  "Propose-fn passed via trace state. Uses IProposable protocol for drift proposals.
   Returns [new-value fwd-lp rev-lp] or nil if dist doesn't support drift.
   Temporarily unbinds *trace-state* to prevent recursive tracing."
  [d current-value]
  (when (satisfies? dist/IProposable d)
    (binding [erp/*trace-state* nil]
      (dist/propose* d current-value))))

(defn- mh-step
  "One MH step: propose at a random address, accept or reject.
   Pure function from old MH state to new MH state."
  [thunk old-state]
  (let [{old-trace :trace, old-score :score, old-size :size,
         old-mem-cache :mem-cache} old-state
        ;; Pick random address from the current trace
        addr (js/Math.floor (* (erp/rand) old-size))
        ;; Re-execute with proposal at chosen address
        ts (make-replay-state old-trace addr old-mem-cache drift-propose-fn)
        new-result (execute-traced thunk ts)]
    (if (nil? new-result)
      ;; Condition failed → reject proposal, keep old state
      old-state
      ;; Compute MH acceptance ratio:
      ;; For prior proposals: α = exp(score' - score) × (old_size / new_size)
      ;; For drift proposals: add p(x'_i)/p(x_i) × q(x_i|x'_i)/q(x'_i|x_i)
      (let [{new-score :score, new-size :size} new-result
            ;; Check for drift proposal correction from the proposed entry
            old-entry (get old-trace addr)
            new-entry (get (:trace new-result) addr)
            fwd-lp (:fwd-lp new-entry)
            rev-lp (:rev-lp new-entry)
            ;; When drift was used, include prior log-prob ratio + proposal asymmetry
            proposal-correction (if fwd-lp
                                  (+ (- (:log-prob new-entry) (:log-prob old-entry))
                                     (- rev-lp fwd-lp))
                                  0.0)
            log-accept (+ (- new-score old-score)
                          (- (js/Math.log old-size)
                             (js/Math.log new-size))
                          proposal-correction)]
        (if (< (js/Math.log (erp/rand)) log-accept)
          new-result
          old-state)))))

(defn mh-query-fn
  "Trace-based single-site Metropolis-Hastings inference.
   Returns a list of num-samples samples with the given lag
   (number of intermediate steps to discard between kept samples).
   Optional burn parameter discards initial samples for convergence.
   Optional callback is called with {:iter i :value v :score s} for each kept sample."
  ([num-samples lag thunk] (mh-query-fn num-samples lag 0 nil thunk))
  ([num-samples lag burn thunk] (mh-query-fn num-samples lag burn nil thunk))
  ([num-samples lag burn callback thunk]
   ;; Get initial sample via traced rejection
   (let [initial (loop [attempts 0]
                   (if (>= attempts max-rejection-attempts)
                     (throw (ex-info "mh-query: no initial sample found"
                                     {:attempts max-rejection-attempts}))
                     (let [result (execute-traced thunk (make-fresh-state))]
                       (if result
                         result
                         (recur (inc attempts))))))
         ;; Advance state by n MH steps
         advance (fn [state n]
                   (loop [s state, i 0]
                     (if (>= i n) s
                       (recur (mh-step thunk s) (inc i)))))
         ;; Burn-in: discard initial samples
         burned (advance initial burn)]
     ;; Collect samples: each kept sample follows (1 + lag) MH steps
     (loop [i 0, state burned, samples (transient [])]
       (if (>= i num-samples)
         (seq (persistent! samples))
         (let [state' (advance state (inc lag))]
           (when callback
             (callback {:iter i :value (:value state') :score (:score state')}))
           (recur (inc i) state' (conj! samples (:value state')))))))))

(defn mh-query-scored-fn
  "Like mh-query-fn but returns a list of {:value v :score s} maps
   instead of bare values."
  ([num-samples lag thunk] (mh-query-scored-fn num-samples lag 0 nil thunk))
  ([num-samples lag burn thunk] (mh-query-scored-fn num-samples lag burn nil thunk))
  ([num-samples lag burn callback thunk]
   (let [initial (loop [attempts 0]
                   (if (>= attempts max-rejection-attempts)
                     (throw (ex-info "mh-query-scored: no initial sample found"
                                     {:attempts max-rejection-attempts}))
                     (let [result (execute-traced thunk (make-fresh-state))]
                       (if result
                         result
                         (recur (inc attempts))))))
         advance (fn [state n]
                   (loop [s state, i 0]
                     (if (>= i n) s
                       (recur (mh-step thunk s) (inc i)))))
         burned (advance initial burn)]
     (loop [i 0, state burned, samples (transient [])]
       (if (>= i num-samples)
         (seq (persistent! samples))
         (let [state' (advance state (inc lag))
               entry {:value (:value state') :score (:score state')}]
           (when callback
             (callback (assoc entry :iter i)))
           (recur (inc i) state' (conj! samples entry))))))))

(defn map-query-fn
  "MAP inference: return the single highest-scoring value from MH samples."
  ([num-samples lag thunk] (map-query-fn num-samples lag 0 thunk))
  ([num-samples lag burn thunk]
   (let [scored (mh-query-scored-fn num-samples lag burn thunk)]
     (:value (apply max-key :score scored)))))

;; ---------------------------------------------------------------------------
;; Importance Sampling
;; ---------------------------------------------------------------------------

(defn importance-query-fn
  "Importance sampling: run thunk n times, collect weighted samples,
   normalize and aggregate. Returns (values probs) like enumeration-query-fn."
  [n thunk]
  (let [raw (loop [i 0, acc (transient [])]
              (if (>= i n)
                (persistent! acc)
                (let [r (execute-traced thunk (make-fresh-state))]
                  (recur (inc i)
                         (if r
                           (conj! acc {:value (:value r) :score (:score r)})
                           acc)))))
        _ (when (empty? raw)
            (throw (ex-info "importance-query: all samples rejected" {:n n})))
        log-z (math/log-sum-exp (mapv :score raw))
        grouped (reduce (fn [m {:keys [value score]}]
                          (update m value
                                  (fnil #(math/log-sum-exp [% score]) ##-Inf)))
                        {} raw)
        values (keys grouped)
        probs (map #(js/Math.exp (- (get grouped %) log-z)) values)]
    (list (apply list values) (apply list probs))))

;; ---------------------------------------------------------------------------
;; Enumeration Query (exact, for finite-domain ERPs)
;; ---------------------------------------------------------------------------

(def ^:private max-enum-combos 1000000)

(defn- discover-enum-domains
  "Execute thunk once with enum-discovery mode to find all choice points.
   Returns vector of {:addr :domain :erp-id} entries."
  [thunk]
  (let [ts (volatile! {:counter 0
                       :old-trace {}
                       :proposal-addr -1
                       :new-trace {}
                       :score 0.0
                       :mem-cache {}
                       :enum-discovery true
                       :enum-domains []})]
    (binding [erp/*trace-state* ts]
      (try
        (thunk)
        (catch :default e
          (if (rejection? e) nil (throw e)))))
    (:enum-domains @ts)))

(defn- odometer-step
  "Advance an odometer (vector of indices) given domain sizes.
   Returns nil when all combinations exhausted."
  [indices sizes]
  (let [n (count indices)]
    (loop [i (dec n)
           indices indices]
      (if (neg? i)
        nil ;; overflow — all combos done
        (let [next-val (inc (nth indices i))]
          (if (< next-val (nth sizes i))
            (assoc indices i next-val)
            (recur (dec i) (assoc indices i 0))))))))

(defn- enum-normalize
  "Normalize a seq of {:value :log-prob} results into (values probs)."
  [results]
  (when (empty? results)
    (throw (ex-info "enumeration-query: all executions rejected" {})))
  (let [log-probs (mapv :log-prob results)
        log-z (math/log-sum-exp log-probs)
        grouped (reduce (fn [m {:keys [value log-prob]}]
                          (update m value (fnil #(math/log-sum-exp [% log-prob]) ##-Inf)))
                        {} results)
        values (keys grouped)
        probs (map #(js/Math.exp (- (get grouped %) log-z)) values)]
    (list (apply list values) (apply list probs))))

(defn- enum-execute-combo
  "Execute a single combination (index vector) via trace replay."
  [thunk domains indices]
  (let [old-trace (into {}
                    (map-indexed
                      (fn [i {:keys [addr domain erp-id]}]
                        (let [val (nth domain (nth indices i))]
                          [addr {:value val :erp-id erp-id :log-prob 0.0}]))
                      domains))
        ts (make-replay-state old-trace -1)
        result (execute-traced thunk ts)]
    (when result
      (let [choice-lps (map (fn [[_addr entry]] (:log-prob entry)) (:trace result))
            total-lp (+ (reduce + 0.0 choice-lps) (:score result))]
        {:value (:value result) :log-prob total-lp}))))

(defn- full-enum
  "Exhaustive odometer enumeration over all combinations."
  [thunk domains max-execs]
  (let [sizes (mapv #(count (:domain %)) domains)
        total-combos (reduce * 1 sizes)]
    (when (and (nil? max-execs) (> total-combos max-enum-combos))
      (throw (ex-info "enumeration-query: too many combinations"
                      {:combos total-combos :limit max-enum-combos})))
    (loop [indices (vec (repeat (count domains) 0))
           acc (transient [])
           executed 0]
      (if (or (nil? indices)
              (and max-execs (>= executed max-execs)))
        (persistent! acc)
        (let [result (enum-execute-combo thunk domains indices)]
          (recur (odometer-step indices sizes)
                 (if result (conj! acc result) acc)
                 (inc executed)))))))

(defn- pq-pop-max
  "Pop the highest log-prob item from a priority queue (vector)."
  [pq]
  (let [n (count pq)
        idx (loop [i 1, best 0]
              (if (>= i n)
                best
                (recur (inc i)
                       (if (> (:log-prob (nth pq i)) (:log-prob (nth pq best)))
                         i best))))
        item (nth pq idx)]
    [item (into (subvec pq 0 idx) (subvec pq (inc idx)))]))

(defn- likely-first-enum
  "Priority-queue enumeration: explore high-probability combinations first.
   Stops after max-execs complete traces."
  [thunk domains max-execs]
  (let [n-domains (count domains)
        max-execs (or max-execs 10000)]
    (loop [pq [{:log-prob 0.0 :indices []}]
           results (transient [])
           executed 0]
      (if (or (empty? pq) (>= executed max-execs))
        (persistent! results)
        (let [[best rest-pq] (pq-pop-max pq)
              depth (count (:indices best))]
          (if (= depth n-domains)
            ;; Complete assignment — execute trace
            (let [result (enum-execute-combo thunk domains (:indices best))]
              (recur rest-pq
                     (if result (conj! results result) results)
                     (inc executed)))
            ;; Partial — expand children for next domain
            (let [{:keys [domain log-probs]} (nth domains depth)
                  children (mapv (fn [idx]
                                  {:log-prob (+ (:log-prob best)
                                                (if log-probs (nth log-probs idx) 0.0))
                                   :indices (conj (:indices best) idx)})
                                (range (count domain)))]
              (recur (into rest-pq children)
                     results
                     executed))))))))

(defn enumeration-query-fn
  "Exact enumeration over all combinations of discrete choices.
   Returns (values probs).
   With opts map: supports :strategy (:full, :likely-first) and :max-executions."
  ([thunk] (enumeration-query-fn {} thunk))
  ([opts thunk]
   (let [{:keys [strategy max-executions]
          :or {strategy :full}} opts
         domains (discover-enum-domains thunk)
         _ (when (some #(nil? (:domain %)) domains)
             (throw (ex-info "enumeration-query: continuous (non-enumerable) ERP encountered"
                             {:erp (first (filter #(nil? (:domain %)) domains))})))]
     (if (zero? (count domains))
       ;; No random choices: just run the thunk
       (let [result (try {:value (thunk)} (catch :default e (if (rejection? e) nil (throw e))))]
         (if result
           (list (list (:value result)) (list 1.0))
           (list (list) (list))))
       ;; Enumerate
       (let [results (case strategy
                       :likely-first (likely-first-enum thunk domains max-executions)
                       :full (full-enum thunk domains max-executions)
                       (throw (ex-info (str "enumeration-query: unknown strategy " strategy)
                                       {:strategy strategy})))]
         (enum-normalize results))))))

;; ---------------------------------------------------------------------------
;; Soft conditioning via enumeration
;; ---------------------------------------------------------------------------

(defn condition-equal
  "Soft conditioning: enumerate the thunk, find P(return=value),
   and add log(P) as a factor score."
  [thunk value]
  (let [[values probs] (enumeration-query-fn thunk)
        prob-map (zipmap (seq values) (seq probs))
        p (get prob-map value 0.0)]
    (if (zero? p)
      (factor ##-Inf)
      (factor (js/Math.log p)))))

;; ---------------------------------------------------------------------------
;; Conditional (creates a reusable sampler)
;; ---------------------------------------------------------------------------

(defn conditional-fn
  "Create a conditional sampler from parameters and a thunk.
   params is a list like ('enumeration'), ('rejection'), or ('mh' lag).
   Returns a zero-argument function that samples from the conditional."
  [params thunk]
  (let [params-vec (vec params)
        strategy (when (seq params-vec) (name (first params-vec)))]
    (case strategy
      "enumerate"
      (fn []
        (let [[values probs] (enumeration-query-fn thunk)
              vs (vec values)
              ps (vec probs)]
          (erp/multinomial vs ps)))

      "rejection"
      (fn []
        (rejection-query-fn thunk))

      "mh"
      (let [lag (if (>= (count params-vec) 2)
                  (second params-vec)
                  1)]
        (fn []
          (first (mh-query-fn 1 lag thunk))))

      "importance"
      (let [n (if (>= (count params-vec) 2)
                (second params-vec)
                1000)]
        (fn []
          (let [[values probs] (importance-query-fn n thunk)
                vs (vec values)
                ps (vec probs)]
            (erp/multinomial vs ps))))

      ;; Default: rejection
      (fn []
        (rejection-query-fn thunk)))))

;; ---------------------------------------------------------------------------
;; Sequential Monte Carlo (SMC / Particle Filtering)
;; ---------------------------------------------------------------------------

(defn- run-cps-particle
  "Run a CPS-transformed model until it yields a checkpoint.
   cps-fn: (fn [k state] -> checkpoint-or-thunk)
   Returns a checkpoint record (Sample, Observe, Factor, or Result)."
  [cps-fn state]
  (loop [v (cps-fn (fn [val s] (cps/->Result val s)) state)]
    (if (fn? v) (recur (v)) v)))

(defn- multinomial-resample
  "Multinomial resampling: draw n particles with replacement,
   probability proportional to weights. Returns new particle vector."
  [particles log-weights n]
  (let [log-z (math/log-sum-exp log-weights)
        probs (mapv #(js/Math.exp (- % log-z)) log-weights)
        ;; Cumulative probabilities for sampling
        cum (loop [i 0, acc 0.0, cs (transient [])]
              (if (>= i (count probs))
                (persistent! cs)
                (let [acc (+ acc (nth probs i))]
                  (recur (inc i) acc (conj! cs acc)))))]
    (vec
      (repeatedly n
        (fn []
          (let [u (erp/rand)]
            (loop [i 0]
              (if (or (>= i (dec (count cum)))
                      (< u (nth cum i)))
                (nth particles i)
                (recur (inc i))))))))))

(defn- effective-sample-size
  "Compute ESS from log-weights. ESS = (sum w)^2 / sum(w^2)."
  [log-weights]
  (let [log-z (math/log-sum-exp log-weights)
        log-w-norm (mapv #(- % log-z) log-weights)
        ;; ESS = 1 / sum(w_i^2) where w_i are normalized
        log-sum-sq (math/log-sum-exp (mapv #(* 2 %) log-w-norm))]
    (js/Math.exp (- log-sum-sq))))

(defn smc-query-fn
  "Sequential Monte Carlo (particle filter) inference.
   n-particles: number of particles.
   cps-model: CPS-transformed model function (fn [k state] -> checkpoint).

   Algorithm:
   1. Initialize N particles, run each to first checkpoint
   2. At Sample checkpoints: draw from distribution, continue
   3. At Observe checkpoints: accumulate log-weights, resample, continue
   4. At Factor checkpoints: accumulate score, resample if needed
   5. Collect values from Result records

   Returns a vector of samples from the posterior."
  ([n-particles cps-model]
   (smc-query-fn n-particles {} cps-model))
  ([n-particles opts cps-model]
   (let [{:keys [resample-threshold]
          :or {resample-threshold 0.5}} opts
         init-state {:addr 0 :log-weight 0.0}
         ;; Initialize all particles
         particles (vec (repeatedly n-particles
                    #(run-cps-particle cps-model init-state)))
         ;; Process particles until all are Result
         process (fn process [particles]
                   (cond
                     ;; All done
                     (every? cps/result? particles)
                     particles

                     ;; All at Sample checkpoint
                     (every? cps/sample? particles)
                     (let [advanced (mapv (fn [p]
                                           (let [v (dist/sample* (:dist p))
                                                 cont (:cont p)
                                                 state (:state p)]
                                             (run-cps-particle
                                               (fn [_ _] (cont v state))
                                               state)))
                                         particles)]
                       (recur advanced))

                     ;; All at Observe checkpoint
                     (every? cps/observe? particles)
                     (let [;; Compute observation log-weights
                           log-weights (mapv (fn [p]
                                              (+ (get-in p [:state :log-weight] 0.0)
                                                 (dist/observe* (:dist p) (:value p))))
                                            particles)
                           ;; Resample if ESS is low
                           ess (effective-sample-size log-weights)
                           should-resample (< ess (* resample-threshold n-particles))
                           resampled (if should-resample
                                       (mapv (fn [p]
                                               (assoc-in p [:state :log-weight] 0.0))
                                             (multinomial-resample
                                               particles log-weights n-particles))
                                       (mapv (fn [p lw]
                                               (assoc-in p [:state :log-weight] lw))
                                             particles log-weights))
                           ;; Continue all particles past the observe
                           advanced (mapv (fn [p]
                                           (let [cont (:cont p)
                                                 state (:state p)]
                                             (run-cps-particle
                                               (fn [_ _] (cont nil state))
                                               state)))
                                         resampled)]
                       (recur advanced))

                     ;; All at Factor checkpoint
                     (every? cps/factor? particles)
                     (let [;; Accumulate factor scores into weights
                           updated (mapv (fn [p]
                                          (let [state (update (:state p) :log-weight
                                                        (fnil + 0.0) (:score p))
                                                cont (:cont p)]
                                            (assoc p :state state :cont cont)))
                                        particles)
                           ;; Check for hard rejections (-Inf weight)
                           log-weights (mapv #(get-in % [:state :log-weight] 0.0) updated)
                           ess (effective-sample-size log-weights)
                           should-resample (< ess (* resample-threshold n-particles))
                           resampled (if should-resample
                                       (mapv (fn [p]
                                               (assoc-in p [:state :log-weight] 0.0))
                                             (multinomial-resample
                                               updated log-weights n-particles))
                                       updated)
                           ;; Continue
                           advanced (mapv (fn [p]
                                           (let [cont (:cont p)
                                                 state (:state p)]
                                             (run-cps-particle
                                               (fn [_ _] (cont nil state))
                                               state)))
                                         resampled)]
                       (recur advanced))

                     ;; Mixed checkpoint types — process individually
                     :else
                     (let [advanced (mapv (fn [p]
                                           (cond
                                             (cps/result? p) p

                                             (cps/sample? p)
                                             (let [v (dist/sample* (:dist p))
                                                   cont (:cont p)
                                                   state (:state p)]
                                               (run-cps-particle
                                                 (fn [_ _] (cont v state))
                                                 state))

                                             (cps/observe? p)
                                             (let [lw (dist/observe* (:dist p) (:value p))
                                                   state (update (:state p) :log-weight
                                                           (fnil + 0.0) lw)
                                                   cont (:cont p)]
                                               (run-cps-particle
                                                 (fn [_ _] (cont nil state))
                                                 state))

                                             (cps/factor? p)
                                             (let [state (update (:state p) :log-weight
                                                           (fnil + 0.0) (:score p))
                                                   cont (:cont p)]
                                               (run-cps-particle
                                                 (fn [_ _] (cont nil state))
                                                 state))

                                             :else
                                             (throw (ex-info "smc: unexpected checkpoint type"
                                                             {:checkpoint p}))))
                                         particles)]
                       (recur advanced))))]
     ;; Final weighted resample: particles carry accumulated log-weights
     (let [final-particles (process particles)
           log-weights (mapv #(get-in % [:state :log-weight] 0.0) final-particles)
           all-neg-inf (every? #(= % ##-Inf) log-weights)]
       (if all-neg-inf
         (throw (ex-info "smc: all particles have zero weight" {:n-particles n-particles}))
         ;; Resample to produce unweighted samples
         (let [resampled (multinomial-resample final-particles log-weights n-particles)]
           (mapv :value resampled)))))))

;; ---------------------------------------------------------------------------
;; Unified inference entry point
;; ---------------------------------------------------------------------------

(defn infer
  "Unified inference entry point. Dispatches to all inference methods.
   opts is a map with :method and method-specific options.
   thunk is a zero-arg model function.

   Methods:
     :rejection    - rejection sampling, returns single value
     :mh           - Metropolis-Hastings, returns list of samples
                     opts: :samples (required), :lag (default 1), :burn (default 0), :callback fn
     :enumeration  - exact enumeration, returns (values probs)
                     opts: :strategy (:full, :likely-first), :max-executions
     :importance   - importance sampling, returns (values probs)
                     opts: :samples (default 1000)
     :forward      - forward sampling (ignores factors/conditions), returns list of samples
                     opts: :samples (required)
     :mh-scored    - MH with scores, returns list of {:value :score} maps
                     opts: :samples (required), :lag (default 1), :burn (default 0), :callback fn
     :map          - MAP inference via MH, returns single highest-scoring value
                     opts: :samples (default 1000), :lag (default 1), :burn (default 0)
     :smc          - Sequential Monte Carlo, returns vector of samples
                     opts: :particles (required), thunk must be CPS-transformed (use smc-query macro)"
  [opts thunk]
  (let [{:keys [method samples lag burn callback strategy max-executions]
         :or {lag 1 burn 0}} opts]
    (case method
      :rejection   (rejection-query-fn thunk)
      :mh          (mh-query-fn samples lag burn callback thunk)
      :enumeration (enumeration-query-fn
                     (cond-> {}
                       strategy       (assoc :strategy strategy)
                       max-executions (assoc :max-executions max-executions))
                     thunk)
      :importance  (importance-query-fn (or samples 1000) thunk)
      :forward     (forward-query-fn samples thunk)
      :mh-scored   (mh-query-scored-fn samples lag burn callback thunk)
      :map         (map-query-fn (or samples 1000) lag burn thunk)
      :smc         (smc-query-fn (or (:particles opts) samples) thunk)
      (throw (ex-info (str "infer: unknown method " method) {:method method})))))
