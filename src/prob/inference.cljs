(ns prob.inference
  "Inference algorithms: rejection sampling, trace-based single-site MH,
   and approximate enumeration.

   MH uses persistent hash-maps as execution traces. Each trace maps
   sequential integer addresses to {:value :log-prob :erp-id} entries.
   Proposals change one address at a time; structural sharing makes
   this O(log32 n) per step."
  (:require [prob.erp :as erp]
            [prob.dist :as dist]
            [prob.math :as math]))

;; ---------------------------------------------------------------------------
;; Condition / Factor support
;; ---------------------------------------------------------------------------

(def ^:private rejection-sentinel ::rejection)

(defn condition [pred]
  (when-not pred
    (throw (ex-info "condition failed" {::type rejection-sentinel})))
  nil)

(defn factor [log-weight]
  (if erp/*trace-state*
    ;; MH mode: accumulate score deterministically
    (vswap! erp/*trace-state* update :score + log-weight)
    ;; Rejection mode: probabilistic rejection
    (when (neg? log-weight)
      (when (> (js/Math.log (js/Math.random)) log-weight)
        (throw (ex-info "factor rejected" {::type rejection-sentinel})))))
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
        addr (js/Math.floor (* (js/Math.random) old-size))
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
        (if (< (js/Math.log (js/Math.random)) log-accept)
          new-result
          old-state)))))

(defn mh-query-fn
  "Trace-based single-site Metropolis-Hastings inference.
   Returns a list of num-samples samples with the given lag
   (number of intermediate steps to discard between kept samples)."
  [num-samples lag thunk]
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
                      (recur (mh-step thunk s) (inc i)))))]
    ;; Collect samples: each kept sample follows (1 + lag) MH steps
    (loop [i 0, state initial, samples (transient [])]
      (if (>= i num-samples)
        (seq (persistent! samples))
        (let [state' (advance state (inc lag))]
          (recur (inc i) state' (conj! samples (:value state'))))))))

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

(defn enumeration-query-fn
  "Exact enumeration over all combinations of discrete choices.
   Returns (values probs)."
  [thunk]
  (let [domains (discover-enum-domains thunk)
        _ (when (some #(nil? (:domain %)) domains)
            (throw (ex-info "enumeration-query: continuous (non-enumerable) ERP encountered"
                            {:erp (first (filter #(nil? (:domain %)) domains))})))
        sizes (mapv #(count (:domain %)) domains)
        total-combos (reduce * 1 sizes)]
    (when (> total-combos max-enum-combos)
      (throw (ex-info "enumeration-query: too many combinations"
                      {:combos total-combos :limit max-enum-combos})))
    (if (zero? (count domains))
      ;; No random choices: just run the thunk
      (let [result (try {:value (thunk)} (catch :default e (if (rejection? e) nil (throw e))))]
        (if result
          (list (list (:value result)) (list 1.0))
          (list (list) (list))))
      ;; Iterate over all combinations
      (let [results
            (loop [indices (vec (repeat (count domains) 0))
                   acc (transient [])]
              (if (nil? indices)
                (persistent! acc)
                ;; Build old-trace for this combination
                (let [old-trace (into {}
                                  (map-indexed
                                    (fn [i {:keys [addr domain erp-id]}]
                                      (let [val (nth domain (nth indices i))
                                            lp 0.0] ;; placeholder, will be recomputed
                                        [addr {:value val :erp-id erp-id :log-prob lp}]))
                                    domains))
                      ts (make-replay-state old-trace -1)
                      result (execute-traced thunk ts)]
                  (recur (odometer-step indices sizes)
                         (if result
                           (let [;; Sum log-probs of all choices + score
                                 choice-lps (map (fn [[_addr entry]] (:log-prob entry))
                                                 (:trace result))
                                 total-lp (+ (reduce + 0.0 choice-lps)
                                             (:score result))]
                             (conj! acc {:value (:value result) :log-prob total-lp}))
                           acc)))))
            ;; Normalize
            _ (when (empty? results)
                (throw (ex-info "enumeration-query: all executions rejected" {})))
            log-probs (mapv :log-prob results)
            log-z (math/log-sum-exp log-probs)
            ;; Aggregate duplicate return values
            grouped (reduce (fn [m {:keys [value log-prob]}]
                              (update m value (fnil #(math/log-sum-exp % log-prob) ##-Inf)))
                            {} results)
            values (keys grouped)
            probs (map #(js/Math.exp (- (get grouped %) log-z)) values)]
        (list (apply list values) (apply list probs))))))

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

      ;; Default: rejection
      (fn []
        (rejection-query-fn thunk)))))
