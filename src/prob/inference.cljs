(ns prob.inference
  "Inference algorithms: rejection sampling, enumeration, and MH."
  (:require [prob.erp :as erp]))

;; ---------------------------------------------------------------------------
;; Condition / Factor support
;; ---------------------------------------------------------------------------

;; We use a special exception type for rejection.
;; In ClojureScript we throw an ExceptionInfo with a sentinel key.

(def ^:private rejection-sentinel ::rejection)

(defn condition [pred]
  (when-not pred
    (throw (ex-info "condition failed" {::type rejection-sentinel})))
  nil)

(defn factor [log-weight]
  ;; For rejection sampling, factor with non-zero weight:
  ;; accept with probability exp(log-weight) (relative to max)
  ;; For simplicity, treat factor(0) as always accept, negative as probabilistic rejection
  (when (neg? log-weight)
    (when (> (js/Math.log (js/Math.random)) log-weight)
      (throw (ex-info "factor rejected" {::type rejection-sentinel}))))
  nil)

(defn- rejection? [e]
  (and (instance? ExceptionInfo e)
       (= (::type (ex-data e)) rejection-sentinel)))

;; ---------------------------------------------------------------------------
;; Rejection Sampling
;; ---------------------------------------------------------------------------

(def ^:private max-rejection-attempts 10000)

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
;; MH Query (simple repeated rejection for now)
;; ---------------------------------------------------------------------------

(defn mh-query-fn
  "Simple MH query implementation using repeated rejection sampling.
   Returns a list of `num-samples` samples with the given `lag`
   (lag is currently treated as number of rejection attempts between samples)."
  [num-samples lag thunk]
  (let [sample-one (fn []
                     (loop [attempts 0]
                       (if (>= attempts max-rejection-attempts)
                         (throw (ex-info "mh-query: exceeded maximum attempts" {:attempts max-rejection-attempts}))
                         (let [result (try
                                        {:value (thunk)}
                                        (catch :default e
                                          (if (rejection? e)
                                            ::rejected
                                            (throw e))))]
                           (if (= result ::rejected)
                             (recur (inc attempts))
                             (:value result))))))]
    ;; Generate num-samples, each with `lag` intermediate rejections
    (loop [i 0, samples (transient [])]
      (if (>= i num-samples)
        (seq (persistent! samples))
        (let [;; Run `lag` samples and discard them
              _ (dotimes [_ lag] (sample-one))
              ;; Keep this sample
              s (sample-one)]
          (recur (inc i) (conj! samples s)))))))

;; ---------------------------------------------------------------------------
;; Enumeration Query (for finite-domain ERPs)
;; ---------------------------------------------------------------------------

(defn enumeration-query-fn
  "Approximate enumeration: runs many samples and aggregates.
   Returns (values probs)."
  [thunk]
  ;; Simple approximation: run many samples and aggregate
  (let [n 1000
        samples (loop [i 0, results (transient [])]
                  (if (>= i n)
                    (persistent! results)
                    (let [result (try
                                  {:value (thunk)}
                                  (catch :default e
                                    (if (rejection? e)
                                      ::rejected
                                      (throw e))))]
                      (if (= result ::rejected)
                        (recur (inc i) results)
                        (recur (inc i) (conj! results (:value result)))))))
        freqs (frequencies samples)
        total (count samples)
        values (keys freqs)
        probs (map #(/ (get freqs %) total) values)]
    (list (apply list values) (apply list probs))))

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
        ;; Sample from the enumerated distribution
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
