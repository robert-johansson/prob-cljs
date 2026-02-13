(ns prob.agents.rft
  "Relational Frame Theory (RFT) agent for Bayesian cognitive modeling.
   Models stimulus equivalence and relational responding via Bayesian
   inference over structured hypothesis spaces.

   Key concepts:
   - A hypothesis assigns a relation type (:same, :different, :more-than, :less-than)
     to each ordered stimulus pair
   - Combinatorial entailment: if A-B is :same and B-C is :same, then A-C must be :same
   - The agent maintains a categorical-dist belief over consistent hypotheses
   - Actions (comparison selections) are chosen via softmax over relational consistency
   - Belief updates are exact Bayesian conditioning on feedback"
  (:require [prob.dist :as dist]
            [prob.core :as core]))

;; ---------------------------------------------------------------------------
;; Relation algebra
;; ---------------------------------------------------------------------------

(def ^:private relation-composition
  "Transitive composition table for relational frames.
   (compose r1 r2) gives the derived relation: if A-B is r1 and B-C is r2, A-C is..."
  {[:same :same]         :same
   [:same :different]    :different
   [:same :more-than]    :more-than
   [:same :less-than]    :less-than
   [:different :same]    :different
   [:different :different] :same
   [:different :more-than] :less-than
   [:different :less-than] :more-than
   [:more-than :same]    :more-than
   [:more-than :different] :less-than
   [:more-than :more-than] :more-than
   [:more-than :less-than] nil          ;; ambiguous
   [:less-than :same]    :less-than
   [:less-than :different] :more-than
   [:less-than :less-than] :less-than
   [:less-than :more-than] nil})        ;; ambiguous

(def ^:private relation-inverse
  "Inverse (mutual entailment): if A-B is r, then B-A is..."
  {:same      :same
   :different :different
   :more-than :less-than
   :less-than :more-than})

(defn- compose-relations
  "Compose two relations via transitivity. Returns nil if ambiguous."
  [r1 r2]
  (get relation-composition [r1 r2]))

;; ---------------------------------------------------------------------------
;; Hypothesis space generation
;; ---------------------------------------------------------------------------

(defn- ordered-pairs
  "All ordered pairs [a b] where a != b from a set of stimuli."
  [stimuli]
  (for [a stimuli
        b stimuli
        :when (not= a b)]
    [a b]))

(defn- hypothesis-consistent?
  "Check if a hypothesis (map of [a b] -> relation) is internally consistent.
   Checks mutual entailment and combinatorial entailment (transitivity)."
  [hyp stimuli]
  (let [stim-vec (vec stimuli)]
    (and
      ;; Mutual entailment: hyp[a,b] must be inverse of hyp[b,a]
      (every? (fn [[a b]]
                (let [r-ab (get hyp [a b])
                      r-ba (get hyp [b a])]
                  (= r-ba (relation-inverse r-ab))))
              (ordered-pairs stim-vec))
      ;; Combinatorial entailment: for all a,b,c the transitive closure holds
      (every? (fn [a]
                (every? (fn [b]
                          (every? (fn [c]
                                    (if (or (= a b) (= b c) (= a c))
                                      true
                                      (let [r-ab (get hyp [a b])
                                            r-bc (get hyp [b c])
                                            r-ac (get hyp [a c])
                                            derived (compose-relations r-ab r-bc)]
                                        ;; nil derived means ambiguous — any value ok
                                        (or (nil? derived) (= r-ac derived)))))
                                  stim-vec))
                        stim-vec))
              stim-vec))))

(defn make-rft-hypothesis-space
  "Generate all internally consistent relational hypotheses for a set of stimuli.
   stimuli: vector of stimulus labels, e.g. [:A :B :C]
   relation-types: vector of possible relations, e.g. [:same :different]

   Returns a vector of hypothesis maps, each mapping [stim-a stim-b] -> relation.
   Only includes hypotheses satisfying mutual entailment and combinatorial entailment."
  [stimuli relation-types]
  (let [pairs (ordered-pairs stimuli)
        ;; Generate directed pairs (a < b in index) to avoid redundancy
        directed (filter (fn [[a b]]
                           (< (.indexOf stimuli a)
                              (.indexOf stimuli b)))
                         pairs)
        ;; For each directed pair, assign a relation; derive inverse automatically
        assign-count (count directed)]
    (if (zero? assign-count)
      [{}]
      (let [;; Generate all possible assignments for directed pairs
            all-assignments
            (reduce (fn [acc [a b]]
                      (mapcat (fn [partial]
                                (map (fn [r]
                                       (assoc partial
                                              [a b] r
                                              [b a] (relation-inverse r)))
                                     relation-types))
                              acc))
                    [{}]
                    directed)]
        ;; Filter to only consistent hypotheses
        (vec (filter #(hypothesis-consistent? % stimuli) all-assignments))))))

;; ---------------------------------------------------------------------------
;; Relation derivation
;; ---------------------------------------------------------------------------

(defn derive-relation
  "Given a hypothesis and two stimuli, return their relation.
   Looks up directly first, then derives via transitive closure through
   intermediate stimuli."
  [hypothesis stim-a stim-b]
  (if (= stim-a stim-b)
    :same
    (or (get hypothesis [stim-a stim-b])
        ;; Try transitive derivation through any intermediate stimulus
        (let [all-stims (set (mapcat identity (keys hypothesis)))]
          (some (fn [mid]
                  (when (and (not= mid stim-a) (not= mid stim-b))
                    (let [r1 (get hypothesis [stim-a mid])
                          r2 (get hypothesis [mid stim-b])]
                      (when (and r1 r2)
                        (compose-relations r1 r2)))))
                all-stims)))))

;; ---------------------------------------------------------------------------
;; Prediction
;; ---------------------------------------------------------------------------

(defn predict-response
  "Given current belief (categorical-dist over hypotheses), a sample stimulus,
   comparison stimuli, and a contextual cue, return [comparisons probs].
   The probability of each comparison is the marginal probability (across hypotheses)
   that it is the correct match under the cue's relational frame.

   cue: the relation to match, e.g. :same or :different
   Returns [comparisons probs] where probs sum to 1."
  [belief sample-stim comparisons cue]
  (let [hypotheses (dist/enumerate* belief)
        ;; For each comparison, compute P(comparison is correct | belief, cue)
        comp-scores
        (mapv (fn [comp]
                (reduce + 0.0
                  (map (fn [hyp]
                         (let [hyp-prob (js/Math.exp (dist/observe* belief hyp))
                               relation (derive-relation hyp sample-stim comp)]
                           (if (= relation cue)
                             hyp-prob
                             0.0)))
                       hypotheses)))
              comparisons)
        total (reduce + 0.0 comp-scores)
        probs (if (zero? total)
                (vec (repeat (count comparisons) (/ 1.0 (count comparisons))))
                (mapv #(/ % total) comp-scores))]
    [comparisons probs]))

;; ---------------------------------------------------------------------------
;; Agent
;; ---------------------------------------------------------------------------

(defn make-rft-agent
  "Create a Bayesian RFT agent for relational learning.

   params:
     :hypothesis-space - vector of hypothesis maps (from make-rft-hypothesis-space)
     :prior            - optional categorical-dist over hypotheses (default: uniform)
     :alpha            - softmax temperature (default: 5.0; higher = more deterministic)

   Returns {:act fn, :update-belief fn, :predict fn, :belief atom, :params map}
     :act           - (fn [sample-stim comparisons cue]) -> [comparisons probs]
                      Select comparison via softmax over relational consistency
     :update-belief - (fn [sample-stim chosen-comp cue correct?]) -> new-belief
                      Bayesian update on feedback. Modifies internal belief atom.
     :predict       - (fn [sample-stim comparisons cue]) -> [comparisons probs]
                      Predict without acting (no softmax, just marginal probabilities)
     :belief        - atom holding current categorical-dist"
  [params]
  (let [hyp-space (:hypothesis-space params)
        n (count hyp-space)
        alpha (or (:alpha params) 5.0)
        prior (or (:prior params)
                  (dist/categorical-dist hyp-space
                                         (vec (repeat n (/ 1.0 n)))))
        belief-atom (atom prior)

        predict-fn
        (fn [sample-stim comparisons cue]
          (predict-response @belief-atom sample-stim comparisons cue))

        act-fn
        (fn [sample-stim comparisons cue]
          (let [[comps probs] (predict-response @belief-atom sample-stim comparisons cue)
                ;; Apply softmax with temperature alpha
                log-probs (mapv #(* alpha (js/Math.log (max % 1e-10))) probs)
                max-lp (apply max log-probs)
                exp-scores (mapv #(js/Math.exp (- % max-lp)) log-probs)
                total (reduce + 0.0 exp-scores)
                softmax-probs (mapv #(/ % total) exp-scores)]
            [comps softmax-probs]))

        update-belief-fn
        (fn [sample-stim chosen-comp cue correct?]
          (let [current-belief @belief-atom
                support (dist/enumerate* current-belief)
                weighted
                (keep
                  (fn [hyp]
                    (let [prior-prob (js/Math.exp (dist/observe* current-belief hyp))
                          relation (derive-relation hyp sample-stim chosen-comp)
                          ;; Likelihood: how probable is this feedback given this hypothesis?
                          ;; If hypothesis says relation matches cue, correct feedback is likely
                          matches-cue? (= relation cue)
                          likelihood (cond
                                       ;; Correct feedback and hypothesis agrees: high likelihood
                                       (and correct? matches-cue?) 0.95
                                       ;; Correct feedback but hypothesis disagrees: low likelihood
                                       (and correct? (not matches-cue?)) 0.05
                                       ;; Incorrect feedback and hypothesis agrees: low likelihood
                                       (and (not correct?) matches-cue?) 0.05
                                       ;; Incorrect feedback and hypothesis disagrees: high likelihood
                                       :else 0.95)
                          posterior-unnorm (* prior-prob likelihood)]
                      (when (pos? posterior-unnorm)
                        [hyp posterior-unnorm])))
                  support)
                total (reduce + 0.0 (map second weighted))]
            (if (or (empty? weighted) (zero? total))
              @belief-atom
              (let [states (mapv first weighted)
                    probs (mapv #(/ (second %) total) weighted)
                    new-belief (dist/categorical-dist states probs)]
                (reset! belief-atom new-belief)
                new-belief))))]

    {:act act-fn
     :update-belief update-belief-fn
     :predict predict-fn
     :belief belief-atom
     :params params}))

;; ---------------------------------------------------------------------------
;; Simulation helpers
;; ---------------------------------------------------------------------------

(defn simulate-mts-trial
  "Run one Matching-to-Sample trial.
   agent: from make-rft-agent
   trial: {:sample stim, :comparisons [stim ...], :cue :same/:different, :correct-idx int}

   Returns {:response idx, :correct? bool, :belief-summary map}
   Side effect: updates agent belief if feedback is provided (i.e. :correct-idx present)."
  [agent trial]
  (let [{:keys [sample comparisons cue correct-idx]} trial
        act-fn (:act agent)
        update-fn (:update-belief agent)
        [comps probs] (act-fn sample comparisons cue)
        ;; Sample response from softmax distribution
        response-idx (let [r (core/rand)]
                       (loop [i 0 cum 0.0]
                         (if (>= i (count probs))
                           (dec (count probs))
                           (let [cum (+ cum (nth probs i))]
                             (if (< r cum) i (recur (inc i) cum))))))
        chosen-comp (nth comparisons response-idx)
        correct? (if (some? correct-idx)
                   (= response-idx correct-idx)
                   nil)
        ;; Update belief if we have feedback
        _ (when (some? correct?)
            (update-fn sample chosen-comp cue correct?))
        ;; Summarize current belief
        belief @(:belief agent)
        support (dist/enumerate* belief)]
    {:response response-idx
     :chosen chosen-comp
     :correct? correct?
     :response-probs probs
     :belief-summary
     (into {}
           (map (fn [hyp]
                  [(pr-str hyp)
                   (js/Math.exp (dist/observe* belief hyp))])
                support))}))

(defn simulate-mts-block
  "Run a block of MTS trials. Returns vector of trial results.
   agent: from make-rft-agent
   trials: vector of trial maps (see simulate-mts-trial)"
  [agent trials]
  (mapv #(simulate-mts-trial agent %) trials))

(defn get-belief-summary
  "Get a human-readable summary of the agent's current belief state.
   Returns a map of hypothesis-string -> probability."
  [agent]
  (let [belief @(:belief agent)
        support (dist/enumerate* belief)]
    (into {}
          (map (fn [hyp]
                 [(pr-str hyp)
                  (js/Math.exp (dist/observe* belief hyp))])
               support))))

(defn get-relation-beliefs
  "Get marginal probability for each stimulus pair's relation type.
   Returns {[stim-a stim-b] {:same p1 :different p2 ...}}"
  [agent stimuli]
  (let [belief @(:belief agent)
        support (dist/enumerate* belief)
        pairs (ordered-pairs stimuli)]
    (into {}
          (map (fn [[a b]]
                 [[a b]
                  (reduce (fn [acc hyp]
                            (let [prob (js/Math.exp (dist/observe* belief hyp))
                                  rel (derive-relation hyp a b)]
                              (update acc rel (fnil + 0.0) prob)))
                          {}
                          support)])
               pairs))))

(defn response-entropy
  "Compute the Shannon entropy of the agent's action distribution for a trial.
   Higher entropy = more uncertainty = predicted slower RT."
  [agent sample-stim comparisons cue]
  (let [[_ probs] (predict-response @(:belief agent) sample-stim comparisons cue)]
    (- (reduce + 0.0
         (map (fn [p] (if (zero? p) 0.0 (* p (js/Math.log p)))) probs)))))

(defn predict-rt
  "Predict response time based on decision entropy.
   base-time: minimum RT in ms (e.g. 300)
   conflict-cost: ms per nat of entropy (e.g. 500)
   Returns predicted RT in ms."
  [agent sample-stim comparisons cue base-time conflict-cost]
  (+ base-time (* conflict-cost (response-entropy agent sample-stim comparisons cue))))
