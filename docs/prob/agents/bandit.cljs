(ns prob.agents.bandit
  (:require [prob.dist :as dist]))

(defn make-bandit-pomdp
  "Create a stochastic multi-armed bandit as a POMDP.
   options:
     :num-arms        - number of arms (default 2)
     :arm-weights     - map from arm index to Bernoulli success probability,
                        e.g. {0 0.9 1 0.1}
     :num-trials      - number of pulls allowed
     :prize-to-utility - map from prize to utility
                         (default {\"chocolate\" 5 \"nothing\" 0})

   Each arm gives \"chocolate\" with probability arm-weights[arm],
   and \"nothing\" with probability (1 - arm-weights[arm]).

   Returns {:world world, :start-state start-state, :utility fn}"
  [options]
  (let [num-arms (or (:num-arms options) 2)
        arm-weights (:arm-weights options)
        num-trials (:num-trials options)
        prize-to-utility (or (:prize-to-utility options) {"chocolate" 5 "nothing" 0})
        actions (vec (range num-arms))

        enumerate-observations-fn
        (fn [state action]
          (let [w (get (:arm-weights state) action 0.5)]
            [["chocolate" "nothing"] [w (- 1.0 w)]]))

        transition-fn
        (fn [state action observation]
          (let [new-time (dec (:time-left state))]
            {:arm-weights (:arm-weights state)
             :prize observation
             :time-left new-time
             :terminate-after-action (<= new-time 0)}))

        observe-fn
        (fn [state]
          (:prize state))

        actions-fn
        (fn [state]
          (if (:terminate-after-action state)
            []
            actions))

        utility-fn
        (fn [state action]
          (get prize-to-utility (:prize state) 0))

        world {:transition-fn transition-fn
               :enumerate-observations enumerate-observations-fn
               :observe observe-fn
               :state-to-actions actions-fn}

        start-state {:arm-weights arm-weights
                     :prize "start"
                     :time-left num-trials
                     :terminate-after-action false}]

    {:world world
     :start-state start-state
     :utility utility-fn}))

(defn make-bandit-prior-belief
  "Create a categorical-dist prior belief over possible bandit configurations.
   possible-arm-weights: vector of possible arm-weight maps,
     e.g. [{0 0.9 1 0.1} {0 0.1 1 0.9}]
   prior-probs: vector of prior probabilities (or nil for uniform)
   start-state: the bandit start state (template)"
  [possible-arm-weights prior-probs start-state]
  (let [n (count possible-arm-weights)
        probs (or prior-probs (vec (repeat n (/ 1.0 n))))
        states (mapv (fn [aw]
                       (assoc start-state :arm-weights aw))
                     possible-arm-weights)]
    (dist/categorical-dist states probs)))

(defn make-prize-bandit-pomdp
  "Create a deterministic prize bandit as a POMDP.
   Each arm deterministically gives a specific prize. The agent's uncertainty
   is over which arm gives which prize.

   options:
     :arm-to-prize    - map from arm index to prize string,
                        e.g. {0 \"chocolate\" 1 \"champagne\"}
     :num-trials      - number of pulls allowed
     :prize-to-utility - map from prize to utility value
                         (default {\"chocolate\" 5 \"champagne\" 3 \"nothing\" 0})

   Returns {:world world, :start-state start-state, :utility fn}"
  [options]
  (let [arm-to-prize (:arm-to-prize options)
        num-trials (:num-trials options)
        prize-to-utility (or (:prize-to-utility options)
                             {"chocolate" 5 "champagne" 3 "nothing" 0})
        all-prizes (vec (distinct (vals arm-to-prize)))
        actions (vec (sort (keys arm-to-prize)))

        enumerate-observations-fn
        (fn [state action]
          (let [prize (get (:arm-to-prize state) action "nothing")]
            [[prize] [1.0]]))

        transition-fn
        (fn [state action observation]
          (let [new-time (dec (:time-left state))]
            {:arm-to-prize (:arm-to-prize state)
             :prize observation
             :time-left new-time
             :terminate-after-action (<= new-time 0)}))

        observe-fn
        (fn [state]
          (:prize state))

        actions-fn
        (fn [state]
          (if (:terminate-after-action state)
            []
            actions))

        utility-fn
        (fn [state action]
          (get prize-to-utility (:prize state) 0))

        world {:transition-fn transition-fn
               :enumerate-observations enumerate-observations-fn
               :observe observe-fn
               :state-to-actions actions-fn}

        start-state {:arm-to-prize arm-to-prize
                     :prize "start"
                     :time-left num-trials
                     :terminate-after-action false}]

    {:world world
     :start-state start-state
     :utility utility-fn}))

(defn make-prize-bandit-start-state
  "Create a start state for a prize bandit with a given arm-to-prize mapping.
   Used to construct prior beliefs over different arm-to-prize configurations.
   num-trials: number of pulls
   arm-to-prize: map from arm index to prize string"
  [num-trials arm-to-prize]
  {:arm-to-prize arm-to-prize
   :prize "start"
   :time-left num-trials
   :terminate-after-action false})
