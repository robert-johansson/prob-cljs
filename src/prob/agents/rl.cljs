(ns prob.agents.rl
  (:require [prob.core :as core]
            [prob.dist :as dist]))

(defn make-greedy-bandit-agent
  "Create a softmax-greedy bandit agent.
   params:
     :prior-belief  - categorical-dist over possible arm-weight maps
                      e.g. categorical over [{0 0.5, 1 0.9}, {0 0.9, 1 0.5}]
     :alpha         - softmax temperature
     :actions       - vector of arm indices [0 1 ...]
     :reward-values - optional {:win \"win\" :lose \"lose\"} (default uses these strings)

   Returns {:act fn, :update-belief fn}"
  [params]
  (let [alpha (or (:alpha params) 10)
        actions (:actions params)
        prior-belief (:prior-belief params)

        expected-reward-fn
        (fn [belief action]
          (let [support (dist/enumerate* belief)]
            (reduce + 0.0
              (map (fn [arm-weights]
                     (let [weight-prob (js/Math.exp (dist/observe* belief arm-weights))
                           arm-weight (get arm-weights action 0.5)]
                       (* weight-prob arm-weight)))
                   support))))

        act-fn
        (fn [belief]
          (core/enumeration-query-fn
            (fn []
              (let [action (core/uniform-draw actions)]
                (core/factor (* alpha (expected-reward-fn belief action)))
                action))))

        update-belief-fn
        (fn [belief reward action]
          (let [support (dist/enumerate* belief)
                won (= reward "win")
                weighted
                (keep
                  (fn [arm-weights]
                    (let [prior-prob (js/Math.exp (dist/observe* belief arm-weights))
                          arm-weight (get arm-weights action 0.5)
                          likelihood (if won arm-weight (- 1.0 arm-weight))
                          posterior-unnorm (* prior-prob likelihood)]
                      (when (pos? posterior-unnorm)
                        [arm-weights posterior-unnorm])))
                  support)
                total (reduce + 0.0 (map second weighted))]
            (if (or (empty? weighted) (zero? total))
              belief
              (let [states (mapv first weighted)
                    probs (mapv #(/ (second %) total) weighted)]
                (dist/categorical-dist states probs)))))]

    {:act act-fn
     :update-belief update-belief-fn
     :expected-reward expected-reward-fn
     :params params}))

(defn simulate-greedy-bandit
  "Run a greedy bandit agent for num-trials.
   env: {:true-arm-weights {0 0.5 1 0.9}} — Bernoulli success probabilities
   agent: from make-greedy-bandit-agent
   num-trials: number of pulls

   Returns vector of {:action a :reward \"win\"/\"lose\" :belief b :expected-rewards {...}}"
  [env agent num-trials]
  (let [true-weights (:true-arm-weights env)
        act-fn (:act agent)
        update-fn (:update-belief agent)
        er-fn (:expected-reward agent)
        actions (get-in agent [:params :actions])
        initial-belief (get-in agent [:params :prior-belief])]
    (loop [trial 0
           belief initial-belief
           trajectory []]
      (if (>= trial num-trials)
        trajectory
        (let [[action-vals action-probs] (act-fn belief)
              action (core/categorical action-vals action-probs)
              true-weight (get true-weights action 0.5)
              reward (if (core/flip true-weight) "win" "lose")
              expected-rewards (into {} (map (fn [a] [a (er-fn belief a)]) actions))
              new-belief (update-fn belief reward action)]
          (recur (inc trial)
                 new-belief
                 (conj trajectory {:action action
                                   :reward reward
                                   :belief belief
                                   :expected-rewards expected-rewards})))))))

(defn make-posterior-sampling-agent
  "Create a Thompson sampling (posterior sampling) bandit agent.
   Same interface as greedy agent but acts by sampling arm-weights
   from posterior, then picking the arm with highest sampled weight.
   params: same as make-greedy-bandit-agent"
  [params]
  (let [actions (:actions params)
        prior-belief (:prior-belief params)

        act-fn
        (fn [belief]
          ;; Sample one arm-weights map from belief, pick best arm
          (let [sampled-weights (dist/sample* belief)
                best-action (apply max-key #(get sampled-weights % 0) actions)]
            ;; Return as [actions probs] with all mass on best
            [(vec actions)
             (mapv #(if (= % best-action) 1.0 0.0) actions)]))

        update-belief-fn
        (fn [belief reward action]
          (let [support (dist/enumerate* belief)
                won (= reward "win")
                weighted
                (keep
                  (fn [arm-weights]
                    (let [prior-prob (js/Math.exp (dist/observe* belief arm-weights))
                          arm-weight (get arm-weights action 0.5)
                          likelihood (if won arm-weight (- 1.0 arm-weight))
                          posterior-unnorm (* prior-prob likelihood)]
                      (when (pos? posterior-unnorm)
                        [arm-weights posterior-unnorm])))
                  support)
                total (reduce + 0.0 (map second weighted))]
            (if (or (empty? weighted) (zero? total))
              belief
              (let [states (mapv first weighted)
                    probs (mapv #(/ (second %) total) weighted)]
                (dist/categorical-dist states probs)))))]

    {:act act-fn
     :update-belief update-belief-fn
     :params params}))

(defn simulate-posterior-sampling
  "Run a posterior sampling agent for num-trials.
   Same interface as simulate-greedy-bandit."
  [env agent num-trials]
  (let [true-weights (:true-arm-weights env)
        act-fn (:act agent)
        update-fn (:update-belief agent)
        initial-belief (get-in agent [:params :prior-belief])]
    (loop [trial 0
           belief initial-belief
           trajectory []]
      (if (>= trial num-trials)
        trajectory
        (let [[action-vals action-probs] (act-fn belief)
              action (core/categorical action-vals action-probs)
              true-weight (get true-weights action 0.5)
              reward (if (core/flip true-weight) "win" "lose")
              new-belief (update-fn belief reward action)]
          (recur (inc trial)
                 new-belief
                 (conj trajectory {:action action
                                   :reward reward
                                   :belief belief})))))))

(defn cumulative-regret
  "Compute cumulative regret curve.
   trajectory: from simulate-greedy-bandit or simulate-posterior-sampling
   true-arm-weights: {0 0.5 1 0.9}
   Returns vector of [trial cumulative-regret] pairs."
  [trajectory true-arm-weights]
  (let [best-weight (apply max (vals true-arm-weights))]
    (loop [i 0
           total-regret 0.0
           result []]
      (if (>= i (count trajectory))
        result
        (let [entry (nth trajectory i)
              action (:action entry)
              arm-weight (get true-arm-weights action 0)
              regret (- best-weight arm-weight)
              new-total (+ total-regret regret)]
          (recur (inc i)
                 new-total
                 (conj result [(inc i) new-total])))))))
