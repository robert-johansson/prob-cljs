(ns prob.agents.pomdp
  (:require [prob.core :as core]
            [prob.dist :as dist]))

(defn belief-to-key
  "Convert a categorical-dist belief to a canonical cache key.
   Rounds probabilities to avoid floating-point noise."
  [belief]
  (let [support (dist/enumerate* belief)
        probs (mapv #(js/Math.round (* 1e6 (js/Math.exp (dist/observe* belief %)))) support)
        pairs (sort-by #(pr-str (first %)) (mapv vector support probs))]
    pairs))

(defn make-pomdp-agent
  "Create a POMDP agent that plans over belief distributions.
   params:
     :utility       - (fn [state action] -> number)
     :alpha         - softmax temperature (higher = more rational)
     :prior-belief  - categorical-dist over possible states
   world: map with
     :transition-fn          - (fn [state action observation] -> next-state)
     :enumerate-observations - (fn [state action] -> [[obs1 obs2] [p1 p2]])
     :state-to-actions       - (fn [state] -> [actions])
     :observe                - (fn [state] -> observation)

   Returns {:act fn, :expected-utility fn, :update-belief fn, :params params}
   act: (fn [belief]) -> [actions probs]
   expected-utility: (fn [belief action]) -> number
   update-belief: (fn [belief observation action]) -> new categorical-dist"
  [params world]
  (let [utility (:utility params)
        alpha (or (:alpha params) 100)
        transition-fn (:transition-fn world)
        enumerate-obs (:enumerate-observations world)
        actions-fn (:state-to-actions world)
        ;; Atom-based caches for memoization
        eu-cache (atom {})
        act-cache (atom {})
        ;; Forward declarations via atoms
        eu-fn-ref (atom nil)
        act-fn-ref (atom nil)

        ;; Compute P(obs | state, action) from enumerate-observations
        obs-likelihood
        (fn [state action obs]
          (let [[obs-vals obs-probs] (enumerate-obs state action)]
            (reduce + 0.0
              (map (fn [o p] (if (= o obs) p 0.0))
                   obs-vals obs-probs))))

        update-belief-fn
        (fn [belief observation action]
          (let [support (dist/enumerate* belief)
                weighted
                (keep
                  (fn [state]
                    (let [state-prob (js/Math.exp (dist/observe* belief state))
                          likelihood (obs-likelihood state action observation)]
                      (when (pos? likelihood)
                        (let [next-state (transition-fn state action observation)]
                          [next-state (* state-prob likelihood)]))))
                  support)
                total (reduce + 0.0 (map second weighted))]
            (if (or (empty? weighted) (zero? total))
              belief
              (let [states (mapv first weighted)
                    probs (mapv #(/ (second %) total) weighted)]
                (dist/categorical-dist states probs)))))]

    ;; Define expected-utility: sum over states x observations
    (reset! eu-fn-ref
      (fn [belief action]
        (let [cache-key [(belief-to-key belief) action]]
          (if-let [cached (get @eu-cache cache-key)]
            cached
            (let [support (dist/enumerate* belief)
                  result
                  (reduce + 0.0
                    (map (fn [state]
                           (let [state-prob (js/Math.exp (dist/observe* belief state))
                                 [obs-vals obs-probs] (enumerate-obs state action)]
                             (reduce + 0.0
                               (map (fn [obs obs-prob]
                                      (let [next-state (transition-fn state action obs)
                                            u (utility next-state action)]
                                        (* state-prob obs-prob
                                           (if (:terminate-after-action next-state)
                                             u
                                             (let [new-belief (update-belief-fn belief obs action)
                                                   [acts probs] (@act-fn-ref new-belief)]
                                               (+ u (reduce + 0.0
                                                      (map (fn [a p]
                                                             (* p (@eu-fn-ref new-belief a)))
                                                           acts probs))))))))
                                    obs-vals obs-probs))))
                         support))]
              (swap! eu-cache assoc cache-key result)
              result)))))

    ;; Define act via softmax over expected utility
    (reset! act-fn-ref
      (fn [belief]
        (let [cache-key (belief-to-key belief)]
          (if-let [cached (get @act-cache cache-key)]
            cached
            (let [any-state (first (dist/enumerate* belief))
                  available (actions-fn any-state)
                  result (if (empty? available)
                           [[] []]
                           (core/enumeration-query-fn
                             (fn []
                               (let [action (core/uniform-draw available)]
                                 (core/factor (* alpha (@eu-fn-ref belief action)))
                                 action))))]
              (swap! act-cache assoc cache-key result)
              result)))))

    {:act @act-fn-ref
     :expected-utility @eu-fn-ref
     :update-belief update-belief-fn
     :params params}))

(defn simulate-pomdp
  "Run POMDP agent in world from start-state, return trajectory.
   Each entry: {:state state :action action :observation obs}"
  [start-state world agent]
  (let [act-fn (:act agent)
        update-belief (:update-belief agent)
        transition-fn (:transition-fn world)
        enumerate-obs (:enumerate-observations world)
        observe-fn (:observe world)
        prior-belief (get-in agent [:params :prior-belief])]
    (loop [state start-state
           belief prior-belief
           trajectory []]
      (if (:terminate-after-action state)
        (conj trajectory {:state state :observation (observe-fn state)})
        (let [[actions probs] (act-fn belief)
              action (if (empty? actions)
                       nil
                       (core/categorical actions probs))]
          (if (nil? action)
            (conj trajectory {:state state :observation (observe-fn state)})
            ;; Sample real observation from true state's distribution
            (let [[obs-vals obs-probs] (enumerate-obs state action)
                  observation (core/categorical obs-vals obs-probs)
                  next-state (transition-fn state action observation)
                  new-belief (update-belief belief observation action)]
              (recur next-state
                     new-belief
                     (conj trajectory {:state state
                                       :action action
                                       :observation observation})))))))))
