(ns prob.agents.mdp
  (:require [prob.core :as core]))

(defn make-mdp-agent
  "Create an MDP agent with softmax planning.
   params:
     :utility  - (fn [state action] -> number)
     :alpha    - softmax temperature (higher = more rational)
   world: map with :transition, :state-to-actions, :feature

   Returns {:act fn, :expected-utility fn, :params params}
   act: (fn [state]) -> [actions probs]
   expected-utility: (fn [state action]) -> number"
  [params world]
  (let [utility (:utility params)
        alpha (or (:alpha params) 100)
        transition (:transition world)
        actions-fn (:state-to-actions world)
        noise-prob (or (:noise-prob world) 0)
        ;; Atom-based caches for memoization (structural equality on state keys)
        eu-cache (atom {})
        act-cache (atom {})
        ;; Forward declarations via atoms holding functions
        eu-fn-ref (atom nil)
        act-fn-ref (atom nil)]
    ;; Define expected-utility
    (reset! eu-fn-ref
      (fn [state action]
        (let [cache-key [(:loc state) (:time-left state) (:prev-loc state) action]]
          (if-let [cached (get @eu-cache cache-key)]
            cached
            (let [;; Apply transition
                  result
                  (if (zero? noise-prob)
                    ;; Deterministic transition: compute directly
                    (let [next-state (transition state action)
                          u (utility next-state action)]
                      (if (:terminate-after-action next-state)
                        u
                        (let [[actions probs] (@act-fn-ref next-state)]
                          (+ u (reduce + 0
                                 (map (fn [a p] (* p (@eu-fn-ref next-state a)))
                                      actions probs))))))
                    ;; Stochastic transition: enumerate next states
                    (let [[next-states next-probs]
                          (core/enumeration-query-fn
                            (fn [] (transition state action)))]
                      (reduce + 0
                        (map (fn [ns np]
                               (let [u (utility ns action)]
                                 (* np
                                    (if (:terminate-after-action ns)
                                      u
                                      (let [[actions probs] (@act-fn-ref ns)]
                                        (+ u (reduce + 0
                                               (map (fn [a p] (* p (@eu-fn-ref ns a)))
                                                    actions probs))))))))
                             next-states next-probs))))]
              (swap! eu-cache assoc cache-key result)
              result)))))
    ;; Define act
    (reset! act-fn-ref
      (fn [state]
        (let [cache-key [(:loc state) (:time-left state) (:prev-loc state)]]
          (if-let [cached (get @act-cache cache-key)]
            cached
            (let [available (actions-fn state)
                  result (if (empty? available)
                           [[] []]
                           (core/enumeration-query-fn
                             (fn []
                               (let [action (core/uniform-draw available)]
                                 (core/factor (* alpha (@eu-fn-ref state action)))
                                 action))))]
              (swap! act-cache assoc cache-key result)
              result)))))
    {:act @act-fn-ref
     :expected-utility @eu-fn-ref
     :params params}))

(defn simulate-mdp
  "Run agent in world from start-state, return trajectory.
   output-type: :states (default), :actions, :state-action"
  ([start-state world agent] (simulate-mdp start-state world agent :states))
  ([start-state world agent output-type]
   (let [act-fn (:act agent)
         transition (:transition world)]
     (loop [state start-state
            trajectory []]
       (if (:terminate-after-action state)
         (case output-type
           :states (conj trajectory state)
           :actions trajectory
           :state-action (conj trajectory {:state state}))
         (let [[actions probs] (act-fn state)
               action (if (empty? actions)
                        nil
                        (core/categorical actions probs))
               next-state (when action (transition state action))
               entry (case output-type
                       :states state
                       :actions action
                       :state-action {:state state :action action})]
           (if (nil? action)
             (conj trajectory entry)
             (recur next-state (conj trajectory entry)))))))))
