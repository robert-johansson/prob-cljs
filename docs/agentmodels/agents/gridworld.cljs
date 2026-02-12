(ns prob.agents.gridworld
  (:require [prob.core :as core]))

;; Grid cells: " " (empty), "#" (wall), {:name "Donut"} (named feature)
;; Coordinate system: x right, y up (y=0 = bottom row)
;; Grid array row 0 = top of display = highest y

(defn- get-feature
  "Get the feature at grid position [x y]. Grid row 0 = top = max y."
  [grid x y]
  (let [rows (count grid)
        row (- rows 1 y)]
    (when (and (>= row 0) (< row rows)
              (>= x 0) (< x (count (nth grid row))))
      (nth (nth grid row) x))))

(defn- blocked?
  "Check if position [x y] is blocked (wall or out of bounds)."
  [grid rows cols x y]
  (or (< x 0) (>= x cols) (< y 0) (>= y rows)
      (= "#" (get-feature grid x y))))

(def ^:private action-deltas
  {"up" [0 1] "down" [0 -1] "left" [-1 0] "right" [1 0]})

(def ^:private orthogonal-actions
  {"up" ["left" "right"] "down" ["left" "right"]
   "left" ["up" "down"] "right" ["up" "down"]})

(defn- deterministic-transition
  "Move in direction, staying put if blocked."
  [grid rows cols state action]
  (let [[x y] (:loc state)
        [dx dy] (get action-deltas action [0 0])
        nx (+ x dx) ny (+ y dy)
        new-loc (if (blocked? grid rows cols nx ny) [x y] [nx ny])
        new-time (dec (:time-left state))
        feature (get-feature grid (first new-loc) (second new-loc))
        at-terminal (or (and (map? feature) (:terminal feature))
                        (<= new-time 1))]
    {:loc new-loc
     :time-left new-time
     :terminate-after-action at-terminal
     :prev-loc [x y]}))

(defn- noisy-transition
  "Transition with probability (1-noise) of intended direction,
   and noise/2 of each orthogonal direction."
  [grid rows cols state action noise-prob]
  (if (zero? noise-prob)
    (deterministic-transition grid rows cols state action)
    (let [ortho (get orthogonal-actions action ["up" "down"])
          intended-prob (- 1.0 noise-prob)
          slip-prob (/ noise-prob 2.0)
          chosen (core/categorical [action (first ortho) (second ortho)]
                                   [intended-prob slip-prob slip-prob])]
      (deterministic-transition grid rows cols state chosen))))

(defn make-gridworld-mdp
  "Create a gridworld MDP from options.
   Options:
     :grid         - 2D array of cells (\" \" empty, \"#\" wall, {:name s} feature)
     :start        - [x y] start position
     :total-time   - number of time steps
     :transition-noise-prob - probability of slipping (default 0)
     :no-reverse   - if true, disallow reversing previous move (default false)"
  [options]
  (let [grid (:grid options)
        rows (count grid)
        cols (count (first grid))
        noise-prob (or (:transition-noise-prob options) 0)
        no-reverse (:no-reverse options)
        transition-fn (fn [state action]
                        (if (zero? noise-prob)
                          (deterministic-transition grid rows cols state action)
                          (noisy-transition grid rows cols state action noise-prob)))
        actions-fn (fn [state]
                     (if (:terminate-after-action state)
                       []
                       (let [all-actions (keys action-deltas)]
                         (if (and no-reverse (:prev-loc state))
                           (let [[px py] (:prev-loc state)
                                 [cx cy] (:loc state)
                                 reverse-delta [(- px cx) (- py cy)]
                                 reverse-action (some (fn [[a d]] (when (= d reverse-delta) a))
                                                      action-deltas)]
                             (vec (remove #{reverse-action} all-actions)))
                           (vec all-actions)))))
        feature-fn (fn [state]
                     (let [[x y] (:loc state)]
                       (get-feature grid x y)))
        world {:transition transition-fn
               :state-to-actions actions-fn
               :feature feature-fn
               :grid grid
               :rows rows
               :cols cols
               :noise-prob noise-prob}
        start-state {:loc (:start options)
                     :time-left (:total-time options)
                     :terminate-after-action false
                     :prev-loc nil}]
    {:world world
     :start-state start-state}))

(defn make-utility-function
  "Create a utility function from a world and a utility table.
   utility-table maps feature names to values, plus optional :time-cost.
   Example: {\"Donut\" 5 \"Noodle\" 2 :time-cost -0.1}"
  [world utility-table]
  (let [feature-fn (:feature world)
        time-cost (or (:time-cost utility-table) 0)]
    (fn [state action]
      (let [feature (feature-fn state)
            name (when (map? feature) (:name feature))
            feature-utility (if name (get utility-table name 0) 0)]
        (+ feature-utility time-cost)))))
