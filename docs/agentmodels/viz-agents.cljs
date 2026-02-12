(ns prob.agents.viz
  (:require [clojure.string :as str]))

(def ^:private feature-colors
  {"Donut N" "#f4a460" "Donut S" "#f4a460" "Donut" "#f4a460"
   "Veg" "#90ee90"
   "Noodle" "#ffd700"
   "Hill" "#ff6347"
   "West" "#87ceeb" "East" "#87ceeb"
   "Star" "#ffd700"})

(def ^:private default-cell-size 50)

(defn- svg-el
  "Create an SVG element with attributes."
  [tag attrs]
  (let [el (js/document.createElementNS "http://www.w3.org/2000/svg" tag)]
    (doseq [[k v] attrs]
      (.setAttribute el (name k) (str v)))
    el))

(defn- grid-to-pixel
  "Convert grid [x y] to pixel [px py] (top-left of cell).
   y=0 is bottom row, but pixel y=0 is top."
  [x y rows cell-size]
  [(* x cell-size)
   (* (- rows 1 y) cell-size)])

(defn draw-gridworld
  "Render a gridworld as SVG and append to output.
   world: map from make-gridworld-mdp with :grid :rows :cols
   opts:
     :trajectory     - seq of states (with :loc) to draw as path
     :action-expected-utilities - seq of {:loc [x y] :action str :eu number}
     :show-start     - show start position marker (default true)
     :start          - [x y] start position (uses first trajectory state if not given)
     :cell-size      - pixel size per cell (default 50)
     :label          - optional label above the grid"
  [world opts]
  (let [grid (:grid world)
        rows (:rows world)
        cols (:cols world)
        cs (or (:cell-size opts) default-cell-size)
        trajectory (:trajectory opts)
        show-start (if (contains? opts :show-start) (:show-start opts) true)
        start-loc (or (:start opts)
                      (when (seq trajectory) (:loc (first trajectory))))
        svg-w (* cols cs)
        svg-h (* rows cs)
        container (doto (js/document.createElement "div")
                    (-> .-className (set! "chart-container")))
        _ (when (:label opts)
            (let [title (doto (js/document.createElement "div")
                          (-> .-className (set! "chart-title"))
                          (-> .-textContent (set! (:label opts))))]
              (.appendChild container title)))
        svg (svg-el "svg" {:width svg-w :height svg-h
                           :style "border: 1px solid #ccc; display: block; margin: 8px 0;"})]
    ;; Draw grid cells
    ;; gy iterates over grid array rows (0=top of display)
    (doseq [gy (range rows)]
      (doseq [gx (range cols)]
        (let [feature (nth (nth grid gy) gx)
              ;; grid row 0 = top of display = highest y value
              grid-y (- rows 1 gy)
              [px py] (grid-to-pixel gx grid-y rows cs)
              is-wall (= "#" feature)
              is-feature (map? feature)
              fname (when is-feature (:name feature))
              color (cond
                      is-wall "#555"
                      (and is-feature (get feature-colors fname))
                      (get feature-colors fname)
                      is-feature "#ddd"
                      :else "#eee")]
          (.appendChild svg (svg-el "rect"
                              {:x px :y py :width cs :height cs
                               :fill color :stroke "#ccc" :stroke-width 1}))
          (when (and is-feature fname)
            (let [txt (svg-el "text"
                        {:x (+ px (/ cs 2)) :y (+ py (/ cs 2) 5)
                         :text-anchor "middle"
                         :font-size (min 11 (/ cs 4))
                         :font-family "sans-serif"
                         :fill "#333"})]
              (set! (.-textContent txt) fname)
              (.appendChild svg txt))))))

    ;; Draw trajectory path
    (when (and trajectory (> (count trajectory) 1))
      (let [points (map (fn [state]
                          (let [[x y] (:loc state)
                                [px py] (grid-to-pixel x y rows cs)]
                            [(+ px (/ cs 2)) (+ py (/ cs 2))]))
                        trajectory)
            path-d (str "M" (str/join " L"
                              (map (fn [[px py]] (str px "," py)) points)))]
        (.appendChild svg (svg-el "path"
                            {:d path-d
                             :fill "none"
                             :stroke "#059669"
                             :stroke-width 3
                             :stroke-linejoin "round"
                             :stroke-linecap "round"
                             :opacity 0.8}))
        (doseq [[i state] (map-indexed vector trajectory)]
          (let [[x y] (:loc state)
                [px py] (grid-to-pixel x y rows cs)
                cx (+ px (/ cs 2))
                cy (+ py (/ cs 2))]
            (.appendChild svg (svg-el "circle"
                                {:cx cx :cy cy :r 5
                                 :fill (if (= i 0) "#2563eb" "#059669")
                                 :stroke "#fff" :stroke-width 1.5}))))))

    ;; Draw start marker (when no trajectory)
    (when (and show-start start-loc (not trajectory))
      (let [[x y] start-loc
            [px py] (grid-to-pixel x y rows cs)
            cx (+ px (/ cs 2))
            cy (+ py (/ cs 2))]
        (.appendChild svg (svg-el "circle"
                            {:cx cx :cy cy :r 8
                             :fill "#2563eb"
                             :stroke "#fff" :stroke-width 2
                             :opacity 0.9}))))

    ;; Draw action expected utilities (Q-value annotations)
    (when (:action-expected-utilities opts)
      (let [action-arrows {"up" [0 -1] "down" [0 1] "left" [-1 0] "right" [1 0]}]
        (doseq [{:keys [loc action eu]} (:action-expected-utilities opts)]
          (let [[x y] loc
                [px py] (grid-to-pixel x y rows cs)
                cx (+ px (/ cs 2))
                cy (+ py (/ cs 2))
                [dx dy] (get action-arrows action [0 0])
                offset 16
                tx (+ cx (* dx offset))
                ty (+ cy (* dy offset))
                txt (svg-el "text"
                      {:x tx :y (+ ty 3)
                       :text-anchor "middle"
                       :font-size 9
                       :font-family "monospace"
                       :fill "#666"})]
            (set! (.-textContent txt) (.toFixed eu 1))
            (.appendChild svg txt)))))

    (.appendChild container svg)
    (js/__appendToOutput container)
    nil))
