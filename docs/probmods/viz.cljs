(ns prob.viz
  (:require [prob.core :as core]))

(defn hist [data label]
  (let [data (vec data) freqs (frequencies data) ks (sort-by str (keys freqs))
        n (count ks) mx (apply max (vals freqs))
        bw (max 30 (min 60 (js/Math.floor (/ 500 (max n 1)))))
        cw (+ (* n bw) 40) ch 200 mb 50 mt 20 ph (- ch mb mt)
        c (doto (js/document.createElement "div") (-> .-className (set! "chart-container")))
        t (doto (js/document.createElement "div") (-> .-className (set! "chart-title")) (-> .-textContent (set! (or label ""))))
        _ (.appendChild c t)
        svg (doto (js/document.createElementNS "http://www.w3.org/2000/svg" "svg") (.setAttribute "width" cw) (.setAttribute "height" ch))
        _ (.appendChild c svg)]
    (doseq [[i k] (map-indexed vector ks)]
      (let [cnt (get freqs k) bh (* (/ cnt mx) ph) x (+ 20 (* i bw)) y (+ mt (- ph bh))
            r (doto (js/document.createElementNS "http://www.w3.org/2000/svg" "rect") (.setAttribute "class" "bar") (.setAttribute "x" (+ x 2)) (.setAttribute "y" y) (.setAttribute "width" (- bw 4)) (.setAttribute "height" bh))
            _ (.appendChild svg r)
            tl (doto (js/document.createElementNS "http://www.w3.org/2000/svg" "text") (.setAttribute "class" "axis-label") (.setAttribute "x" (+ x (/ bw 2))) (.setAttribute "y" (- ch 5)) (.setAttribute "text-anchor" "middle") (-> .-textContent (set! (str k))))
            _ (.appendChild svg tl)]))
    (js/__appendToOutput c) nil))

(defn density [data label]
  (let [data (vec (sort data)) n (count data) mn (first data) mx (last data)
        rng (let [r (- mx mn)] (if (zero? r) 1 r))
        bw (max (* 1.06 (js/Math.pow n -0.2) (js/Math.sqrt (core/variance data))) (* rng 0.01))
        steps 80 xs (mapv #(+ mn (* rng (/ % steps))) (range (inc steps)))
        kde (fn [x] (/ (reduce + 0 (map (fn [d] (let [z (/ (- x d) bw)] (* 0.3989422804014327 (js/Math.exp (* -0.5 z z))))) data)) (* n bw)))
        ys (mapv kde xs) my (apply max ys)
        cw 500 ch 200 ml 20 mb 30 mt 10 pw (- cw ml 10) ph (- ch mb mt)
        c (doto (js/document.createElement "div") (-> .-className (set! "chart-container")))
        t (doto (js/document.createElement "div") (-> .-className (set! "chart-title")) (-> .-textContent (set! (or label ""))))
        _ (.appendChild c t)
        svg (doto (js/document.createElementNS "http://www.w3.org/2000/svg" "svg") (.setAttribute "width" cw) (.setAttribute "height" ch))
        _ (.appendChild c svg)
        pts (mapv (fn [i] (let [x (+ ml (* (/ (- (nth xs i) mn) rng) pw)) y (+ mt (- ph (* (/ (nth ys i) my) ph)))] (str x "," y))) (range (count xs)))
        lp (str "M" (clojure.string/join " L" pts))
        ap (str lp " L" (+ ml pw) "," (+ mt ph) " L" ml "," (+ mt ph) " Z")
        _ (.appendChild svg (doto (js/document.createElementNS "http://www.w3.org/2000/svg" "path") (.setAttribute "class" "density-area") (.setAttribute "d" ap)))
        _ (.appendChild svg (doto (js/document.createElementNS "http://www.w3.org/2000/svg" "path") (.setAttribute "class" "density-line") (.setAttribute "d" lp)))]
    (doseq [i (range 6)]
      (let [v (+ mn (* rng (/ i 5))) x (+ ml (* (/ i 5) pw))
            tl (doto (js/document.createElementNS "http://www.w3.org/2000/svg" "text") (.setAttribute "class" "axis-label") (.setAttribute "x" x) (.setAttribute "y" (- ch 5)) (.setAttribute "text-anchor" "middle") (-> .-textContent (set! (.toFixed v 2))))]
        (.appendChild svg tl)))
    (js/__appendToOutput c) nil))

(defn barplot [[values probs] label]
  (let [vs (vec values) ps (vec (map double probs)) n (count vs) mp (apply max ps)
        bw (max 30 (min 60 (js/Math.floor (/ 500 (max n 1)))))
        cw (+ (* n bw) 40) ch 200 mb 50 mt 20 ph (- ch mb mt)
        c (doto (js/document.createElement "div") (-> .-className (set! "chart-container")))
        t (doto (js/document.createElement "div") (-> .-className (set! "chart-title")) (-> .-textContent (set! (or label ""))))
        _ (.appendChild c t)
        svg (doto (js/document.createElementNS "http://www.w3.org/2000/svg" "svg") (.setAttribute "width" cw) (.setAttribute "height" ch))
        _ (.appendChild c svg)]
    (doseq [[i v] (map-indexed vector vs)]
      (let [p (nth ps i) bh (* (/ p mp) ph) x (+ 20 (* i bw)) y (+ mt (- ph bh))
            _ (.appendChild svg (doto (js/document.createElementNS "http://www.w3.org/2000/svg" "rect") (.setAttribute "class" "bar") (.setAttribute "x" (+ x 2)) (.setAttribute "y" y) (.setAttribute "width" (- bw 4)) (.setAttribute "height" bh)))
            _ (.appendChild svg (doto (js/document.createElementNS "http://www.w3.org/2000/svg" "text") (.setAttribute "class" "axis-label") (.setAttribute "x" (+ x (/ bw 2))) (.setAttribute "y" (- ch 5)) (.setAttribute "text-anchor" "middle") (-> .-textContent (set! (str v)))))
            _ (.appendChild svg (doto (js/document.createElementNS "http://www.w3.org/2000/svg" "text") (.setAttribute "class" "axis-label") (.setAttribute "x" (+ x (/ bw 2))) (.setAttribute "y" (- y 3)) (.setAttribute "text-anchor" "middle") (-> .-textContent (set! (.toFixed p 3)))))]))
    (js/__appendToOutput c) nil))

(defn- pair-car [d] (if (contains? d :car) (:car d) (first d)))
(defn- pair-cdr [d] (if (contains? d :cdr) (:cdr d) (second d)))

(defn scatter [data label]
  (let [data (vec data)
        xs (mapv (fn [d] (if (sequential? d) (first d) (pair-car d))) data)
        ys (mapv (fn [d] (if (sequential? d) (second d) (pair-cdr d))) data)
        x-min (apply min xs) x-max (apply max xs) y-min (apply min ys) y-max (apply max ys)
        x-rng (let [r (- x-max x-min)] (if (zero? r) 1 r))
        y-rng (let [r (- y-max y-min)] (if (zero? r) 1 r))
        cw 350 ch 300 ml 40 mr 10 mb 30 mt 20
        pw (- cw ml mr) ph (- ch mb mt)
        c (doto (js/document.createElement "div") (-> .-className (set! "chart-container")))
        t (doto (js/document.createElement "div") (-> .-className (set! "chart-title")) (-> .-textContent (set! (or label ""))))
        _ (.appendChild c t)
        svg (doto (js/document.createElementNS "http://www.w3.org/2000/svg" "svg") (.setAttribute "width" cw) (.setAttribute "height" ch))
        _ (.appendChild c svg)]
    ;; axes
    (.appendChild svg (doto (js/document.createElementNS "http://www.w3.org/2000/svg" "line")
                        (.setAttribute "x1" ml) (.setAttribute "y1" mt) (.setAttribute "x2" ml) (.setAttribute "y2" (+ mt ph))
                        (.setAttribute "stroke" "#999") (.setAttribute "stroke-width" "1")))
    (.appendChild svg (doto (js/document.createElementNS "http://www.w3.org/2000/svg" "line")
                        (.setAttribute "x1" ml) (.setAttribute "y1" (+ mt ph)) (.setAttribute "x2" (+ ml pw)) (.setAttribute "y2" (+ mt ph))
                        (.setAttribute "stroke" "#999") (.setAttribute "stroke-width" "1")))
    ;; axis labels
    (doseq [i (range 6)]
      (let [v (+ x-min (* x-rng (/ i 5))) x (+ ml (* (/ i 5) pw))]
        (.appendChild svg (doto (js/document.createElementNS "http://www.w3.org/2000/svg" "text")
                            (.setAttribute "class" "axis-label") (.setAttribute "x" x) (.setAttribute "y" (- ch 5))
                            (.setAttribute "text-anchor" "middle") (-> .-textContent (set! (str (js/Math.round v))))))))
    (doseq [i (range 6)]
      (let [v (+ y-min (* y-rng (/ i 5))) y (- (+ mt ph) (* (/ i 5) ph))]
        (.appendChild svg (doto (js/document.createElementNS "http://www.w3.org/2000/svg" "text")
                            (.setAttribute "class" "axis-label") (.setAttribute "x" (- ml 5)) (.setAttribute "y" (+ y 4))
                            (.setAttribute "text-anchor" "end") (-> .-textContent (set! (str (js/Math.round v))))))))
    ;; points
    (doseq [i (range (count data))]
      (let [px (+ ml (* (/ (- (nth xs i) x-min) x-rng) pw))
            py (- (+ mt ph) (* (/ (- (nth ys i) y-min) y-rng) ph))]
        (.appendChild svg (doto (js/document.createElementNS "http://www.w3.org/2000/svg" "circle")
                            (.setAttribute "cx" px) (.setAttribute "cy" py) (.setAttribute "r" "3")
                            (.setAttribute "fill" "#4a90d9") (.setAttribute "opacity" "0.6")))))
    (js/__appendToOutput c) nil))

(defn lineplot [data label]
  (let [;; Normalize to multi-series format: [{:data [[x y]...] :label "name"} ...]
        series (if (and (seq data) (map? (first data)))
                 (vec data)
                 [{:data (vec data) :label ""}])
        colors ["#4a90d9" "#e74c3c" "#2ecc71" "#f39c12" "#9b59b6" "#1abc9c"]
        all-pts (mapcat :data series)
        xs (map first all-pts) ys (map second all-pts)
        x-min (apply min xs) x-max (apply max xs)
        y-min (apply min ys) y-max (apply max ys)
        x-rng (let [r (- x-max x-min)] (if (zero? r) 1 r))
        y-rng (let [r (- y-max y-min)] (if (zero? r) 1 r))
        cw 500 ch 300 ml 50 mr 20 mb 30 mt 20
        pw (- cw ml mr) ph (- ch mb mt)
        c (doto (js/document.createElement "div") (-> .-className (set! "chart-container")))
        t (doto (js/document.createElement "div") (-> .-className (set! "chart-title")) (-> .-textContent (set! (or label ""))))
        _ (.appendChild c t)
        svg (doto (js/document.createElementNS "http://www.w3.org/2000/svg" "svg") (.setAttribute "width" cw) (.setAttribute "height" ch))
        _ (.appendChild c svg)
        sx (fn [x] (+ ml (* (/ (- x x-min) x-rng) pw)))
        sy (fn [y] (- (+ mt ph) (* (/ (- y y-min) y-rng) ph)))]
    ;; axes
    (.appendChild svg (doto (js/document.createElementNS "http://www.w3.org/2000/svg" "line")
                        (.setAttribute "x1" ml) (.setAttribute "y1" mt) (.setAttribute "x2" ml) (.setAttribute "y2" (+ mt ph))
                        (.setAttribute "stroke" "#999") (.setAttribute "stroke-width" "1")))
    (.appendChild svg (doto (js/document.createElementNS "http://www.w3.org/2000/svg" "line")
                        (.setAttribute "x1" ml) (.setAttribute "y1" (+ mt ph)) (.setAttribute "x2" (+ ml pw)) (.setAttribute "y2" (+ mt ph))
                        (.setAttribute "stroke" "#999") (.setAttribute "stroke-width" "1")))
    ;; x-axis labels
    (doseq [i (range 6)]
      (let [v (+ x-min (* x-rng (/ i 5))) x (sx v)]
        (.appendChild svg (doto (js/document.createElementNS "http://www.w3.org/2000/svg" "text")
                            (.setAttribute "class" "axis-label") (.setAttribute "x" x) (.setAttribute "y" (- ch 5))
                            (.setAttribute "text-anchor" "middle") (-> .-textContent (set! (.toFixed v 2)))))))
    ;; y-axis labels
    (doseq [i (range 6)]
      (let [v (+ y-min (* y-rng (/ i 5))) y (sy v)]
        (.appendChild svg (doto (js/document.createElementNS "http://www.w3.org/2000/svg" "text")
                            (.setAttribute "class" "axis-label") (.setAttribute "x" (- ml 5)) (.setAttribute "y" (+ y 4))
                            (.setAttribute "text-anchor" "end") (-> .-textContent (set! (.toFixed v 2)))))))
    ;; series lines
    (doseq [[idx s] (map-indexed vector series)]
      (let [pts (sort-by first (:data s))
            color (nth colors (mod idx (count colors)))
            path-d (str "M" (clojure.string/join " L" (map (fn [[x y]] (str (sx x) "," (sy y))) pts)))]
        (.appendChild svg (doto (js/document.createElementNS "http://www.w3.org/2000/svg" "path")
                            (.setAttribute "d" path-d)
                            (.setAttribute "fill" "none")
                            (.setAttribute "stroke" color)
                            (.setAttribute "stroke-width" "2")))))
    ;; legend for multi-series
    (when (> (count series) 1)
      (doseq [[idx s] (map-indexed vector series)]
        (let [color (nth colors (mod idx (count colors)))
              ly (+ mt 5 (* idx 16))]
          (.appendChild svg (doto (js/document.createElementNS "http://www.w3.org/2000/svg" "line")
                              (.setAttribute "x1" (+ ml pw -80)) (.setAttribute "y1" ly)
                              (.setAttribute "x2" (+ ml pw -60)) (.setAttribute "y2" ly)
                              (.setAttribute "stroke" color) (.setAttribute "stroke-width" "2")))
          (.appendChild svg (doto (js/document.createElementNS "http://www.w3.org/2000/svg" "text")
                              (.setAttribute "class" "axis-label") (.setAttribute "x" (+ ml pw -55)) (.setAttribute "y" (+ ly 4))
                              (-> .-textContent (set! (or (:label s) ""))))))))
    (js/__appendToOutput c) nil))

(defn table [data label]
  (let [c (doto (js/document.createElement "div") (-> .-className (set! "chart-container")))
        t (doto (js/document.createElement "div") (-> .-className (set! "chart-title")) (-> .-textContent (set! (or label ""))))
        _ (.appendChild c t)
        tbl (doto (js/document.createElement "table")
              (-> .-style .-borderCollapse (set! "collapse"))
              (-> .-style .-margin (set! "8px auto"))
              (-> .-style .-fontFamily (set! "monospace"))
              (-> .-style .-fontSize (set! "13px")))]
    (doseq [row data]
      (let [tr (js/document.createElement "tr")]
        (doseq [cell row]
          (let [td (doto (js/document.createElement "td")
                     (-> .-style .-padding (set! "4px 10px"))
                     (-> .-style .-border (set! "1px solid #ddd"))
                     (-> .-textContent (set! (str cell))))]
            (.appendChild tr td)))
        (.appendChild tbl tr)))
    (.appendChild c tbl)
    (js/__appendToOutput c) nil))

(defn heatmap [data label]
  (let [data (vec data)
        ;; data: seq of [x y value] triples OR seq of seqs (matrix)
        triples (if (and (sequential? (first data)) (= 3 (count (first data))))
                  data
                  ;; matrix form: row-index = y, col-index = x
                  (for [[r row] (map-indexed vector data)
                        [c val] (map-indexed vector row)]
                    [c r val]))
        triples (vec triples)
        xs (mapv first triples) ys (mapv second triples) vs (mapv #(nth % 2) triples)
        x-vals (vec (sort (distinct xs))) y-vals (vec (sort (distinct ys)))
        v-min (apply min vs) v-max (apply max vs)
        v-rng (let [r (- v-max v-min)] (if (zero? r) 1 r))
        nx (count x-vals) ny (count y-vals)
        cell-w (max 15 (min 40 (js/Math.floor (/ 400 (max nx 1)))))
        cell-h (max 15 (min 40 (js/Math.floor (/ 300 (max ny 1)))))
        ml 40 mt 20 mb 30 mr 60
        cw (+ ml (* nx cell-w) mr) ch (+ mt (* ny cell-h) mb)
        c (doto (js/document.createElement "div") (-> .-className (set! "chart-container")))
        t (doto (js/document.createElement "div") (-> .-className (set! "chart-title")) (-> .-textContent (set! (or label ""))))
        _ (.appendChild c t)
        svg (doto (js/document.createElementNS "http://www.w3.org/2000/svg" "svg") (.setAttribute "width" cw) (.setAttribute "height" ch))
        _ (.appendChild c svg)
        x-idx (into {} (map-indexed (fn [i v] [v i]) x-vals))
        y-idx (into {} (map-indexed (fn [i v] [v i]) y-vals))
        interp (fn [v] (let [t (/ (- v v-min) v-rng)
                              r (js/Math.round (+ 255 (* (- 74 255) t)))
                              g (js/Math.round (+ 255 (* (- 144 255) t)))
                              b (js/Math.round (+ 255 (* (- 217 255) t)))]
                          (str "rgb(" r "," g "," b ")")))]
    ;; cells
    (doseq [[x y v] triples]
      (let [xi (get x-idx x) yi (get y-idx y)
            px (+ ml (* xi cell-w)) py (+ mt (* yi cell-h))]
        (.appendChild svg (doto (js/document.createElementNS "http://www.w3.org/2000/svg" "rect")
                            (.setAttribute "x" px) (.setAttribute "y" py)
                            (.setAttribute "width" cell-w) (.setAttribute "height" cell-h)
                            (.setAttribute "fill" (interp v))
                            (.setAttribute "stroke" "#fff") (.setAttribute "stroke-width" "1")))))
    ;; x labels
    (doseq [[i v] (map-indexed vector x-vals)]
      (.appendChild svg (doto (js/document.createElementNS "http://www.w3.org/2000/svg" "text")
                          (.setAttribute "class" "axis-label")
                          (.setAttribute "x" (+ ml (* i cell-w) (/ cell-w 2)))
                          (.setAttribute "y" (+ mt (* ny cell-h) 15))
                          (.setAttribute "text-anchor" "middle")
                          (.setAttribute "font-size" "10")
                          (-> .-textContent (set! (str v))))))
    ;; y labels
    (doseq [[i v] (map-indexed vector y-vals)]
      (.appendChild svg (doto (js/document.createElementNS "http://www.w3.org/2000/svg" "text")
                          (.setAttribute "class" "axis-label")
                          (.setAttribute "x" (- ml 5))
                          (.setAttribute "y" (+ mt (* i cell-h) (/ cell-h 2) 4))
                          (.setAttribute "text-anchor" "end")
                          (.setAttribute "font-size" "10")
                          (-> .-textContent (set! (str v))))))
    ;; color scale legend
    (let [lx (+ ml (* nx cell-w) 15) lw 15 lh (* ny cell-h)]
      (doseq [i (range 20)]
        (let [t (/ i 19) y (+ mt (* (- 1 t) lh (/ 1 20) 19))]
          (.appendChild svg (doto (js/document.createElementNS "http://www.w3.org/2000/svg" "rect")
                              (.setAttribute "x" lx) (.setAttribute "y" (+ mt (* (- 19 i) (/ lh 20))))
                              (.setAttribute "width" lw) (.setAttribute "height" (/ lh 20))
                              (.setAttribute "fill" (interp (+ v-min (* t v-rng))))))))
      (.appendChild svg (doto (js/document.createElementNS "http://www.w3.org/2000/svg" "text")
                          (.setAttribute "class" "axis-label") (.setAttribute "x" (+ lx lw 4)) (.setAttribute "y" (+ mt 10))
                          (.setAttribute "font-size" "9") (-> .-textContent (set! (.toFixed v-max 2)))))
      (.appendChild svg (doto (js/document.createElementNS "http://www.w3.org/2000/svg" "text")
                          (.setAttribute "class" "axis-label") (.setAttribute "x" (+ lx lw 4)) (.setAttribute "y" (+ mt lh))
                          (.setAttribute "font-size" "9") (-> .-textContent (set! (.toFixed v-min 2))))))
    (js/__appendToOutput c) nil))

(defn marginals [data label]
  (let [;; data: seq of maps or seq of seqs; extract each "column" as a marginal
        data (vec data)
        sample (first data)]
    (cond
      ;; seq of maps: one marginal per key
      (map? sample)
      (let [ks (keys sample)]
        (doseq [k ks]
          (let [values (mapv #(get % k) data)
                sub-label (str (or label "") (when label " - ") (name k))]
            (if (every? number? values)
              (density values sub-label)
              (hist values sub-label)))))
      ;; seq of seqs/vectors: one marginal per index
      (or (sequential? sample) (vector? sample))
      (let [n (count sample)]
        (doseq [i (range n)]
          (let [values (mapv #(nth (vec %) i) data)
                sub-label (str (or label "") (when label " - ") "dim " i)]
            (if (every? number? values)
              (density values sub-label)
              (hist values sub-label)))))
      ;; scalar: just a single histogram
      :else (hist data label))))

(defn- classify-data [values]
  (cond
    (every? number? values) (if (every? integer? values) :integer :real)
    :else :categorical))

(defn auto [data label]
  (let [data (vec data)
        sample (first data)]
    (cond
      ;; [values probs] pair from enumeration
      (and (= 2 (count data))
           (sequential? (first data))
           (sequential? (second data))
           (every? number? (second data)))
      (barplot data label)

      ;; seq of [x y] pairs
      (and (sequential? sample) (= 2 (count sample)) (every? number? (map first data)))
      (let [xs (map first data) ys (map second data)
            x-type (classify-data xs) y-type (classify-data ys)]
        (cond
          (and (= :real x-type) (= :real y-type)) (scatter data label)
          (and (#{:integer :real} x-type) (#{:integer :real} y-type)) (lineplot data label)
          :else (scatter data label)))

      ;; seq of maps -> marginals
      (map? sample)
      (marginals data label)

      ;; seq of numbers
      (every? number? data)
      (if (> (count (distinct data)) 20)
        (density data label)
        (hist data label))

      ;; seq of categorical values
      :else (hist data label))))

(defn display [& args]
  (js/__appendTextToOutput (apply str (interpose " " args))))

;; Physics bridge (requires box2d + phys.js)
(defn cljs->js-world [world]
  (let [to-vec (fn tv [x] (cond (or (seq? x) (list? x) (vector? x)) (mapv tv x) :else x))]
    (clj->js (to-vec (vec world)))))

(defn run-physics [steps world]
  (let [result (js/runPhysics steps (cljs->js-world world))]
    (map (fn [obj] (let [o (js->clj obj)] (list (list (nth (first o) 0) (nth (first o) 1) (apply list (nth (first o) 2))) (apply list (second o))))) result)))

(defn animate-physics [steps world]
  (js/animatePhysics steps (cljs->js-world world) nil))
