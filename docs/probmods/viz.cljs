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
