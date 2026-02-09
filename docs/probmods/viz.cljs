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
