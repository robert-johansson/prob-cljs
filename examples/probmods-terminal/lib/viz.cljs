(ns lib.viz
  "Terminal visualizations: histogram, density plot, barplot."
  (:require [lib.braille :as braille]
            [lib.terminal :as term]))

;; --- Histogram ---

(defn hist
  "Print a horizontal bar chart using Unicode block characters.
   data: sequence of values, label: title string."
  [data label]
  (let [freqs (frequencies data)
        ks (sort-by str (keys freqs))
        max-count (apply max (vals freqs))
        ;; Determine label width for alignment
        labels (map str ks)
        max-label-len (apply max (map count labels))
        bar-width 40]
    (when label
      (println (str "  " (term/bold label))))
    (println (str "  " (term/dim (apply str (repeat (+ max-label-len bar-width 8) "\u2500")))))
    (doseq [k ks]
      (let [cnt (get freqs k)
            lbl (str k)
            padding (apply str (repeat (- max-label-len (count lbl)) " "))
            ;; Full blocks + fractional block for sub-character precision
            raw-width (* (/ cnt max-count) bar-width)
            full-blocks (js/Math.floor raw-width)
            frac (- raw-width full-blocks)
            ;; 8 levels: " " "▏" "▎" "▍" "▌" "▋" "▊" "▉"
            fractional-chars [" " "\u258f" "\u258e" "\u258d" "\u258c" "\u258b" "\u258a" "\u2589"]
            frac-idx (js/Math.round (* frac 7))
            frac-char (if (pos? frac-idx)
                         (nth fractional-chars frac-idx)
                         "")
            bar (str (apply str (repeat full-blocks "\u2588")) frac-char)]
        (println (str "  " padding lbl "  " (term/fg 34 bar) " " cnt))))
    (println)))

;; --- Density plot (braille KDE) ---

(defn density
  "Print a braille-rendered kernel density estimate.
   data: sequence of numbers, label: title string."
  [data label]
  (let [data (vec (sort data))
        n (count data)
        mn (first data)
        mx (last data)
        rng (let [r (- mx mn)] (if (zero? r) 1.0 r))
        ;; Bandwidth: Silverman's rule
        mean-val (/ (reduce + 0 data) n)
        variance (/ (reduce + 0 (map #(js/Math.pow (- % mean-val) 2) data)) n)
        sd (js/Math.sqrt variance)
        bw (max (* 1.06 (js/Math.pow n -0.2) sd) (* rng 0.01))
        ;; Chart buffer: 50 cols x 12 rows = 100x48 pixels
        buf-cols 50
        buf-rows 12
        buf (braille/make-buffer buf-cols buf-rows)
        px-w (:px-w buf)
        px-h (:px-h buf)
        ;; Compute KDE at each pixel x
        steps px-w
        xs (mapv #(+ mn (* rng (/ % steps))) (range (inc steps)))
        kde (fn [x]
              (/ (reduce + 0
                   (map (fn [d]
                          (let [z (/ (- x d) bw)]
                            (* 0.3989422804014327 (js/Math.exp (* -0.5 z z)))))
                        data))
                 (* n bw)))
        ys (mapv kde xs)
        my (apply max ys)]
    (when label
      (println (str "  " (term/bold label))))
    ;; Fill area under curve
    (braille/set-draw-color! buf braille/color-blue)
    (dotimes [i (count xs)]
      (let [px i
            y-val (nth ys i)
            y-px (js/Math.round (- px-h 1 (* (/ y-val my) (- px-h 2))))]
        ;; Draw vertical line from bottom to curve
        (loop [py (dec px-h)]
          (when (>= py y-px)
            (braille/set-pixel! buf px py)
            (recur (dec py))))))
    ;; Draw curve line on top
    (braille/set-draw-color! buf braille/color-cyan)
    (dotimes [i (count xs)]
      (let [px i
            y-val (nth ys i)
            y-px (js/Math.round (- px-h 1 (* (/ y-val my) (- px-h 2))))]
        (braille/set-pixel! buf px y-px)
        (when (pos? i)
          (let [prev-y (nth ys (dec i))
                prev-py (js/Math.round (- px-h 1 (* (/ prev-y my) (- px-h 2))))]
            (braille/draw-line! buf (dec px) prev-py px y-px)))))
    (println (braille/render buf))
    ;; Axis labels
    (println (str "  " (term/dim (.toFixed mn 2))
                  (apply str (repeat (- buf-cols 12) " "))
                  (term/dim (.toFixed mx 2))))
    (println)))

;; --- Barplot ---

(defn barplot
  "Print a bar chart showing probability values.
   dist: (values probs) pair, label: title string."
  [[values probs] label]
  (let [vs (vec values)
        ps (vec (map double probs))
        max-p (apply max ps)
        labels (map str vs)
        max-label-len (apply max (map count labels))
        bar-width 40]
    (when label
      (println (str "  " (term/bold label))))
    (println (str "  " (term/dim (apply str (repeat (+ max-label-len bar-width 14) "\u2500")))))
    (doseq [[v p] (map vector vs ps)]
      (let [lbl (str v)
            padding (apply str (repeat (- max-label-len (count lbl)) " "))
            raw-width (* (/ p max-p) bar-width)
            full-blocks (js/Math.floor raw-width)
            frac (- raw-width full-blocks)
            fractional-chars [" " "\u258f" "\u258e" "\u258d" "\u258c" "\u258b" "\u258a" "\u2589"]
            frac-idx (js/Math.round (* frac 7))
            frac-char (if (pos? frac-idx)
                         (nth fractional-chars frac-idx)
                         "")
            bar (str (apply str (repeat full-blocks "\u2588")) frac-char)]
        (println (str "  " padding lbl "  " (term/fg 34 bar) " " (.toFixed p 3)))))
    (println)))

;; --- Display ---

(defn display
  "Print values to terminal."
  [& args]
  (println (apply str (interpose " " args))))
