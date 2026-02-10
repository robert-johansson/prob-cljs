(ns lib.braille
  "Braille framebuffer for terminal rendering.
   Each terminal cell encodes a 2x4 pixel grid via Unicode braille (U+2800).")

;; Bit positions within a 2x4 cell:
;;   col0  col1
;;   0     3
;;   1     4
;;   2     5
;;   6     7

(def pixel-bits
  {[0 0] 0, [0 1] 1, [0 2] 2, [0 3] 6,
   [1 0] 3, [1 1] 4, [1 2] 5, [1 3] 7})

;; ANSI color codes
(def color-reset  0)
(def color-white  37)
(def color-gray   90)
(def color-yellow 93)
(def color-cyan   36)
(def color-blue   34)
(def color-red    91)
(def color-green  32)
(def color-magenta 35)

(defn make-buffer
  "Create a framebuffer with given terminal dimensions.
   Returns {:cols :rows :px-w :px-h :framebuf :colorbuf :draw-color}."
  [cols rows]
  {:cols cols
   :rows rows
   :px-w (* cols 2)
   :px-h (* rows 4)
   :framebuf (js/Uint8Array. (* cols rows))
   :colorbuf (js/Uint8Array. (* cols rows))
   :draw-color (atom color-white)})

(defn clear! [{:keys [framebuf colorbuf]}]
  (.fill framebuf 0)
  (.fill colorbuf 0))

(defn set-pixel!
  "Set a pixel at (px, py) in the braille framebuffer."
  [{:keys [cols px-w px-h framebuf colorbuf draw-color]} px py]
  (when (and (>= px 0) (< px px-w) (>= py 0) (< py px-h))
    (let [col (quot px 2)
          row (quot py 4)
          dx  (mod px 2)
          dy  (mod py 4)
          idx (+ (* row cols) col)
          bit (get pixel-bits [dx dy])]
      (aset framebuf idx (bit-or (aget framebuf idx) (bit-shift-left 1 bit)))
      (aset colorbuf idx @draw-color))))

(defn render
  "Return the framebuffer as a string of braille characters with ANSI colors."
  [{:keys [cols rows framebuf colorbuf]}]
  (let [sb (array)
        prev-color (atom 0)]
    (dotimes [r rows]
      (dotimes [c cols]
        (let [idx (+ (* r cols) c)
              v   (aget framebuf idx)
              clr (aget colorbuf idx)]
          (when (and (pos? v) (not= clr @prev-color))
            (.push sb (str "\033[" clr "m"))
            (reset! prev-color clr))
          (when (and (zero? v) (pos? @prev-color))
            (.push sb "\033[0m")
            (reset! prev-color 0))
          (.push sb (.fromCharCode js/String (+ 0x2800 v)))))
      (.push sb "\n"))
    (.push sb "\033[0m")
    (.join sb "")))

(defn set-draw-color! [buf color]
  (reset! (:draw-color buf) color))

(defn draw-circle!
  "Rasterize a filled circle at pixel coords (cx, cy) with radius r."
  [buf cx cy r]
  (let [x0 (js/Math.round (- cx r))
        x1 (js/Math.round (+ cx r))
        y0 (js/Math.round (- cy r))
        y1 (js/Math.round (+ cy r))
        r2 (* r r)]
    (loop [py y0]
      (when (<= py y1)
        (loop [px x0]
          (when (<= px x1)
            (let [dx (- px cx)
                  dy (- py cy)]
              (when (<= (+ (* dx dx) (* dy dy)) r2)
                (set-pixel! buf px py)))
            (recur (inc px))))
        (recur (inc py))))))

(defn draw-rect!
  "Rasterize a filled rectangle from (x0,y0) to (x1,y1) in pixel coords."
  [buf x0 y0 x1 y1]
  (let [x0 (js/Math.round x0) x1 (js/Math.round x1)
        y0 (js/Math.round y0) y1 (js/Math.round y1)]
    (loop [py (min y0 y1)]
      (when (<= py (max y0 y1))
        (loop [px (min x0 x1)]
          (when (<= px (max x0 x1))
            (set-pixel! buf px py)
            (recur (inc px))))
        (recur (inc py))))))

(defn draw-line!
  "Rasterize a line from (x0,y0) to (x1,y1) using Bresenham's algorithm."
  [buf x0 y0 x1 y1]
  (let [x0 (js/Math.round x0) y0 (js/Math.round y0)
        x1 (js/Math.round x1) y1 (js/Math.round y1)
        dx (js/Math.abs (- x1 x0))
        dy (- (js/Math.abs (- y1 y0)))
        sx (if (< x0 x1) 1 -1)
        sy (if (< y0 y1) 1 -1)]
    (loop [x x0 y y0 err (+ dx dy)]
      (set-pixel! buf x y)
      (when-not (and (= x x1) (= y y1))
        (let [e2 (* 2 err)
              [x' err'] (if (>= e2 dy) [(+ x sx) (+ err dy)] [x err])
              [y' err''] (if (<= e2 dx) [(+ y sy) (+ err' dx)] [y err'])]
          (recur x' y' err''))))))
