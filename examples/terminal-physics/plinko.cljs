(ns plinko
  (:require ["planck" :as planck]))

;; --- Planck.js interop ---

(def Vec2 (.-Vec2 planck))
(def World (.-World planck))
(def Circle (.-Circle planck))
(def Box (.-Box planck))

;; --- Braille framebuffer ---
;; Each terminal cell encodes a 2x4 pixel grid via Unicode braille (U+2800).
;; Bit positions within a cell:
;;   col0  col1
;;   0     3
;;   1     4
;;   2     5
;;   6     7

(def pixel-bits
  "Map of [dx dy] within a 2x4 cell to the bit index for braille encoding."
  {[0 0] 0, [0 1] 1, [0 2] 2, [0 3] 6,
   [1 0] 3, [1 1] 4, [1 2] 5, [1 3] 7})

(def cols 60)          ; terminal columns for the display
(def rows 30)          ; terminal rows for the display
(def px-w (* cols 2))  ; pixel width  (120)
(def px-h (* rows 4))  ; pixel height (120)

(def framebuf (js/Uint8Array. (* cols rows)))
(def colorbuf (js/Uint8Array. (* cols rows)))  ; ANSI color per cell

;; ANSI color codes
(def color-reset  0)
(def color-white  37)
(def color-gray   90)
(def color-yellow 93)
(def color-cyan   36)
(def color-blue   34)
(def color-red    91)

(def ^:dynamic *draw-color* color-white)

(defn clear! []
  (.fill framebuf 0)
  (.fill colorbuf 0))

(defn set-pixel!
  "Set a pixel at (px, py) in the braille framebuffer."
  [px py]
  (when (and (>= px 0) (< px px-w) (>= py 0) (< py px-h))
    (let [col (quot px 2)
          row (quot py 4)
          dx  (mod px 2)
          dy  (mod py 4)
          idx (+ (* row cols) col)
          bit (get pixel-bits [dx dy])]
      (aset framebuf idx (bit-or (aget framebuf idx) (bit-shift-left 1 bit)))
      (aset colorbuf idx *draw-color*))))

(defn render
  "Return the framebuffer as a string of braille characters with ANSI colors."
  []
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

;; --- Drawing helpers ---

(defn draw-circle!
  "Rasterize a filled circle at pixel coords (cx, cy) with radius r."
  [cx cy r]
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
                (set-pixel! px py)))
            (recur (inc px))))
        (recur (inc py))))))

(defn draw-rect!
  "Rasterize a filled rectangle from (x0,y0) to (x1,y1) in pixel coords."
  [x0 y0 x1 y1]
  (let [x0 (js/Math.round x0) x1 (js/Math.round x1)
        y0 (js/Math.round y0) y1 (js/Math.round y1)]
    (loop [py (min y0 y1)]
      (when (<= py (max y0 y1))
        (loop [px (min x0 x1)]
          (when (<= px (max x0 x1))
            (set-pixel! px py)
            (recur (inc px))))
        (recur (inc py))))))

;; --- Coordinate transform ---
;; World is in meters; we map to pixel coordinates.
;; World origin is at bottom-center of the display.

(def world-scale 8.0)  ; pixels per meter

(defn world->px-x [wx]
  (+ (/ px-w 2) (* wx world-scale)))

(defn world->px-y [wy]
  (- px-h (* wy world-scale)))

;; --- Physics world ---

(def world (World. (Vec2. 0 -10)))

;; Ground
(let [ground (.createBody world #js {:type "static" :position (Vec2. 0 0)})]
  (.createFixture ground (Box. 7 0.2) #js {:friction 0.5}))

;; Walls
(let [left-wall (.createBody world #js {:type "static" :position (Vec2. -7 7)})]
  (.createFixture left-wall (Box. 0.2 7.5) #js {:friction 0.3}))
(let [right-wall (.createBody world #js {:type "static" :position (Vec2. 7 7)})]
  (.createFixture right-wall (Box. 0.2 7.5) #js {:friction 0.3}))

;; Bins at bottom (dividers)
(doseq [bx (range -6 7 2)]
  (let [divider (.createBody world #js {:type "static" :position (Vec2. bx 1.0)})]
    (.createFixture divider (Box. 0.1 1.0) #js {:friction 0.3})))

;; Pegs: triangle arrangement
(def peg-bodies (atom []))

(doseq [row (range 8)
        :let [y (- 12.5 (* row 1.5))
              n-pegs (+ 3 row)
              x-start (- (* (dec n-pegs) 0.75))]]
  (doseq [i (range n-pegs)
          :let [x (+ x-start (* i 1.5))]]
    (let [peg (.createBody world #js {:type "static" :position (Vec2. x y)})]
      (.createFixture peg (Circle. 0.25) #js {:restitution 0.4 :friction 0.1})
      (swap! peg-bodies conj peg))))

;; Ball
(def ball (.createBody world #js {:type "dynamic"
                                   :position (Vec2. (- (* (js/Math.random) 2) 1) 14.5)
                                   :bullet true}))
(.createFixture ball (Circle. 0.35) #js {:restitution 0.3 :density 1.5 :friction 0.1})

;; --- Render a frame ---

(defn draw-frame! []
  (clear!)

  ;; Draw ground
  (set! *draw-color* color-blue)
  (let [x0 (world->px-x -7) y0 (world->px-y 0.2)
        x1 (world->px-x 7)  y1 (world->px-y -0.2)]
    (draw-rect! x0 y0 x1 y1))

  ;; Draw walls
  (set! *draw-color* color-blue)
  (let [lx (world->px-x -7)]
    (draw-rect! (- lx 1.5) (world->px-y 14.5) (+ lx 1.5) (world->px-y -0.5)))
  (let [rx (world->px-x 7)]
    (draw-rect! (- rx 1.5) (world->px-y 14.5) (+ rx 1.5) (world->px-y -0.5)))

  ;; Draw bin dividers
  (set! *draw-color* color-gray)
  (doseq [bx (range -6 7 2)]
    (let [cx (world->px-x bx)]
      (draw-rect! (- cx 0.8) (world->px-y 2.0) (+ cx 0.8) (world->px-y 0.0))))

  ;; Draw pegs
  (set! *draw-color* color-cyan)
  (doseq [peg @peg-bodies]
    (let [pos (.getPosition peg)
          px (world->px-x (.-x pos))
          py (world->px-y (.-y pos))]
      (draw-circle! px py (* 0.25 world-scale))))

  ;; Draw ball (last = highest priority color)
  (set! *draw-color* color-yellow)
  (let [pos (.getPosition ball)
        px (world->px-x (.-x pos))
        py (world->px-y (.-y pos))]
    (draw-circle! px py (* 0.35 world-scale))))

;; --- Animation loop ---

(def frame-count (atom 0))
(def max-frames 300)   ; ~10 seconds at 30fps
(def interval-id (atom nil))

(defn animate! []
  (.step world (/ 1 30) 8 3)
  (draw-frame!)

  ;; ANSI: move cursor to top-left, then print frame
  (let [frame-str (render)
        ball-pos (.getPosition ball)]
    (.write js/process.stdout
            (str "\033[H"   ; cursor home
                 "\033[2J"  ; clear screen
                 "\n  Plinko — Terminal Physics (prob-cljs)\n\n"
                 frame-str
                 "\n  Ball: ("
                 (.toFixed (.-x ball-pos) 2) ", "
                 (.toFixed (.-y ball-pos) 2) ")"
                 "  Frame: " @frame-count "/" max-frames "\n")))

  (swap! frame-count inc)

  ;; Stop when ball is nearly at rest at the bottom or we hit max frames
  (let [vel (.getLinearVelocity ball)
        pos (.getPosition ball)
        speed (+ (js/Math.abs (.-x vel)) (js/Math.abs (.-y vel)))]
    (when (or (>= @frame-count max-frames)
              (and (< (.-y pos) 2.0) (< speed 0.2) (> @frame-count 30)))
      (js/clearInterval @interval-id)
      (.write js/process.stdout "\n  Ball at rest. Done!\n\n"))))

;; Hide cursor, run animation, show cursor on exit
(.write js/process.stdout "\033[?25l")  ; hide cursor

(js/process.on "exit" (fn [] (.write js/process.stdout "\033[?25h")))  ; show cursor on exit
(js/process.on "SIGINT" (fn [] (.write js/process.stdout "\033[?25h\n") (js/process.exit 0)))

(reset! interval-id (js/setInterval animate! 33))
