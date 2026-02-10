(ns lib.physics
  "Planck.js bridge matching ProbMods world format.
   World objects: ((shape static? (w h)) (x y))
   shape: \"circle\" or \"rect\""
  (:require ["planck" :as planck]
            [lib.braille :as braille]
            [lib.terminal :as term]))

(def Vec2 (.-Vec2 planck))
(def World (.-World planck))
(def Circle (.-Circle planck))
(def Box (.-Box planck))

;; World dimensions matching browser ProbMods
(def world-width 350)
(def world-height 500)

;; Physics scale: world coords to physics meters
(def phys-scale (/ 1.0 10))  ; 10 world pixels = 1 meter

;; --- World format helpers ---

(defn obj-shape [obj] (first (first obj)))
(defn obj-static? [obj] (second (first obj)))
(defn obj-dims [obj] (nth (first obj) 2))
(defn obj-w [obj] (first (obj-dims obj)))
(defn obj-h [obj] (second (obj-dims obj)))
(defn obj-x [obj] (first (second obj)))
(defn obj-y [obj] (second (second obj)))

;; --- Create Planck.js world from ProbMods format ---

(defn create-planck-world
  "Build a Planck.js world from ProbMods object list.
   Returns {:world planck-world :bodies [{:body b :obj obj :static? s}]}"
  [objects]
  (let [world (World. (Vec2. 0 10))  ; gravity down
        bodies (atom [])]
    (doseq [obj (vec objects)]
      (let [shape (obj-shape obj)
            static? (obj-static? obj)
            w (obj-w obj)
            h (obj-h obj)
            x (* (obj-x obj) phys-scale)
            y (* (obj-y obj) phys-scale)
            body-type (if static? "static" "dynamic")
            body (.createBody world #js {:type body-type
                                          :position (Vec2. x y)})]
        (if (= shape "circle")
          (.createFixture body (Circle. (* (/ w 2) phys-scale))
                          #js {:density 1.0 :friction 0.5 :restitution 0.2})
          (.createFixture body (Box. (* (/ w 2) phys-scale)
                                     (* (/ h 2) phys-scale))
                          #js {:density 1.0 :friction 0.5 :restitution 0.2}))
        (swap! bodies conj {:body body :obj obj :static? static?
                            :shape shape :w w :h h})))
    {:world world :bodies @bodies}))

;; --- Render world to braille ---

(def render-cols 50)
(def render-rows 20)

(defn render-world-frame
  "Render current physics state to a braille string."
  [bodies]
  (let [buf (braille/make-buffer render-cols render-rows)
        px-w (:px-w buf)
        px-h (:px-h buf)
        ;; Scale: world coords -> pixel coords
        sx (/ px-w world-width)
        sy (/ px-h world-height)]
    (doseq [{:keys [body static? shape w h]} bodies]
      (let [pos (.getPosition body)
            cx (* (/ (.-x pos) phys-scale) sx)
            cy (* (/ (.-y pos) phys-scale) sy)]
        (braille/set-draw-color! buf (if static? braille/color-gray braille/color-cyan))
        (if (= shape "circle")
          (braille/draw-circle! buf cx cy (max 1 (* (/ w 2) sx)))
          (braille/draw-rect! buf
                              (- cx (* (/ w 2) sx))
                              (- cy (* (/ h 2) sy))
                              (+ cx (* (/ w 2) sx))
                              (+ cy (* (/ h 2) sy))))))
    (braille/render buf)))

;; --- Animate physics ---

(defn animate-physics
  "Animate a ProbMods world in the terminal. Returns a promise."
  [steps objects]
  (js/Promise.
   (fn [resolve _reject]
     (let [{:keys [world bodies]} (create-planck-world objects)
           frame (atom 0)
           max-frames (min steps 300)
           dt (/ 1.0 30)
           interval-id (atom nil)]
       (reset! interval-id
         (js/setInterval
          (fn []
            (.step world dt 8 3)
            (let [frame-str (render-world-frame bodies)]
              ;; Move cursor up to overwrite previous frame
              (.write js/process.stdout
                      (str "\033[" (+ render-rows 2) "A"
                           "  " (term/dim (str "Physics  frame " @frame "/" max-frames)) "\n"
                           frame-str)))
            (swap! frame inc)
            (when (>= @frame max-frames)
              (js/clearInterval @interval-id)
              (resolve nil)))
          33))
       ;; Print initial blank space for frame
       (println (str "  " (term/dim (str "Physics  frame 0/" max-frames))))
       (dotimes [_ render-rows]
         (println))))))

;; --- Run physics (no rendering, returns final state) ---

(defn run-physics
  "Step a ProbMods world synchronously. Returns final world in ProbMods format."
  [steps objects]
  (let [{:keys [world bodies]} (create-planck-world objects)
        dt (/ 1.0 60)]
    ;; Step the world
    (dotimes [_ steps]
      (.step world dt 8 3))
    ;; Convert back to ProbMods format
    (map (fn [{:keys [body static? shape w h]}]
           (let [pos (.getPosition body)
                 x (/ (.-x pos) phys-scale)
                 y (/ (.-y pos) phys-scale)]
             (list (list shape static? (list w h)) (list x y))))
         bodies)))
