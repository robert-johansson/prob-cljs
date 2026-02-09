(ns prob.erp
  "Elementary Random Primitives (ERPs).
   These are sampling-only for rejection sampling; log-prob and
   proposal functions can be added later for MH.")

;; ---------------------------------------------------------------------------
;; Core random
;; ---------------------------------------------------------------------------

(defn ^:private rand-uniform []
  (js/Math.random))

;; ---------------------------------------------------------------------------
;; Bernoulli / flip
;; ---------------------------------------------------------------------------

(defn flip
  ([] (< (rand-uniform) 0.5))
  ([p] (< (rand-uniform) p)))

;; ---------------------------------------------------------------------------
;; Continuous uniform
;; ---------------------------------------------------------------------------

(defn uniform [a b]
  (+ a (* (rand-uniform) (- b a))))

;; ---------------------------------------------------------------------------
;; Discrete uniform draw
;; ---------------------------------------------------------------------------

(defn uniform-draw [lst]
  (let [items (vec lst)
        n (count items)]
    (nth items (js/Math.floor (* (rand-uniform) n)))))

;; ---------------------------------------------------------------------------
;; Random integer [0, n)
;; ---------------------------------------------------------------------------

(defn random-integer [n]
  (js/Math.floor (* (rand-uniform) n)))

;; ---------------------------------------------------------------------------
;; Multinomial (weighted discrete choice)
;; ---------------------------------------------------------------------------

(defn multinomial [items probs]
  (let [items (vec items)
        probs (vec probs)
        total (reduce + 0 probs)
        r (* (rand-uniform) total)]
    (loop [i 0, cumulative 0]
      (if (>= i (count items))
        (peek items)  ;; fallback to last
        (let [cumulative (+ cumulative (nth probs i))]
          (if (< r cumulative)
            (nth items i)
            (recur (inc i) cumulative)))))))

(defn sample-discrete [weights]
  (let [weights (vec weights)
        indices (vec (range (count weights)))]
    (multinomial indices weights)))

;; ---------------------------------------------------------------------------
;; Gaussian (Box-Muller transform)
;; ---------------------------------------------------------------------------

(defn gaussian
  ([] (gaussian 0 1))
  ([mu sigma]
   (if (zero? sigma)
     mu
     (let [u1 (rand-uniform)
           u2 (rand-uniform)
           z (* (js/Math.sqrt (* -2 (js/Math.log u1)))
                (js/Math.cos (* 2 js/Math.PI u2)))]
       (+ mu (* sigma z))))))

;; ---------------------------------------------------------------------------
;; Gamma (Marsaglia and Tsang's method)
;; ---------------------------------------------------------------------------

(defn gamma [a b]
  ;; a = shape, b = scale
  (if (< a 1)
    ;; For a < 1, use the relation: gamma(a) = gamma(a+1) * U^(1/a)
    (* (gamma (inc a) b)
       (js/Math.pow (rand-uniform) (/ 1 a)))
    ;; Marsaglia and Tsang method for a >= 1
    (let [d (- a (/ 1 3))
          c (/ 1 (js/Math.sqrt (* 9 d)))]
      (loop []
        (let [x (gaussian 0 1)
              v (js/Math.pow (+ 1 (* c x)) 3)]
          (if (and (> v 0)
                   (< (js/Math.log (rand-uniform))
                      (+ (* 0.5 x x)
                         (* d (- 1 v (js/Math.log v))))))
            (* d v b)
            (recur)))))))

;; ---------------------------------------------------------------------------
;; Beta
;; ---------------------------------------------------------------------------

(defn beta [a b]
  (let [x (gamma a 1)
        y (gamma b 1)]
    (/ x (+ x y))))

;; ---------------------------------------------------------------------------
;; Dirichlet
;; ---------------------------------------------------------------------------

(defn dirichlet [alpha-vec]
  (let [alpha (vec alpha-vec)
        samples (mapv #(gamma % 1) alpha)
        total (reduce + 0 samples)]
    (mapv #(/ % total) samples)))

;; ---------------------------------------------------------------------------
;; Exponential
;; ---------------------------------------------------------------------------

(defn exponential [rate]
  (/ (- (js/Math.log (rand-uniform))) rate))
