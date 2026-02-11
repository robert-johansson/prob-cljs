(ns prob.erp
  "Elementary Random Primitives (ERPs).
   Each ERP is trace-aware: when *trace-state* is bound (during MH),
   random choices are recorded in a persistent trace map and can be
   replayed from an existing trace."
  (:require [prob.math :as math]))

;; ---------------------------------------------------------------------------
;; Trace infrastructure
;; ---------------------------------------------------------------------------

(def ^:dynamic *trace-state*
  "When non-nil, a volatile! holding trace state for MH inference.
   State is a persistent map:
     :counter       - sequential address counter (int)
     :old-trace     - persistent map to replay from (addr -> entry)
     :proposal-addr - address being re-proposed (-1 for fresh)
     :new-trace     - persistent map being built (addr -> entry)
     :score         - accumulated log-weight from factor
     :mem-cache     - persistent map of [fn-id args] -> result for trace-aware mem"
  nil)

(defn trace-choice!
  "Record a traced random choice. Returns the value to use.
   erp-id:      keyword identifying the ERP type
   sample-fn:   zero-arg fn that samples a fresh value
   log-prob-fn: (fn [value] log-probability)
   domain:      (optional) vector of enumerable values, or nil for continuous
   dist:        (optional) distribution object for drift proposals"
  ([erp-id sample-fn log-prob-fn]
   (trace-choice! erp-id sample-fn log-prob-fn nil nil))
  ([erp-id sample-fn log-prob-fn domain]
   (trace-choice! erp-id sample-fn log-prob-fn domain nil))
  ([erp-id sample-fn log-prob-fn domain dist]
   (let [state @*trace-state*
         addr (:counter state)
         old-entry (get (:old-trace state) addr)
         ;; At proposal address with drift-capable dist: use propose-fn from state
         propose-fn (:propose-fn state)
         [drift-value drift-fwd drift-rev]
         (when (and propose-fn
                    (= addr (:proposal-addr state))
                    old-entry
                    dist)
           (propose-fn dist (:value old-entry)))
         value (cond
                 ;; Drift proposal was used
                 drift-value drift-value
                 ;; Proposal address: sample fresh
                 (= addr (:proposal-addr state))
                 (sample-fn)
                 ;; Replay from old trace if ERP type matches
                 (and old-entry (= (:erp-id old-entry) erp-id))
                 (:value old-entry)
                 ;; Otherwise: sample fresh
                 :else
                 (sample-fn))
         lp (log-prob-fn value)
         entry (cond-> {:value value :log-prob lp :erp-id erp-id}
                 dist (assoc :dist dist)
                 drift-fwd (assoc :fwd-lp drift-fwd :rev-lp drift-rev))]
     (vswap! *trace-state*
             (fn [s]
               (let [s (-> s
                           (update :counter inc)
                           (assoc-in [:new-trace addr] entry))]
                 (if (:enum-discovery s)
                   (update s :enum-domains conj
                           {:addr addr :domain domain :erp-id erp-id})
                   s))))
     value)))

;; ---------------------------------------------------------------------------
;; Core random
;; ---------------------------------------------------------------------------

(defn- rand-uniform []
  (js/Math.random))

;; ---------------------------------------------------------------------------
;; Raw (untraced) sampling — used internally by compound ERPs
;; (e.g. gamma uses raw-gaussian, beta uses raw-gamma)
;; ---------------------------------------------------------------------------

(defn- raw-flip [p]
  (< (rand-uniform) p))

(defn- raw-uniform [a b]
  (+ a (* (rand-uniform) (- b a))))

(defn- raw-uniform-draw [items]
  (nth items (js/Math.floor (* (rand-uniform) (count items)))))

(defn- raw-random-integer [n]
  (js/Math.floor (* (rand-uniform) n)))

(defn- raw-multinomial [items probs]
  (let [items (vec items)
        probs (vec probs)
        total (reduce + 0 probs)
        r (* (rand-uniform) total)]
    (loop [i 0, cumulative 0]
      (if (>= i (count items))
        (peek items)
        (let [cumulative (+ cumulative (nth probs i))]
          (if (< r cumulative)
            (nth items i)
            (recur (inc i) cumulative)))))))

(defn- raw-gaussian [mu sigma]
  (if (zero? sigma)
    mu
    (let [u1 (rand-uniform)
          u2 (rand-uniform)
          z (* (js/Math.sqrt (* -2 (js/Math.log u1)))
               (js/Math.cos (* 2 js/Math.PI u2)))]
      (+ mu (* sigma z)))))

(defn- raw-gamma [a b]
  (if (< a 1)
    (* (raw-gamma (inc a) b)
       (js/Math.pow (rand-uniform) (/ 1 a)))
    (let [d (- a (/ 1 3))
          c (/ 1 (js/Math.sqrt (* 9 d)))]
      (loop []
        (let [x (raw-gaussian 0 1)
              v (js/Math.pow (+ 1 (* c x)) 3)]
          (if (and (> v 0)
                   (< (js/Math.log (rand-uniform))
                      (+ (* 0.5 x x)
                         (* d (- 1 v (js/Math.log v))))))
            (* d v b)
            (recur)))))))

(defn- raw-beta [a b]
  (let [x (raw-gamma a 1)
        y (raw-gamma b 1)]
    (/ x (+ x y))))

(defn- raw-dirichlet [alpha]
  (let [samples (mapv #(raw-gamma % 1) alpha)
        total (reduce + 0 samples)]
    (mapv #(/ % total) samples)))

(defn- raw-exponential [rate]
  (/ (- (js/Math.log (rand-uniform))) rate))

(defn- raw-binomial [n p]
  (loop [i 0, successes 0]
    (if (>= i n) successes
      (recur (inc i) (if (raw-flip p) (inc successes) successes)))))

(defn- raw-poisson [lambda]
  ;; Knuth's algorithm
  (let [l (js/Math.exp (- lambda))]
    (loop [k 0, p 1.0]
      (let [p (* p (rand-uniform))]
        (if (< p l) k (recur (inc k) p))))))

(defn- raw-categorical-map [weight-map]
  (let [items (keys weight-map)
        weights (vals weight-map)]
    (raw-multinomial (vec items) (vec weights))))

(defn- raw-categorical-vecs [categories weights]
  (raw-multinomial (vec categories) (vec weights)))

;; ---------------------------------------------------------------------------
;; Log-probability functions
;; ---------------------------------------------------------------------------

(def ^:private log-2pi (js/Math.log (* 2.0 js/Math.PI)))

(defn- flip-lp [p v]
  (if v (js/Math.log p) (js/Math.log (- 1.0 p))))

(defn- gaussian-lp [mu sigma x]
  (if (zero? sigma)
    (if (== x mu) 0.0 ##-Inf)
    (let [z (/ (- x mu) sigma)]
      (* -0.5 (+ log-2pi
                 (* 2.0 (js/Math.log sigma))
                 (* z z))))))

(defn- uniform-lp [a b x]
  (cond
    (== a b) (if (== x a) 0.0 ##-Inf)
    (or (< x a) (> x b)) ##-Inf
    :else (- (js/Math.log (- b a)))))

(defn- uniform-draw-lp [items x]
  (let [n (count items)
        k (count (filter #(= % x) items))]
    (if (zero? k) ##-Inf (js/Math.log (/ (double k) (double n))))))

(defn- random-integer-lp [n x]
  (if (and (integer? x) (>= x 0) (< x n))
    (- (js/Math.log n))
    ##-Inf))

(defn- multinomial-lp [items probs x]
  (let [total (reduce + 0.0 probs)
        value-prob (reduce + 0.0
                     (map (fn [item prob]
                            (if (= item x) prob 0.0))
                          items probs))]
    (if (zero? value-prob) ##-Inf (js/Math.log (/ value-prob total)))))

(defn- sample-discrete-lp [weights idx]
  (if (or (not (integer? idx)) (neg? idx) (>= idx (count weights)))
    ##-Inf
    (let [total (reduce + 0.0 weights)]
      (js/Math.log (/ (nth weights idx) total)))))

(defn- gamma-lp [shape scale x]
  (if (<= x 0)
    ##-Inf
    (+ (* (dec shape) (js/Math.log x))
       (- (/ x scale))
       (- (* shape (js/Math.log scale)))
       (- (math/log-gamma-fn shape)))))

(defn- beta-lp [a b x]
  (if (or (< x 0) (> x 1))
    ##-Inf
    (let [mul-log (fn [c v] (if (zero? c) 0.0 (* c (js/Math.log v))))]
      (+ (mul-log (dec a) x)
         (mul-log (dec b) (- 1.0 x))
         (- (math/log-beta-fn a b))))))

(defn- exponential-lp [rate x]
  (if (< x 0)
    ##-Inf
    (- (js/Math.log rate) (* rate x))))

(defn- binomial-lp [n p k]
  (if (or (not (integer? k)) (neg? k) (> k n))
    ##-Inf
    (+ (- (math/log-fact n) (math/log-fact k) (math/log-fact (- n k)))
       (if (pos? k) (* k (js/Math.log p)) 0.0)
       (if (< k n) (* (- n k) (js/Math.log (- 1.0 p))) 0.0))))

(defn- poisson-lp [lambda k]
  (if (or (not (integer? k)) (neg? k))
    ##-Inf
    (- (* k (js/Math.log lambda)) lambda (math/log-fact k))))

(defn- categorical-lp [categories weights x]
  (let [total (reduce + 0.0 weights)
        value-prob (reduce + 0.0
                     (map (fn [cat w]
                            (if (= cat x) w 0.0))
                          categories weights))]
    (if (zero? value-prob) ##-Inf (js/Math.log (/ value-prob total)))))

(defn- dirichlet-lp [alpha x]
  (let [x (vec x)]
    (if (or (not= (count x) (count alpha))
            (some #(< % 0) x))
      ##-Inf
      (let [mul-log (fn [c v] (if (zero? c) 0.0 (* c (js/Math.log v))))]
        (+ (math/log-gamma-fn (reduce + alpha))
           (- (reduce + (map math/log-gamma-fn alpha)))
           (reduce + (map (fn [ai xi] (mul-log (dec ai) xi))
                          alpha x)))))))

;; ---------------------------------------------------------------------------
;; Public ERPs (trace-aware)
;; ---------------------------------------------------------------------------

(defn flip
  ([] (flip 0.5))
  ([p]
   (if *trace-state*
     (trace-choice! :flip #(raw-flip p) #(flip-lp p %) [true false])
     (raw-flip p))))

(defn uniform [a b]
  (if *trace-state*
    (trace-choice! :uniform #(raw-uniform a b) #(uniform-lp a b %))
    (raw-uniform a b)))

(defn uniform-draw [lst]
  (let [items (vec lst)]
    (if *trace-state*
      (trace-choice! :uniform-draw
        #(raw-uniform-draw items)
        #(uniform-draw-lp items %)
        (vec (distinct items)))
      (raw-uniform-draw items))))

(defn random-integer [n]
  (if *trace-state*
    (trace-choice! :random-integer
      #(raw-random-integer n)
      #(random-integer-lp n %)
      (vec (range n)))
    (raw-random-integer n)))

(defn multinomial [items probs]
  (let [items (vec items)
        probs (vec probs)]
    (if *trace-state*
      (trace-choice! :multinomial
        #(raw-multinomial items probs)
        #(multinomial-lp items probs %)
        (vec (distinct items)))
      (raw-multinomial items probs))))

(defn sample-discrete [weights]
  (let [weights (vec weights)
        indices (vec (range (count weights)))]
    (if *trace-state*
      (trace-choice! :sample-discrete
        #(raw-multinomial indices weights)
        #(sample-discrete-lp weights %)
        indices)
      (raw-multinomial indices weights))))

(defn gaussian
  ([] (gaussian 0 1))
  ([mu sigma]
   (if *trace-state*
     (trace-choice! :gaussian
       #(raw-gaussian mu sigma)
       #(gaussian-lp mu sigma %))
     (raw-gaussian mu sigma))))

(defn gamma [a b]
  (if *trace-state*
    (trace-choice! :gamma #(raw-gamma a b) #(gamma-lp a b %))
    (raw-gamma a b)))

(defn beta [a b]
  (if *trace-state*
    (trace-choice! :beta #(raw-beta a b) #(beta-lp a b %))
    (raw-beta a b)))

(defn dirichlet [alpha-vec]
  (let [alpha (vec alpha-vec)]
    (if *trace-state*
      (trace-choice! :dirichlet
        #(raw-dirichlet alpha)
        #(dirichlet-lp alpha %))
      (raw-dirichlet alpha))))

(defn exponential [rate]
  (if *trace-state*
    (trace-choice! :exponential
      #(raw-exponential rate)
      #(exponential-lp rate %))
    (raw-exponential rate)))

(defn binomial [n p]
  (if *trace-state*
    (trace-choice! :binomial
      #(raw-binomial n p)
      #(binomial-lp n p %)
      (vec (range (inc n))))
    (raw-binomial n p)))

(defn poisson [lambda]
  (if *trace-state*
    (trace-choice! :poisson
      #(raw-poisson lambda)
      #(poisson-lp lambda %))
    (raw-poisson lambda)))

(defn categorical
  "Weighted discrete choice. Accepts either a map {category weight ...}
   or two sequences [categories] [weights]."
  ([weight-map]
   (let [cats (vec (keys weight-map))
         ws (vec (vals weight-map))]
     (if *trace-state*
       (trace-choice! :categorical
         #(raw-categorical-map weight-map)
         #(categorical-lp cats ws %)
         cats)
       (raw-categorical-map weight-map))))
  ([categories weights]
   (let [cats (vec categories)
         ws (vec weights)]
     (if *trace-state*
       (trace-choice! :categorical
         #(raw-categorical-vecs cats ws)
         #(categorical-lp cats ws %)
         (vec (distinct cats)))
       (raw-categorical-vecs cats ws)))))
