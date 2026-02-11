(ns prob.builtins
  "Built-in functions for probabilistic programming.
   Includes list operations, math, string ops, comparisons, and more.
   These are available for direct use or re-export through prob.core."
  (:require [prob.erp :as erp]))

(declare equal?)

;; ---------------------------------------------------------------------------
;; Pair type (for non-list cons cells, e.g. (pair 1 2))
;; ---------------------------------------------------------------------------

(defrecord Pair [car cdr])

(defn pair [a b]
  (if (or (seq? b) (vector? b) (list? b) (nil? b))
    (cons a b)
    (->Pair a b)))

(defn pair? [x]
  (boolean
    (or (instance? Pair x)
        (and (seq? x) (seq x))
        (and (vector? x) (seq x)))))

(defn car [x]
  (cond
    (instance? Pair x) (:car x)
    (seq? x) (first x)
    (vector? x) (first x)
    :else (throw (ex-info "car: not a pair" {:val x}))))

(defn cdr [x]
  (cond
    (instance? Pair x) (:cdr x)
    (and (seq? x) (empty? (rest x))) '()
    (seq? x) (rest x)
    (and (vector? x) (empty? (rest x))) '()
    (vector? x) (rest x)
    :else (throw (ex-info "cdr: not a pair" {:val x}))))

;; ---------------------------------------------------------------------------
;; List operations
;; ---------------------------------------------------------------------------

(defn null? [x]
  (and (or (seq? x) (vector? x) (list? x) (nil? x))
       (empty? (seq x))))

(defn list? [x]
  (or (seq? x) (vector? x) (nil? x)))

(defn length [lst]
  (count (seq lst)))

(defn append [& lsts]
  (apply concat lsts))

(defn list-ref [lst n]
  (nth (vec lst) n))

(defn list-elt [lst n]
  (list-ref lst (dec n)))

(defn take-n [lst n]
  (take n lst))

(defn drop-n [lst n]
  (drop n lst))

(defn second-elem [lst] (nth (vec lst) 1))
(defn third-elem [lst] (nth (vec lst) 2))
(defn fourth-elem [lst] (nth (vec lst) 3))
(defn fifth-elem [lst] (nth (vec lst) 4))
(defn sixth-elem [lst] (nth (vec lst) 5))
(defn seventh-elem [lst] (nth (vec lst) 6))
(defn last-elem [lst] (last lst))
(defn but-last [lst] (butlast lst))

(defn make-list [n x]
  (repeat n x))

(defn for-each [f lst]
  (doseq [x lst] (f x))
  nil)

(defn zip [& lsts]
  (apply map clojure.core/list lsts))

(defn sort-list
  ([lst] (sort lst))
  ([lst cmp] (sort cmp lst)))

(defn partition-list [pred lst]
  (let [grouped (group-by (comp boolean pred) lst)]
    (clojure.core/list (or (get grouped true) '())
                       (or (get grouped false) '()))))

(defn unique
  ([lst] (unique lst equal?))
  ([lst eq]
   (reduce (fn [acc x]
             (if (some #(eq x %) acc)
               acc
               (conj acc x)))
           []
           lst)))

(defn nub [lst]
  (distinct lst))

(defn range-inclusive [start end]
  (range start (inc end)))

(defn iota
  ([count] (iota count 0 1))
  ([count start] (iota count start 1))
  ([count start step]
   (take count (iterate #(+ % step) start))))

(defn update-list [lst n value]
  (let [v (vec lst)]
    (assoc v n value)))

(defn list-index [lst x]
  (.indexOf (to-array lst) x))

(defn repeat-fn [n f]
  (repeatedly n f))

(defn fold [f init & lsts]
  (let [arrays (map vec lsts)
        n (apply min (map count arrays))]
    (loop [i 0, acc init]
      (if (>= i n)
        acc
        (let [args (conj (vec (map #(nth % i) arrays)) acc)]
          (recur (inc i) (apply f args)))))))

(defn foldl [f init lst]
  (reduce (fn [acc v] (f acc v)) init lst))

(defn foldr [f init lst]
  (reduce (fn [acc v] (f acc v)) init (reverse lst)))

;; ---------------------------------------------------------------------------
;; Set operations
;; ---------------------------------------------------------------------------

(defn union [& lsts]
  (vec (distinct (apply concat lsts))))

(defn intersection [& lsts]
  (if (empty? lsts)
    '()
    (reduce (fn [a b]
              (let [s (set b)]
                (distinct (filter s a))))
            lsts)))

(defn difference [& lsts]
  (if (<= (count lsts) 1)
    (first lsts)
    (let [excl (set (apply concat (rest lsts)))]
      (remove excl (first lsts)))))

;; ---------------------------------------------------------------------------
;; Math
;; ---------------------------------------------------------------------------

(defn mod' [x y] (mod x y))
(defn round [x] (js/Math.round x))
(defn floor [x] (js/Math.floor x))
(defn ceil [x] (js/Math.ceil x))
(defn abs [x] (js/Math.abs x))
(defn log [x] (js/Math.log x))
(defn exp [x] (js/Math.exp x))
(defn expt [x y] (js/Math.pow x y))
(defn sqrt [x] (js/Math.sqrt x))
(defn sin [x] (js/Math.sin x))
(defn cos [x] (js/Math.cos x))
(defn tan [x] (js/Math.tan x))
(defn asin [x] (js/Math.asin x))
(defn acos [x] (js/Math.acos x))
(defn atan [x] (js/Math.atan x))
(defn atan2 [y x] (js/Math.atan2 y x))

(defn sum [lst]
  (reduce + 0 lst))

(defn prod [lst]
  (reduce * 1 lst))

(defn mean [lst]
  (let [a (vec lst)
        n (count a)]
    (/ (reduce + 0 a) n)))

(defn variance [lst]
  (let [a (vec lst)
        n (count a)
        m (/ (reduce + 0 a) n)]
    (/ (reduce + 0 (map #(js/Math.pow (- % m) 2) a)) n)))

(defn sd [lst]
  (js/Math.sqrt (variance lst)))

(defn weighted-mean [values weights]
  (let [vs (vec values) ws (vec weights)
        tw (reduce + 0.0 ws)]
    (if (zero? tw) 0.0
      (/ (reduce + 0.0 (map * vs ws)) tw))))

(defn weighted-variance [values weights]
  (let [vs (vec values) ws (vec weights)
        tw (reduce + 0.0 ws)
        mu (weighted-mean vs ws)]
    (if (zero? tw) 0.0
      (/ (reduce + 0.0 (map (fn [v w] (* w (js/Math.pow (- v mu) 2))) vs ws)) tw))))

(defn mode
  "Most frequent value in a collection. If tied, returns one of the modes."
  [lst]
  (key (apply max-key val (frequencies lst))))

(defn empirical-distribution [samples]
  (let [n (count samples)]
    (into {} (map (fn [[k v]] [k (/ (double v) n)]) (frequencies samples)))))

(defn expectation
  ([samples] (mean samples))
  ([samples f] (mean (map f samples))))

;; ---------------------------------------------------------------------------
;; Comparison
;; ---------------------------------------------------------------------------

(defn eq? [x y]
  (cond
    (and (symbol? x) (symbol? y)) (= (name x) (name y))
    (and (string? x) (string? y)) (= x y)
    (not= (type x) (type y)) false
    :else (identical? x y)))

(defn- ->comparable [x]
  (if (symbol? x) (name x) x))

(defn equal? [x y]
  (let [x' (->comparable x)
        y' (->comparable y)]
    (cond
      (and (number? x') (number? y')) (== x' y')
      (and (string? x') (string? y')) (= x' y')
      (and (boolean? x') (boolean? y')) (= x' y')
      (and (or (seq? x) (vector? x)) (or (seq? y) (vector? y)))
      (let [xs (seq x) ys (seq y)]
        (and (= (count xs) (count ys))
             (every? true? (map equal? xs ys))))
      (and (instance? Pair x) (instance? Pair y))
      (and (equal? (:car x) (:car y))
           (equal? (:cdr x) (:cdr y)))
      (and (nil? x) (nil? y)) true
      :else (= x' y'))))

(defn soft-equal [y x tol]
  (and (> y (- x tol)) (< y (+ x tol))))

(defn member
  ([x lst] (member x lst equal?))
  ([x lst cmp]
   (loop [remaining (seq lst)]
     (cond
       (nil? remaining) false
       (cmp x (first remaining)) remaining
       :else (recur (next remaining))))))

(defn assoc-list [x alist]
  (loop [remaining (seq alist)]
    (cond
      (nil? remaining) false
      (equal? (car (first remaining)) x) (first remaining)
      :else (recur (next remaining)))))

;; ---------------------------------------------------------------------------
;; Type predicates
;; ---------------------------------------------------------------------------

(defn number?' [x] (number? x))
(defn string?' [x] (string? x))
(defn boolean?' [x] (boolean? x))
(defn procedure? [x] (fn? x))

;; ---------------------------------------------------------------------------
;; String operations
;; ---------------------------------------------------------------------------

(defn string-append [& args]
  (apply str args))

(defn string-length [s]
  (count s))

(defn stringify [x]
  (str x))

(defn string-split [s sep]
  (seq (.split s sep)))

(defn string-slice
  ([s start] (.slice s start))
  ([s start end] (.slice s start end)))

(defn string->number [s]
  (let [x (js/parseFloat s)]
    (if (js/isNaN x) false x)))

(defn number->string [x]
  (str x))

(defn symbol->string [s]
  (if (symbol? s) (name s) (str s)))

(defn string->symbol [s]
  (symbol s))

(defn boolean->number [b]
  (if b 1 0))

(defn number->boolean [x]
  (not (zero? x)))

;; ---------------------------------------------------------------------------
;; Higher-order
;; ---------------------------------------------------------------------------

(defn apply-fn [f lst]
  (apply f (seq lst)))

(defn compose [& fns]
  (apply comp fns))

(defn identity-fn [x] x)

;; ---------------------------------------------------------------------------
;; Memoization
;; ---------------------------------------------------------------------------

(defn mem [f]
  (let [fn-id (gensym "mem")
        local-cache (volatile! {})]
    (fn [& args]
      (if erp/*trace-state*
        ;; Trace-aware: cache in :mem-cache of trace state (rolls back on MH reject)
        (let [key [fn-id args]
              state @erp/*trace-state*
              cached (get (:mem-cache state) key ::not-found)]
          (if (not= cached ::not-found)
            cached
            (let [result (apply f args)]
              (vswap! erp/*trace-state* assoc-in [:mem-cache key] result)
              result)))
        ;; Outside inference: closure-local volatile cache
        (let [cached (get @local-cache args ::not-found)]
          (if (not= cached ::not-found)
            cached
            (let [result (apply f args)]
              (vswap! local-cache assoc args result)
              result)))))))

;; ---------------------------------------------------------------------------
;; Cache (LRU-bounded deterministic memoization)
;; ---------------------------------------------------------------------------

(defn cache
  "LRU-bounded deterministic memoization.
   (cache f) uses default max-size of 1000. (cache f n) uses max-size n."
  ([f] (cache f 1000))
  ([f max-size]
   (let [store (volatile! {})
         order (volatile! [])]
     (fn [& args]
       (let [cached (get @store args ::not-found)]
         (if (not= cached ::not-found)
           ;; Hit: move to end of access order
           (do
             (vswap! order (fn [o] (conj (vec (remove #(= % args) o)) args)))
             cached)
           ;; Miss: compute, store, evict if needed
           (let [result (apply f args)]
             (vswap! store assoc args result)
             (vswap! order conj args)
             (when (> (count @order) max-size)
               (let [oldest (first @order)]
                 (vswap! store dissoc oldest)
                 (vswap! order (fn [o] (vec (rest o))))))
             result)))))))

;; ---------------------------------------------------------------------------
;; DPmem (Dirichlet Process memoization)
;; ---------------------------------------------------------------------------

(defn DPmem
  "Dirichlet Process memoization. Returns a memoized function.
   alpha is the concentration parameter. f is the function to memoize.
   Uses Chinese Restaurant Process for table selection."
  [alpha f]
  (let [dp-id (gensym "dp")
        local-state (volatile! {})]
    (fn [& args]
      (let [key [dp-id (vec args)]
            state (if erp/*trace-state*
                    (get (:mem-cache @erp/*trace-state*) key)
                    (get @local-state args))
            {:keys [tables counts]
             :or {tables [] counts []}} state
            n (reduce + 0 counts)
            ;; CRP probabilities: existing tables proportional to count,
            ;; new table proportional to alpha
            crp-items (conj (vec (range (count tables))) :new)
            crp-probs (conj (vec (map double counts)) (double alpha))
            choice (erp/multinomial crp-items crp-probs)
            [result new-state]
            (if (= choice :new)
              ;; Seat at new table: call f to get result
              (let [r (apply f args)]
                [r {:tables (conj tables r)
                    :counts (conj counts 1)}])
              ;; Seat at existing table
              [(nth tables choice)
               {:tables tables
                :counts (update counts choice inc)}])]
        (if erp/*trace-state*
          (vswap! erp/*trace-state* assoc-in [:mem-cache key] new-state)
          (vswap! local-state assoc args new-state))
        result))))

;; ---------------------------------------------------------------------------
;; Gensym
;; ---------------------------------------------------------------------------

(defn make-gensym
  ([] (make-gensym "g"))
  ([prefix]
   (let [counter (volatile! 0)]
     (fn []
       (let [n @counter]
         (vswap! counter inc)
         (symbol (str prefix n)))))))

(def ^:private default-gensym (make-gensym "g"))

(defn gensym' [] (default-gensym))

;; ---------------------------------------------------------------------------
;; I/O
;; ---------------------------------------------------------------------------

(defn display [& args]
  (apply println args))

;; ---------------------------------------------------------------------------
;; Misc
;; ---------------------------------------------------------------------------

(defn error [& args]
  (throw (ex-info (apply str args) {})))

(defn sample [thunk] (thunk))

(defn get-time [] (js/Date.now))
