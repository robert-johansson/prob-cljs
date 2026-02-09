(ns prob.builtins
  "Built-in functions for probabilistic programming.
   Includes list operations, math, string ops, comparisons, and more.
   These are available for direct use or re-export through prob.core.")

(declare equal?)

;; ---------------------------------------------------------------------------
;; Pair type (for non-list cons cells, e.g. (pair 1 2))
;; ---------------------------------------------------------------------------

(defn- dotted-pair? [x]
  (and (map? x) (= (:__type x) :pair)))

(defn pair [a b]
  (if (or (seq? b) (vector? b) (list? b) (nil? b))
    (cons a b)
    {:__type :pair :car a :cdr b}))

(defn pair? [x]
  (boolean
    (or (dotted-pair? x)
        (and (seq? x) (seq x))
        (and (vector? x) (seq x)))))

(defn car [x]
  (cond
    (dotted-pair? x) (:car x)
    (seq? x) (first x)
    (vector? x) (first x)
    :else (throw (ex-info "car: not a pair" {:val x}))))

(defn cdr [x]
  (cond
    (dotted-pair? x) (:cdr x)
    (and (seq? x) (= (count (rest x)) 0)) '()
    (seq? x) (rest x)
    (and (vector? x) (= (count (rest x)) 0)) '()
    (vector? x) (rest x)
    :else (throw (ex-info "cdr: not a pair" {:val x}))))

;; ---------------------------------------------------------------------------
;; List operations
;; ---------------------------------------------------------------------------

(defn null? [x]
  (and (or (seq? x) (vector? x) (list? x) (nil? x))
       (empty? (seq x))))

(defn list? [x]
  (or (and (clojure.core/seq? x) true)
      (and (vector? x) true)
      (nil? x)
      (and (clojure.core/list? x) true)
      false))

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
  (.indexOf (vec lst) x))

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
      (and (dotted-pair? x) (instance? Pair y))
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
  (let [cache (atom {})]
    (fn [& args]
      (if-let [cached (find @cache args)]
        (val cached)
        (let [result (apply f args)]
          (swap! cache assoc args result)
          result)))))

;; ---------------------------------------------------------------------------
;; Gensym
;; ---------------------------------------------------------------------------

(defn make-gensym
  ([] (make-gensym "g"))
  ([prefix]
   (let [counter (atom 0)]
     (fn []
       (let [n @counter]
         (swap! counter inc)
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
