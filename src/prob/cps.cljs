(ns prob.cps
  "CPS transformation for Sequential Monte Carlo inference.

   Checkpoint records represent paused computations:
   - Sample: particle wants to sample from a distribution
   - Observe: particle hit an observation point
   - Factor: particle accumulated a log-weight
   - Result: particle finished execution

   cps-of-expr transforms model code so that at each probabilistic
   operation, execution yields a checkpoint record containing the
   continuation. The SMC driver processes these checkpoints across
   all particles."
  (:require [prob.dist :as dist]
            [prob.erp :as erp]))

;; ---------------------------------------------------------------------------
;; Checkpoint records
;; ---------------------------------------------------------------------------

(defrecord Sample [addr dist cont state])
(defrecord Observe [dist value cont state])
(defrecord Factor [score cont state])
(defrecord Result [value state])

(defn sample? [x] (instance? Sample x))
(defn observe? [x] (instance? Observe x))
(defn factor? [x] (instance? Factor x))
(defn result? [x] (instance? Result x))
(defn checkpoint? [x] (or (sample? x) (observe? x) (factor? x) (result? x)))

;; ---------------------------------------------------------------------------
;; CPS runtime helpers (called by generated code)
;; ---------------------------------------------------------------------------

(defn cps-sample
  "CPS version of sample*. Emits a Sample checkpoint.
   The SMC driver will draw from dist and call cont with the value."
  [dist cont state]
  (let [addr (:addr state 0)
        state (update state :addr (fnil inc 0))]
    (->Sample addr dist cont state)))

(defn cps-observe
  "CPS version of observe. Emits an Observe checkpoint.
   The SMC driver will accumulate the log-weight and call cont."
  [dist value cont state]
  (->Observe dist value cont state))

(defn cps-factor
  "CPS version of factor. Emits a Factor checkpoint."
  [score cont state]
  (->Factor score cont state))

(defn cps-condition
  "CPS version of condition. Hard constraint as -Infinity weight."
  [pred cont state]
  (if pred
    (cont nil state)
    (->Factor ##-Inf cont state)))

;; ---------------------------------------------------------------------------
;; CPS form transformer
;;
;; Pure function: form -> form. Runs at macro expansion time.
;; Takes a Clojure/Script form and a continuation symbol, returns
;; CPS-transformed code.
;;
;; All emitted symbols are fully qualified to avoid namespace
;; resolution issues in both Clojure macros and SCI macros.
;; ---------------------------------------------------------------------------

(def ^:private probabilistic-ops
  "Set of symbols that become checkpoints in CPS."
  #{'sample* 'prob.core/sample* 'prob.dist/sample*
    'observe 'prob.core/observe 'prob.inference/observe
    'factor 'prob.core/factor 'prob.inference/factor
    'condition 'prob.core/condition 'prob.inference/condition
    'sample 'prob.builtins/sample})

(def ^:private erp-to-dist
  "Map from ERP function names to distribution constructors.
   When an ERP is called inside CPS code, we convert it to
   (sample* (dist-constructor args...))."
  {'flip         'prob.dist/bernoulli-dist
   'gaussian     'prob.dist/gaussian-dist
   'uniform      'prob.dist/uniform-dist
   'beta         'prob.dist/beta-dist
   'gamma        'prob.dist/gamma-dist
   'exponential  'prob.dist/exponential-dist
   'dirichlet    'prob.dist/dirichlet-dist
   'uniform-draw 'prob.dist/uniform-draw-dist
   'random-integer 'prob.dist/random-integer-dist
   'multinomial  'prob.dist/multinomial-dist
   'sample-discrete 'prob.dist/sample-discrete-dist
   'binomial     'prob.dist/binomial-dist
   'poisson      'prob.dist/poisson-dist
   'categorical  'prob.dist/categorical-dist
   ;; Qualified variants
   'prob.core/flip         'prob.dist/bernoulli-dist
   'prob.core/gaussian     'prob.dist/gaussian-dist
   'prob.core/uniform      'prob.dist/uniform-dist
   'prob.core/beta         'prob.dist/beta-dist
   'prob.core/gamma        'prob.dist/gamma-dist
   'prob.core/exponential  'prob.dist/exponential-dist
   'prob.core/dirichlet    'prob.dist/dirichlet-dist
   'prob.core/uniform-draw 'prob.dist/uniform-draw-dist
   'prob.core/random-integer 'prob.dist/random-integer-dist
   'prob.core/multinomial  'prob.dist/multinomial-dist
   'prob.core/sample-discrete 'prob.dist/sample-discrete-dist
   'prob.core/binomial     'prob.dist/binomial-dist
   'prob.core/poisson      'prob.dist/poisson-dist
   'prob.core/categorical  'prob.dist/categorical-dist
   'prob.erp/flip         'prob.dist/bernoulli-dist
   'prob.erp/gaussian     'prob.dist/gaussian-dist
   'prob.erp/uniform      'prob.dist/uniform-dist
   'prob.erp/beta         'prob.dist/beta-dist
   'prob.erp/gamma        'prob.dist/gamma-dist
   'prob.erp/exponential  'prob.dist/exponential-dist
   'prob.erp/dirichlet    'prob.dist/dirichlet-dist
   'prob.erp/uniform-draw 'prob.dist/uniform-draw-dist
   'prob.erp/random-integer 'prob.dist/random-integer-dist
   'prob.erp/multinomial  'prob.dist/multinomial-dist
   'prob.erp/sample-discrete 'prob.dist/sample-discrete-dist
   'prob.erp/binomial     'prob.dist/binomial-dist
   'prob.erp/poisson      'prob.dist/poisson-dist
   'prob.erp/categorical  'prob.dist/categorical-dist})

(defn- fresh-sym
  "Generate a unique symbol for CPS bindings."
  [prefix]
  (gensym (str prefix "__")))

(defn- atomic?
  "Is this form a literal or symbol (no sub-expressions to CPS)?"
  [form]
  (or (nil? form)
      (boolean? form)
      (number? form)
      (string? form)
      (keyword? form)
      (symbol? form)))

(defn- erp-call?
  "Is this a call to an ERP function that should become sample*?"
  [form]
  (and (seq? form) (symbol? (first form)) (contains? erp-to-dist (first form))))

(defn- prob-op?
  "Is this a call to a probabilistic operation?"
  [form]
  (and (seq? form) (symbol? (first form)) (contains? probabilistic-ops (first form))))

(defn- special-form?
  "Is this a special form we handle explicitly?"
  [sym]
  (contains? #{'let 'if 'do 'fn 'fn* 'when 'when-not 'cond 'case 'and 'or
               'quote 'def 'loop 'recur 'letfn} sym))

(declare cps-of-expr)

(defn- cps-of-args
  "CPS-transform a list of argument expressions, then call body-fn
   with the list of result symbols. Threads state through.
   body-fn: (fn [arg-syms] -> form)"
  [args body-fn]
  (if (empty? args)
    (body-fn [])
    (let [process (fn process [remaining acc]
                    (if (empty? remaining)
                      (body-fn (vec acc))
                      (let [arg (first remaining)
                            rest-args (rest remaining)]
                        (if (atomic? arg)
                          (process rest-args (conj acc arg))
                          (let [s (fresh-sym "a")]
                            (cps-of-expr arg
                              (list 'fn [s '$state]
                                (process rest-args (conj acc s)))))))))]
      (process args []))))

(defn- cps-of-let
  "CPS transform of (let [bindings...] body...)."
  [bindings body k]
  (if (empty? bindings)
    (cps-of-expr (cons 'do body) k)
    (let [sym (first bindings)
          expr (second bindings)
          rest-bindings (drop 2 bindings)]
      (cps-of-expr expr
        (list 'fn [sym '$state]
          (cps-of-let (vec rest-bindings) body k))))))

(defn- cps-of-do
  "CPS transform of (do e1 e2 ... en). Last expression gets the continuation."
  [exprs k]
  (cond
    (empty? exprs) (list k nil '$state)
    (= 1 (count exprs)) (cps-of-expr (first exprs) k)
    :else
    (let [ignore (fresh-sym "_")]
      (cps-of-expr (first exprs)
        (list 'fn [ignore '$state]
          (cps-of-do (rest exprs) k))))))

(defn- cps-of-if
  "CPS transform of (if test then else)."
  [test then else k]
  (if (atomic? test)
    (list 'if test
      (cps-of-expr then k)
      (cps-of-expr else k))
    (let [t (fresh-sym "t")]
      (cps-of-expr test
        (list 'fn [t '$state]
          (list 'if t
            (cps-of-expr then k)
            (cps-of-expr else k)))))))

(defn- cps-of-fn
  "CPS transform of (fn [args] body).
   The CPS version takes an extra continuation and state arg."
  [params body k]
  (let [inner-k (fresh-sym "k")]
    (list k
      (list 'fn (vec (concat params [inner-k '$state]))
        (cps-of-expr (cons 'do body) inner-k))
      '$state)))

(defn- cps-of-sample
  "CPS transform of (sample* dist) or (sample dist).
   Emits a Sample checkpoint."
  [dist-expr k]
  (if (atomic? dist-expr)
    (list 'prob.cps/cps-sample dist-expr k '$state)
    (let [d (fresh-sym "d")]
      (cps-of-expr dist-expr
        (list 'fn [d '$state]
          (list 'prob.cps/cps-sample d k '$state))))))

(defn- cps-of-observe
  "CPS transform of (observe dist value).
   Emits an Observe checkpoint."
  [dist-expr value-expr k]
  (cps-of-args [dist-expr value-expr]
    (fn [[d v]]
      (list 'prob.cps/cps-observe d v k '$state))))

(defn- cps-of-factor
  "CPS transform of (factor score).
   Emits a Factor checkpoint."
  [score-expr k]
  (if (atomic? score-expr)
    (list 'prob.cps/cps-factor score-expr k '$state)
    (let [s (fresh-sym "s")]
      (cps-of-expr score-expr
        (list 'fn [s '$state]
          (list 'prob.cps/cps-factor s k '$state))))))

(defn- cps-of-condition
  "CPS transform of (condition pred).
   Hard constraint: false → -Infinity weight."
  [pred-expr k]
  (if (atomic? pred-expr)
    (list 'prob.cps/cps-condition pred-expr k '$state)
    (let [p (fresh-sym "p")]
      (cps-of-expr pred-expr
        (list 'fn [p '$state]
          (list 'prob.cps/cps-condition p k '$state))))))

(defn- cps-of-erp
  "CPS transform of a direct ERP call like (flip 0.7).
   Converts to (sample* (bernoulli-dist 0.7))."
  [op args k]
  (let [dist-ctor (get erp-to-dist op)]
    (cps-of-args args
      (fn [evaled-args]
        (list 'prob.cps/cps-sample
          (cons dist-ctor evaled-args)
          k '$state)))))

(def ^:dynamic *loop-fn*
  "When non-nil, the symbol of the current enclosing loop's CPS function.
   Used by cps-of-expr to handle recur."
  nil)

(defn- cps-of-loop
  "CPS transform of (loop [bindings...] body).
   Creates a named CPS function that recur calls back into.
   Binds *loop-fn* so that recur inside the body generates a
   direct CPS call to the loop function.

   (loop [x init-x, y init-y] body)
   →
   (let [loop-fn (fn loop-fn [x y k $state] <CPS of body>)]
     <CPS of init-x then init-y, then call loop-fn>)"
  [bindings body k]
  (let [pairs (partition 2 bindings)
        loop-vars (mapv first pairs)
        init-exprs (mapv second pairs)
        loop-sym (fresh-sym "loop")
        ;; Build the loop function body with *loop-fn* bound
        loop-fn-params (vec (concat loop-vars ['k '$state]))
        loop-fn-body (binding [*loop-fn* loop-sym]
                       (cps-of-expr (cons 'do body) 'k))
        loop-fn (list 'fn loop-sym loop-fn-params loop-fn-body)]
    ;; CPS-evaluate init exprs, then call the loop function
    (cps-of-args init-exprs
      (fn [evaled-inits]
        (list (list 'fn [loop-sym '$state]
                (concat (list loop-sym) evaled-inits (list k '$state)))
              loop-fn '$state)))))

(defn- cps-of-recur
  "CPS transform of (recur args...).
   Evaluates args in CPS, then calls the enclosing loop function directly."
  [args k]
  (when-not *loop-fn*
    (throw (ex-info "cps: recur outside of loop" {:args args})))
  (cps-of-args args
    (fn [evaled-args]
      (concat (list *loop-fn*) evaled-args (list k '$state)))))

(defn- cps-of-case
  "CPS transform of (case test-expr val1 result1 val2 result2 ... default?).
   Evaluates test-expr, then dispatches via case with each branch CPS'd."
  [test-expr clauses k]
  (if (atomic? test-expr)
    ;; Build case form with CPS'd branches
    (let [pairs (partition 2 clauses)
          has-default (odd? (count clauses))
          default (when has-default (last clauses))
          case-clauses (mapcat (fn [[val expr]]
                                 [val (cps-of-expr expr k)])
                               pairs)]
      (if has-default
        (concat (list 'case test-expr) case-clauses (list (cps-of-expr default k)))
        (concat (list 'case test-expr) case-clauses)))
    ;; Non-atomic test: evaluate first
    (let [t (fresh-sym "case")]
      (cps-of-expr test-expr
        (list 'fn [t '$state]
          (cps-of-case t clauses k))))))

(defn- cps-of-application
  "CPS transform of a function call (f arg1 arg2 ...).
   Assumes f is a primitive (non-CPS) function."
  [f args k]
  (cps-of-args (cons f args)
    (fn [evaled]
      (let [evaled-f (first evaled)
            evaled-args (rest evaled)]
        (list k (cons evaled-f evaled-args) '$state)))))

(defn cps-of-expr
  "Transform a form into CPS. k is a continuation (symbol or form).
   The transformed code calls (k result $state) when done.

   Handles: let, if, do, fn, when, when-not, cond, case, and, or,
   loop/recur, sample*/sample, observe, factor, condition, ERP calls,
   and primitive function application."
  [form k]
  (cond
    ;; nil, booleans, numbers, strings, keywords
    (or (nil? form) (boolean? form) (number? form) (string? form) (keyword? form))
    (list k form '$state)

    ;; Symbols
    (symbol? form)
    (list k form '$state)

    ;; Vectors — treat elements as expressions to evaluate
    (vector? form)
    (cps-of-args (seq form)
      (fn [evaled]
        (list k (vec evaled) '$state)))

    ;; Maps — treat as literal (keys and vals evaluated)
    (map? form)
    (let [ks (keys form)
          vs (vals form)]
      (cps-of-args vs
        (fn [evaled-vs]
          (list k (zipmap ks evaled-vs) '$state))))

    ;; Sets
    (set? form)
    (cps-of-args (seq form)
      (fn [evaled]
        (list k (set evaled) '$state)))

    ;; Quoted forms
    (and (seq? form) (= 'quote (first form)))
    (list k form '$state)

    ;; Sequences (function calls and special forms)
    (seq? form)
    (let [op (first form)]
      (cond
        ;; let / let*
        (or (= op 'let) (= op 'let*))
        (let [bindings (vec (second form))
              body (drop 2 form)]
          (cps-of-let bindings (vec body) k))

        ;; do
        (= op 'do)
        (cps-of-do (rest form) k)

        ;; if
        (= op 'if)
        (let [test (nth form 1)
              then (nth form 2)
              else (if (> (count form) 3) (nth form 3) nil)]
          (cps-of-if test then else k))

        ;; when -> if
        (= op 'when)
        (let [test (nth form 1)
              body (drop 2 form)]
          (cps-of-if test (cons 'do body) nil k))

        ;; when-not -> if
        (= op 'when-not)
        (let [test (nth form 1)
              body (drop 2 form)]
          (cps-of-if test nil (cons 'do body) k))

        ;; cond -> nested if
        (= op 'cond)
        (let [pairs (partition 2 (rest form))]
          (if (empty? pairs)
            (list k nil '$state)
            (let [desugar (fn desugar [pairs]
                            (if (empty? pairs)
                              nil
                              (let [[test expr] (first pairs)
                                    rest-pairs (rest pairs)]
                                (if (or (= test :else) (= test true))
                                  expr
                                  (list 'if test expr (desugar rest-pairs))))))]
              (cps-of-expr (desugar pairs) k))))

        ;; and -> if
        (= op 'and)
        (let [args (rest form)]
          (cond
            (empty? args) (list k true '$state)
            (= 1 (count args)) (cps-of-expr (first args) k)
            :else
            (let [desugar (fn desugar [args]
                            (if (= 1 (count args))
                              (first args)
                              (let [a (fresh-sym "and")]
                                (list 'let [a (first args)]
                                  (list 'if a (desugar (rest args)) a)))))]
              (cps-of-expr (desugar args) k))))

        ;; or -> if
        (= op 'or)
        (let [args (rest form)]
          (cond
            (empty? args) (list k nil '$state)
            (= 1 (count args)) (cps-of-expr (first args) k)
            :else
            (let [desugar (fn desugar [args]
                            (if (= 1 (count args))
                              (first args)
                              (let [a (fresh-sym "or")]
                                (list 'let [a (first args)]
                                  (list 'if a a (desugar (rest args)))))))]
              (cps-of-expr (desugar args) k))))

        ;; fn / fn*
        (or (= op 'fn) (= op 'fn*))
        (let [;; Handle (fn [args] body) and (fn name [args] body)
              has-name (symbol? (second form))
              params (if has-name (nth form 2) (second form))
              body (if has-name (drop 3 form) (drop 2 form))]
          (cps-of-fn params (vec body) k))

        ;; loop
        (= op 'loop)
        (let [bindings (vec (second form))
              body (drop 2 form)]
          (cps-of-loop bindings (vec body) k))

        ;; recur
        (= op 'recur)
        (cps-of-recur (rest form) k)

        ;; case
        (= op 'case)
        (let [test-expr (nth form 1)
              clauses (drop 2 form)]
          (cps-of-case test-expr (vec clauses) k))

        ;; sample* / sample
        (or (= op 'sample*) (= op 'prob.core/sample*)
            (= op 'prob.dist/sample*)
            (= op 'sample) (= op 'prob.builtins/sample))
        (cps-of-sample (second form) k)

        ;; observe
        (or (= op 'observe) (= op 'prob.core/observe)
            (= op 'prob.inference/observe))
        (cps-of-observe (nth form 1) (nth form 2) k)

        ;; factor
        (or (= op 'factor) (= op 'prob.core/factor)
            (= op 'prob.inference/factor))
        (cps-of-factor (second form) k)

        ;; condition
        (or (= op 'condition) (= op 'prob.core/condition)
            (= op 'prob.inference/condition))
        (cps-of-condition (second form) k)

        ;; Direct ERP calls → convert to sample* + dist constructor
        (erp-call? form)
        (cps-of-erp op (rest form) k)

        ;; Regular function application (primitive)
        :else
        (cps-of-application (first form) (rest form) k)))

    ;; Fallback: pass through
    :else
    (list k form '$state)))
