(ns prob.macros
  "Query macros for probabilistic programming.
   These wrap the body in a thunk and delegate to prob.core inference fns.

   Usage:
     (ns my-ns
       (:require [prob.core :refer [flip condition mean]])
       (:require-macros [prob.macros :refer [rejection-query mh-query enumeration-query]]))")

(defmacro rejection-query
  "Sample a single value from the conditional distribution defined by body.
   Use (condition expr) inside body to constrain.

   Example:
     (rejection-query
       (let [x (flip)]
         (condition x)
         x))
   ;=> true"
  [& body]
  `(prob.core/rejection-query-fn (fn [] ~@body)))

(defmacro mh-query
  "Generate n samples with given lag using Metropolis-Hastings.
   Returns a list of samples.

   Example:
     (mh-query 100 1
       (let [x (flip 0.7)]
         (condition x)
         x))
   ;=> (true true true ...)"
  [n lag & body]
  `(prob.core/mh-query-fn ~n ~lag (fn [] ~@body)))

(defmacro importance-query
  "Importance sampling with n samples.
   Returns (values probs).

   Example:
     (importance-query 1000
       (let [x (flip)]
         (factor (if x 0 -2))
         x))"
  [n & body]
  `(prob.core/importance-query-fn ~n (fn [] ~@body)))

(defmacro enumeration-query
  "Approximate the conditional distribution by enumeration.
   Returns (values probs).

   Example:
     (enumeration-query
       (let [x (flip)]
         (condition x)
         x))
   ;=> ((true) (1))"
  [& body]
  `(prob.core/enumeration-query-fn (fn [] ~@body)))

(defmacro mh-query-scored
  "Like mh-query but returns {:value :score} maps.

   Example:
     (mh-query-scored 100 1
       (let [x (flip 0.7)]
         (condition x)
         x))
   ;=> ({:value true :score ...} ...)"
  [n lag & body]
  `(prob.core/mh-query-scored-fn ~n ~lag (fn [] ~@body)))

(defmacro map-query
  "MAP inference: return the highest-scoring value from MH samples.

   Example:
     (map-query 500 1
       (let [x (uniform-draw [:a :b :c])]
         (condition (not= x :c))
         x))"
  [n lag & body]
  `(prob.core/map-query-fn ~n ~lag (fn [] ~@body)))

(defmacro forward-query
  "Forward sampling: run body n times from the prior.
   factor/observe/condition are no-ops.

   Example:
     (forward-query 100 (flip 0.7))
   ;=> [true true false true ...]"
  [n & body]
  `(prob.core/forward-query-fn ~n (fn [] ~@body)))

(defmacro query
  "Create a reusable conditional sampler.
   method is a list like '(rejection), '(enumerate), or '(mh lag).

   Example:
     (def coin (query '(rejection)
                 (let [x (flip)]
                   (condition x)
                   x)))
     (coin) ;=> true"
  [method & body]
  `(prob.core/conditional-fn ~method (fn [] ~@body)))

;; ---------------------------------------------------------------------------
;; CPS transform (Clojure version for nbb macro expansion)
;; ---------------------------------------------------------------------------
;; This is a pure form→form function, identical in logic to prob.cps/cps-of-expr.
;; Defined here because macros.clj runs as Clojure, not ClojureScript.

(def ^:private erp-to-dist
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

(def ^:private probabilistic-ops
  #{'sample* 'prob.core/sample* 'prob.dist/sample*
    'observe 'prob.core/observe 'prob.inference/observe
    'factor 'prob.core/factor 'prob.inference/factor
    'condition 'prob.core/condition 'prob.inference/condition
    'sample 'prob.builtins/sample})

(defn- m-fresh-sym [prefix]
  (gensym (str prefix "__")))

(defn- m-atomic? [form]
  (or (nil? form) (boolean? form) (number? form)
      (string? form) (keyword? form) (symbol? form)))

(defn- m-erp-call? [form]
  (and (seq? form) (symbol? (first form)) (contains? erp-to-dist (first form))))

(declare m-cps-of-expr)

(defn- m-cps-of-args [args body-fn]
  (if (empty? args)
    (body-fn [])
    (letfn [(process [remaining acc]
              (if (empty? remaining)
                (body-fn (vec acc))
                (let [arg (first remaining)
                      rest-args (rest remaining)]
                  (if (m-atomic? arg)
                    (process rest-args (conj acc arg))
                    (let [s (m-fresh-sym "a")]
                      (m-cps-of-expr arg
                        (list 'fn [s '$state]
                          (process rest-args (conj acc s)))))))))]
      (process args []))))

(defn- m-cps-of-let [bindings body k]
  (if (empty? bindings)
    (m-cps-of-expr (cons 'do body) k)
    (let [sym (first bindings)
          expr (second bindings)
          rest-bindings (drop 2 bindings)]
      (m-cps-of-expr expr
        (list 'fn [sym '$state]
          (m-cps-of-let (vec rest-bindings) body k))))))

(defn- m-cps-of-do [exprs k]
  (cond
    (empty? exprs) (list k nil '$state)
    (= 1 (count exprs)) (m-cps-of-expr (first exprs) k)
    :else
    (let [ignore (m-fresh-sym "_")]
      (m-cps-of-expr (first exprs)
        (list 'fn [ignore '$state]
          (m-cps-of-do (rest exprs) k))))))

(defn- m-cps-of-if [test then else k]
  (if (m-atomic? test)
    (list 'if test
      (m-cps-of-expr then k)
      (m-cps-of-expr else k))
    (let [t (m-fresh-sym "t")]
      (m-cps-of-expr test
        (list 'fn [t '$state]
          (list 'if t
            (m-cps-of-expr then k)
            (m-cps-of-expr else k)))))))

(defn- m-cps-of-fn [params body k]
  (let [inner-k (m-fresh-sym "k")]
    (list k
      (list 'fn (vec (concat params [inner-k '$state]))
        (m-cps-of-expr (cons 'do body) inner-k))
      '$state)))

(defn- m-cps-of-sample [dist-expr k]
  (if (m-atomic? dist-expr)
    (list 'prob.cps/cps-sample dist-expr k '$state)
    (let [d (m-fresh-sym "d")]
      (m-cps-of-expr dist-expr
        (list 'fn [d '$state]
          (list 'prob.cps/cps-sample d k '$state))))))

(defn- m-cps-of-observe [dist-expr value-expr k]
  (m-cps-of-args [dist-expr value-expr]
    (fn [[d v]]
      (list 'prob.cps/cps-observe d v k '$state))))

(defn- m-cps-of-factor [score-expr k]
  (if (m-atomic? score-expr)
    (list 'prob.cps/cps-factor score-expr k '$state)
    (let [s (m-fresh-sym "s")]
      (m-cps-of-expr score-expr
        (list 'fn [s '$state]
          (list 'prob.cps/cps-factor s k '$state))))))

(defn- m-cps-of-condition [pred-expr k]
  (if (m-atomic? pred-expr)
    (list 'prob.cps/cps-condition pred-expr k '$state)
    (let [p (m-fresh-sym "p")]
      (m-cps-of-expr pred-expr
        (list 'fn [p '$state]
          (list 'prob.cps/cps-condition p k '$state))))))

(defn- m-cps-of-erp [op args k]
  (let [dist-ctor (get erp-to-dist op)]
    (m-cps-of-args args
      (fn [evaled-args]
        (list 'prob.cps/cps-sample
          (cons dist-ctor evaled-args)
          k '$state)))))

(def ^:dynamic *m-loop-fn* nil)

(defn- m-cps-of-loop [bindings body k]
  (let [pairs (partition 2 bindings)
        loop-vars (mapv first pairs)
        init-exprs (mapv second pairs)
        loop-sym (m-fresh-sym "loop")
        loop-fn-params (vec (concat loop-vars ['k '$state]))
        loop-fn-body (binding [*m-loop-fn* loop-sym]
                       (m-cps-of-expr (cons 'do body) 'k))
        loop-fn (list 'fn loop-sym loop-fn-params loop-fn-body)]
    (m-cps-of-args init-exprs
      (fn [evaled-inits]
        (list (list 'fn [loop-sym '$state]
                (concat (list loop-sym) evaled-inits (list k '$state)))
              loop-fn '$state)))))

(defn- m-cps-of-recur [args k]
  (when-not *m-loop-fn*
    (throw (ex-info "cps: recur outside of loop" {:args args})))
  (m-cps-of-args args
    (fn [evaled-args]
      (concat (list *m-loop-fn*) evaled-args (list k '$state)))))

(defn- m-cps-of-case [test-expr clauses k]
  (if (m-atomic? test-expr)
    (let [pairs (partition 2 clauses)
          has-default (odd? (count clauses))
          default (when has-default (last clauses))
          case-clauses (mapcat (fn [[val expr]]
                                 [val (m-cps-of-expr expr k)])
                               pairs)]
      (if has-default
        (concat (list 'case test-expr) case-clauses (list (m-cps-of-expr default k)))
        (concat (list 'case test-expr) case-clauses)))
    (let [t (m-fresh-sym "case")]
      (m-cps-of-expr test-expr
        (list 'fn [t '$state]
          (m-cps-of-case t clauses k))))))

(defn- m-cps-of-application [f args k]
  (m-cps-of-args (cons f args)
    (fn [evaled]
      (let [evaled-f (first evaled)
            evaled-args (rest evaled)]
        (list k (cons evaled-f evaled-args) '$state)))))

(defn m-cps-of-expr [form k]
  (cond
    (or (nil? form) (boolean? form) (number? form) (string? form) (keyword? form))
    (list k form '$state)

    (symbol? form)
    (list k form '$state)

    (vector? form)
    (m-cps-of-args (seq form)
      (fn [evaled] (list k (vec evaled) '$state)))

    (map? form)
    (let [ks (keys form)
          vs (vals form)]
      (m-cps-of-args vs
        (fn [evaled-vs] (list k (zipmap ks evaled-vs) '$state))))

    (set? form)
    (m-cps-of-args (seq form)
      (fn [evaled] (list k (set evaled) '$state)))

    (and (seq? form) (= 'quote (first form)))
    (list k form '$state)

    (seq? form)
    (let [op (first form)]
      (cond
        (or (= op 'let) (= op 'let*))
        (let [bindings (vec (second form))
              body (drop 2 form)]
          (m-cps-of-let bindings (vec body) k))

        (= op 'do)
        (m-cps-of-do (rest form) k)

        (= op 'if)
        (let [test (nth form 1)
              then (nth form 2)
              else (if (> (count form) 3) (nth form 3) nil)]
          (m-cps-of-if test then else k))

        (= op 'when)
        (let [test (nth form 1)
              body (drop 2 form)]
          (m-cps-of-if test (cons 'do body) nil k))

        (= op 'when-not)
        (let [test (nth form 1)
              body (drop 2 form)]
          (m-cps-of-if test nil (cons 'do body) k))

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
              (m-cps-of-expr (desugar pairs) k))))

        (= op 'and)
        (let [args (rest form)]
          (cond
            (empty? args) (list k true '$state)
            (= 1 (count args)) (m-cps-of-expr (first args) k)
            :else
            (let [desugar (fn desugar [args]
                            (if (= 1 (count args))
                              (first args)
                              (let [a (m-fresh-sym "and")]
                                (list 'let [a (first args)]
                                  (list 'if a (desugar (rest args)) a)))))]
              (m-cps-of-expr (desugar args) k))))

        (= op 'or)
        (let [args (rest form)]
          (cond
            (empty? args) (list k nil '$state)
            (= 1 (count args)) (m-cps-of-expr (first args) k)
            :else
            (let [desugar (fn desugar [args]
                            (if (= 1 (count args))
                              (first args)
                              (let [a (m-fresh-sym "or")]
                                (list 'let [a (first args)]
                                  (list 'if a a (desugar (rest args)))))))]
              (m-cps-of-expr (desugar args) k))))

        (or (= op 'fn) (= op 'fn*))
        (let [has-name (symbol? (second form))
              params (if has-name (nth form 2) (second form))
              body (if has-name (drop 3 form) (drop 2 form))]
          (m-cps-of-fn params (vec body) k))

        (= op 'loop)
        (let [bindings (vec (second form))
              body (drop 2 form)]
          (m-cps-of-loop bindings (vec body) k))

        (= op 'recur)
        (m-cps-of-recur (rest form) k)

        (= op 'case)
        (let [test-expr (nth form 1)
              clauses (drop 2 form)]
          (m-cps-of-case test-expr (vec clauses) k))

        (or (= op 'sample*) (= op 'prob.core/sample*)
            (= op 'prob.dist/sample*)
            (= op 'sample) (= op 'prob.builtins/sample))
        (m-cps-of-sample (second form) k)

        (or (= op 'observe) (= op 'prob.core/observe)
            (= op 'prob.inference/observe))
        (m-cps-of-observe (nth form 1) (nth form 2) k)

        (or (= op 'factor) (= op 'prob.core/factor)
            (= op 'prob.inference/factor))
        (m-cps-of-factor (second form) k)

        (or (= op 'condition) (= op 'prob.core/condition)
            (= op 'prob.inference/condition))
        (m-cps-of-condition (second form) k)

        (m-erp-call? form)
        (m-cps-of-erp op (rest form) k)

        :else
        (m-cps-of-application (first form) (rest form) k)))

    :else
    (list k form '$state)))

(defmacro smc-query
  "Sequential Monte Carlo inference with n-particles.
   CPS-transforms the model body at macro expansion time.

   Example:
     (smc-query 100
       (let [p (beta 1 1)]
         (observe (bernoulli-dist p) true)
         (observe (bernoulli-dist p) true)
         (observe (bernoulli-dist p) false)
         p))
   ;=> list of weighted samples"
  [n-particles & body]
  (let [cps-body (m-cps-of-expr (cons 'do body) 'k__final)]
    `(prob.core/smc-query-fn ~n-particles
       (fn [~'k__final ~'$state] ~cps-body))))
