(ns prob.sci
  "SCI configuration for prob-cljs. Registers all prob namespaces
   with SCI for use in Scittle (browser) or any SCI-based host.

   This file is only used by the Scittle plugin build — nbb users
   don't need it (they load the .cljs files directly)."
  (:require [sci.core :as sci]
            [prob.erp :as erp]
            [prob.inference :as inference]
            [prob.builtins :as builtins]
            [prob.dist :as dist]
            [prob.math :as math]
            [prob.core :as core]))

;; ---------------------------------------------------------------------------
;; SCI namespace objects
;; ---------------------------------------------------------------------------

(def erp-ns (sci/create-ns 'prob.erp nil))
(def inference-ns (sci/create-ns 'prob.inference nil))
(def builtins-ns (sci/create-ns 'prob.builtins nil))
(def dist-ns (sci/create-ns 'prob.dist nil))
(def math-ns (sci/create-ns 'prob.math nil))
(def core-ns (sci/create-ns 'prob.core nil))
(def macros-ns (sci/create-ns 'prob.macros nil))

;; ---------------------------------------------------------------------------
;; prob.erp
;; ---------------------------------------------------------------------------

(def erp-namespace
  {'flip           (sci/copy-var erp/flip erp-ns)
   'gaussian       (sci/copy-var erp/gaussian erp-ns)
   'uniform        (sci/copy-var erp/uniform erp-ns)
   'uniform-draw   (sci/copy-var erp/uniform-draw erp-ns)
   'random-integer (sci/copy-var erp/random-integer erp-ns)
   'multinomial    (sci/copy-var erp/multinomial erp-ns)
   'sample-discrete (sci/copy-var erp/sample-discrete erp-ns)
   'beta           (sci/copy-var erp/beta erp-ns)
   'gamma          (sci/copy-var erp/gamma erp-ns)
   'dirichlet      (sci/copy-var erp/dirichlet erp-ns)
   'exponential    (sci/copy-var erp/exponential erp-ns)
   'binomial       (sci/copy-var erp/binomial erp-ns)
   'poisson        (sci/copy-var erp/poisson erp-ns)
   'categorical    (sci/copy-var erp/categorical erp-ns)
   'rand           (sci/copy-var erp/rand erp-ns)
   'set-seed!      (sci/copy-var erp/set-seed! erp-ns)})

;; ---------------------------------------------------------------------------
;; prob.dist
;; ---------------------------------------------------------------------------

(def dist-namespace
  {'sample*             (sci/copy-var dist/sample* dist-ns)
   'observe*            (sci/copy-var dist/observe* dist-ns)
   'dist?               (sci/copy-var dist/dist? dist-ns)
   'bernoulli-dist      (sci/copy-var dist/bernoulli-dist dist-ns)
   'gaussian-dist       (sci/copy-var dist/gaussian-dist dist-ns)
   'uniform-dist        (sci/copy-var dist/uniform-dist dist-ns)
   'beta-dist           (sci/copy-var dist/beta-dist dist-ns)
   'gamma-dist          (sci/copy-var dist/gamma-dist dist-ns)
   'exponential-dist    (sci/copy-var dist/exponential-dist dist-ns)
   'dirichlet-dist      (sci/copy-var dist/dirichlet-dist dist-ns)
   'uniform-draw-dist   (sci/copy-var dist/uniform-draw-dist dist-ns)
   'random-integer-dist (sci/copy-var dist/random-integer-dist dist-ns)
   'multinomial-dist    (sci/copy-var dist/multinomial-dist dist-ns)
   'sample-discrete-dist (sci/copy-var dist/sample-discrete-dist dist-ns)
   'binomial-dist       (sci/copy-var dist/binomial-dist dist-ns)
   'poisson-dist        (sci/copy-var dist/poisson-dist dist-ns)
   'categorical-dist    (sci/copy-var dist/categorical-dist dist-ns)
   'enumerate*          (sci/copy-var dist/enumerate* dist-ns)
   'delta-dist          (sci/copy-var dist/delta-dist dist-ns)
   'cauchy-dist         (sci/copy-var dist/cauchy-dist dist-ns)
   'laplace-dist        (sci/copy-var dist/laplace-dist dist-ns)
   'lognormal-dist      (sci/copy-var dist/lognormal-dist dist-ns)
   'student-t-dist      (sci/copy-var dist/student-t-dist dist-ns)
   'mixture-dist        (sci/copy-var dist/mixture-dist dist-ns)
   'marginal-dist       (sci/copy-var dist/marginal-dist dist-ns)})

;; ---------------------------------------------------------------------------
;; prob.math
;; ---------------------------------------------------------------------------

(def math-namespace
  {'log-gamma-fn (sci/copy-var math/log-gamma-fn math-ns)
   'log-beta-fn  (sci/copy-var math/log-beta-fn math-ns)
   'log-fact     (sci/copy-var math/log-fact math-ns)
   'log-sum-exp  (sci/copy-var math/log-sum-exp math-ns)
   'digamma      (sci/copy-var math/digamma math-ns)
   'erf          (sci/copy-var math/erf math-ns)})

;; ---------------------------------------------------------------------------
;; prob.inference
;; ---------------------------------------------------------------------------

(def inference-namespace
  {'condition            (sci/copy-var inference/condition inference-ns)
   'factor               (sci/copy-var inference/factor inference-ns)
   'observe              (sci/copy-var inference/observe inference-ns)
   'rejection-query-fn   (sci/copy-var inference/rejection-query-fn inference-ns)
   'mh-query-fn          (sci/copy-var inference/mh-query-fn inference-ns)
   'enumeration-query-fn (sci/copy-var inference/enumeration-query-fn inference-ns)
   'importance-query-fn  (sci/copy-var inference/importance-query-fn inference-ns)
   'conditional-fn       (sci/copy-var inference/conditional-fn inference-ns)
   'mh-query-scored-fn  (sci/copy-var inference/mh-query-scored-fn inference-ns)
   'map-query-fn        (sci/copy-var inference/map-query-fn inference-ns)
   'condition-equal     (sci/copy-var inference/condition-equal inference-ns)})

;; ---------------------------------------------------------------------------
;; prob.builtins
;; ---------------------------------------------------------------------------

(def builtins-namespace
  {'pair           (sci/copy-var builtins/pair builtins-ns)
   'pair?          (sci/copy-var builtins/pair? builtins-ns)
   'car            (sci/copy-var builtins/car builtins-ns)
   'cdr            (sci/copy-var builtins/cdr builtins-ns)
   'null?          (sci/copy-var builtins/null? builtins-ns)
   'length         (sci/copy-var builtins/length builtins-ns)
   'append         (sci/copy-var builtins/append builtins-ns)
   'list-ref       (sci/copy-var builtins/list-ref builtins-ns)
   'list-elt       (sci/copy-var builtins/list-elt builtins-ns)
   'take-n         (sci/copy-var builtins/take-n builtins-ns)
   'drop-n         (sci/copy-var builtins/drop-n builtins-ns)
   'second-elem    (sci/copy-var builtins/second-elem builtins-ns)
   'third-elem     (sci/copy-var builtins/third-elem builtins-ns)
   'fourth-elem    (sci/copy-var builtins/fourth-elem builtins-ns)
   'fifth-elem     (sci/copy-var builtins/fifth-elem builtins-ns)
   'sixth-elem     (sci/copy-var builtins/sixth-elem builtins-ns)
   'seventh-elem   (sci/copy-var builtins/seventh-elem builtins-ns)
   'last-elem      (sci/copy-var builtins/last-elem builtins-ns)
   'but-last       (sci/copy-var builtins/but-last builtins-ns)
   'make-list      (sci/copy-var builtins/make-list builtins-ns)
   'for-each       (sci/copy-var builtins/for-each builtins-ns)
   'zip            (sci/copy-var builtins/zip builtins-ns)
   'sort-list      (sci/copy-var builtins/sort-list builtins-ns)
   'partition-list (sci/copy-var builtins/partition-list builtins-ns)
   'unique         (sci/copy-var builtins/unique builtins-ns)
   'nub            (sci/copy-var builtins/nub builtins-ns)
   'range-inclusive (sci/copy-var builtins/range-inclusive builtins-ns)
   'iota           (sci/copy-var builtins/iota builtins-ns)
   'update-list    (sci/copy-var builtins/update-list builtins-ns)
   'list-index     (sci/copy-var builtins/list-index builtins-ns)
   'repeat-fn      (sci/copy-var builtins/repeat-fn builtins-ns)
   'fold           (sci/copy-var builtins/fold builtins-ns)
   'foldl          (sci/copy-var builtins/foldl builtins-ns)
   'foldr          (sci/copy-var builtins/foldr builtins-ns)
   'union          (sci/copy-var builtins/union builtins-ns)
   'intersection   (sci/copy-var builtins/intersection builtins-ns)
   'difference     (sci/copy-var builtins/difference builtins-ns)
   'mod'           (sci/copy-var builtins/mod' builtins-ns)
   'round          (sci/copy-var builtins/round builtins-ns)
   'floor          (sci/copy-var builtins/floor builtins-ns)
   'ceil           (sci/copy-var builtins/ceil builtins-ns)
   'abs            (sci/copy-var builtins/abs builtins-ns)
   'log            (sci/copy-var builtins/log builtins-ns)
   'exp            (sci/copy-var builtins/exp builtins-ns)
   'expt           (sci/copy-var builtins/expt builtins-ns)
   'sqrt           (sci/copy-var builtins/sqrt builtins-ns)
   'sin            (sci/copy-var builtins/sin builtins-ns)
   'cos            (sci/copy-var builtins/cos builtins-ns)
   'tan            (sci/copy-var builtins/tan builtins-ns)
   'asin           (sci/copy-var builtins/asin builtins-ns)
   'acos           (sci/copy-var builtins/acos builtins-ns)
   'atan           (sci/copy-var builtins/atan builtins-ns)
   'atan2          (sci/copy-var builtins/atan2 builtins-ns)
   'sum            (sci/copy-var builtins/sum builtins-ns)
   'prod           (sci/copy-var builtins/prod builtins-ns)
   'mean           (sci/copy-var builtins/mean builtins-ns)
   'variance       (sci/copy-var builtins/variance builtins-ns)
   'weighted-mean  (sci/copy-var builtins/weighted-mean builtins-ns)
   'weighted-variance (sci/copy-var builtins/weighted-variance builtins-ns)
   'empirical-distribution (sci/copy-var builtins/empirical-distribution builtins-ns)
   'expectation    (sci/copy-var builtins/expectation builtins-ns)
   'eq?            (sci/copy-var builtins/eq? builtins-ns)
   'equal?         (sci/copy-var builtins/equal? builtins-ns)
   'soft-equal     (sci/copy-var builtins/soft-equal builtins-ns)
   'member         (sci/copy-var builtins/member builtins-ns)
   'assoc-list     (sci/copy-var builtins/assoc-list builtins-ns)
   'number?'       (sci/copy-var builtins/number?' builtins-ns)
   'string?'       (sci/copy-var builtins/string?' builtins-ns)
   'boolean?'      (sci/copy-var builtins/boolean?' builtins-ns)
   'procedure?     (sci/copy-var builtins/procedure? builtins-ns)
   'string-append  (sci/copy-var builtins/string-append builtins-ns)
   'string-length  (sci/copy-var builtins/string-length builtins-ns)
   'stringify       (sci/copy-var builtins/stringify builtins-ns)
   'string-split   (sci/copy-var builtins/string-split builtins-ns)
   'string-slice   (sci/copy-var builtins/string-slice builtins-ns)
   'string->number (sci/copy-var builtins/string->number builtins-ns)
   'number->string (sci/copy-var builtins/number->string builtins-ns)
   'symbol->string (sci/copy-var builtins/symbol->string builtins-ns)
   'string->symbol (sci/copy-var builtins/string->symbol builtins-ns)
   'boolean->number (sci/copy-var builtins/boolean->number builtins-ns)
   'number->boolean (sci/copy-var builtins/number->boolean builtins-ns)
   'apply-fn       (sci/copy-var builtins/apply-fn builtins-ns)
   'compose        (sci/copy-var builtins/compose builtins-ns)
   'identity-fn    (sci/copy-var builtins/identity-fn builtins-ns)
   'mem            (sci/copy-var builtins/mem builtins-ns)
   'DPmem          (sci/copy-var builtins/DPmem builtins-ns)
   'make-gensym    (sci/copy-var builtins/make-gensym builtins-ns)
   'gensym'        (sci/copy-var builtins/gensym' builtins-ns)
   'display        (sci/copy-var builtins/display builtins-ns)
   'error          (sci/copy-var builtins/error builtins-ns)
   'sample         (sci/copy-var builtins/sample builtins-ns)
   'get-time       (sci/copy-var builtins/get-time builtins-ns)})

;; ---------------------------------------------------------------------------
;; prob.core (public API)
;; ---------------------------------------------------------------------------

(def core-namespace
  {'rand               (sci/copy-var core/rand core-ns)
   'set-seed!          (sci/copy-var core/set-seed! core-ns)
   'flip               (sci/copy-var core/flip core-ns)
   'gaussian           (sci/copy-var core/gaussian core-ns)
   'uniform            (sci/copy-var core/uniform core-ns)
   'uniform-draw       (sci/copy-var core/uniform-draw core-ns)
   'random-integer     (sci/copy-var core/random-integer core-ns)
   'multinomial        (sci/copy-var core/multinomial core-ns)
   'sample-discrete    (sci/copy-var core/sample-discrete core-ns)
   'beta               (sci/copy-var core/beta core-ns)
   'gamma              (sci/copy-var core/gamma core-ns)
   'dirichlet          (sci/copy-var core/dirichlet core-ns)
   'exponential        (sci/copy-var core/exponential core-ns)
   'binomial           (sci/copy-var core/binomial core-ns)
   'poisson            (sci/copy-var core/poisson core-ns)
   'categorical        (sci/copy-var core/categorical core-ns)
   'sample*            (sci/copy-var core/sample* core-ns)
   'observe*           (sci/copy-var core/observe* core-ns)
   'dist?              (sci/copy-var core/dist? core-ns)
   'enumerate*         (sci/copy-var core/enumerate* core-ns)
   'bernoulli-dist     (sci/copy-var core/bernoulli-dist core-ns)
   'gaussian-dist      (sci/copy-var core/gaussian-dist core-ns)
   'uniform-dist       (sci/copy-var core/uniform-dist core-ns)
   'beta-dist          (sci/copy-var core/beta-dist core-ns)
   'gamma-dist         (sci/copy-var core/gamma-dist core-ns)
   'exponential-dist   (sci/copy-var core/exponential-dist core-ns)
   'dirichlet-dist     (sci/copy-var core/dirichlet-dist core-ns)
   'uniform-draw-dist  (sci/copy-var core/uniform-draw-dist core-ns)
   'random-integer-dist (sci/copy-var core/random-integer-dist core-ns)
   'multinomial-dist   (sci/copy-var core/multinomial-dist core-ns)
   'sample-discrete-dist (sci/copy-var core/sample-discrete-dist core-ns)
   'binomial-dist      (sci/copy-var core/binomial-dist core-ns)
   'poisson-dist       (sci/copy-var core/poisson-dist core-ns)
   'categorical-dist   (sci/copy-var core/categorical-dist core-ns)
   'log-gamma-fn       (sci/copy-var core/log-gamma-fn core-ns)
   'log-beta-fn        (sci/copy-var core/log-beta-fn core-ns)
   'log-fact           (sci/copy-var core/log-fact core-ns)
   'log-sum-exp        (sci/copy-var core/log-sum-exp core-ns)
   'digamma            (sci/copy-var core/digamma core-ns)
   'erf                (sci/copy-var core/erf core-ns)
   'condition          (sci/copy-var core/condition core-ns)
   'factor             (sci/copy-var core/factor core-ns)
   'observe            (sci/copy-var core/observe core-ns)
   'rejection-query-fn (sci/copy-var core/rejection-query-fn core-ns)
   'mh-query-fn        (sci/copy-var core/mh-query-fn core-ns)
   'enumeration-query-fn (sci/copy-var core/enumeration-query-fn core-ns)
   'importance-query-fn (sci/copy-var core/importance-query-fn core-ns)
   'conditional-fn     (sci/copy-var core/conditional-fn core-ns)
   'mh-query-scored-fn (sci/copy-var core/mh-query-scored-fn core-ns)
   'map-query-fn       (sci/copy-var core/map-query-fn core-ns)
   'condition-equal    (sci/copy-var core/condition-equal core-ns)
   'delta-dist         (sci/copy-var core/delta-dist core-ns)
   'cauchy-dist        (sci/copy-var core/cauchy-dist core-ns)
   'laplace-dist       (sci/copy-var core/laplace-dist core-ns)
   'lognormal-dist     (sci/copy-var core/lognormal-dist core-ns)
   'student-t-dist     (sci/copy-var core/student-t-dist core-ns)
   'mixture-dist       (sci/copy-var core/mixture-dist core-ns)
   'marginal-dist      (sci/copy-var core/marginal-dist core-ns)
   'mem                (sci/copy-var core/mem core-ns)
   'DPmem              (sci/copy-var core/DPmem core-ns)
   'mean               (sci/copy-var core/mean core-ns)
   'variance           (sci/copy-var core/variance core-ns)
   'weighted-mean      (sci/copy-var core/weighted-mean core-ns)
   'weighted-variance  (sci/copy-var core/weighted-variance core-ns)
   'empirical-distribution (sci/copy-var core/empirical-distribution core-ns)
   'expectation        (sci/copy-var core/expectation core-ns)
   'sum                (sci/copy-var core/sum core-ns)
   'prod               (sci/copy-var core/prod core-ns)
   'repeat-fn          (sci/copy-var core/repeat-fn core-ns)})

;; ---------------------------------------------------------------------------
;; prob.macros (SCI macros — ^:macro functions with [&form &env & args])
;; ---------------------------------------------------------------------------

(defn ^:macro rejection-query [_ _ & body]
  `(prob.core/rejection-query-fn (fn [] ~@body)))

(defn ^:macro mh-query [_ _ n lag & body]
  `(prob.core/mh-query-fn ~n ~lag (fn [] ~@body)))

(defn ^:macro importance-query [_ _ n & body]
  `(prob.core/importance-query-fn ~n (fn [] ~@body)))

(defn ^:macro enumeration-query [_ _ & body]
  `(prob.core/enumeration-query-fn (fn [] ~@body)))

(defn ^:macro mh-query-scored [_ _ n lag & body]
  `(prob.core/mh-query-scored-fn ~n ~lag (fn [] ~@body)))

(defn ^:macro map-query [_ _ n lag & body]
  `(prob.core/map-query-fn ~n ~lag (fn [] ~@body)))

(defn ^:macro query [_ _ method & body]
  `(prob.core/conditional-fn ~method (fn [] ~@body)))

(def macros-namespace
  {'rejection-query   (sci/copy-var rejection-query macros-ns)
   'mh-query          (sci/copy-var mh-query macros-ns)
   'mh-query-scored   (sci/copy-var mh-query-scored macros-ns)
   'map-query         (sci/copy-var map-query macros-ns)
   'importance-query  (sci/copy-var importance-query macros-ns)
   'enumeration-query (sci/copy-var enumeration-query macros-ns)
   'query             (sci/copy-var query macros-ns)})

;; ---------------------------------------------------------------------------
;; Combined config
;; ---------------------------------------------------------------------------

(def config
  {:namespaces
   {'prob.erp       erp-namespace
    'prob.inference inference-namespace
    'prob.builtins  builtins-namespace
    'prob.dist      dist-namespace
    'prob.math      math-namespace
    'prob.core      core-namespace
    'prob.macros    macros-namespace}})
