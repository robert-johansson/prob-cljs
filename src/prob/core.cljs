(ns prob.core
  "Probabilistic programming as a ClojureScript library.
   Use these functions directly in nbb scripts — no Church strings needed.

   ERPs: flip, gaussian, beta, uniform, etc.
   Inference: condition, factor, rejection-query-fn, mh-query-fn, etc.
   Utilities: mem, mean, variance, sum, prod, sample-discrete

   For query macros (rejection-query, mh-query, enumeration-query),
   see prob.macros."
  (:require [prob.erp :as erp]
            [prob.inference :as inference]
            [prob.builtins :as builtins]
            [prob.dist :as dist]
            [prob.math :as math]))

;; ---------------------------------------------------------------------------
;; Elementary Random Primitives
;; ---------------------------------------------------------------------------

(def rand erp/rand)
(def set-seed! erp/set-seed!)

(def flip erp/flip)
(def gaussian erp/gaussian)
(def uniform erp/uniform)
(def uniform-draw erp/uniform-draw)
(def random-integer erp/random-integer)
(def multinomial erp/multinomial)
(def sample-discrete erp/sample-discrete)
(def beta erp/beta)
(def gamma erp/gamma)
(def dirichlet erp/dirichlet)
(def exponential erp/exponential)
(def binomial erp/binomial)
(def poisson erp/poisson)
(def categorical erp/categorical)

;; ---------------------------------------------------------------------------
;; Distribution protocol
;; ---------------------------------------------------------------------------

(def sample* dist/sample*)
(def observe* dist/observe*)
(def dist? dist/dist?)
(def enumerate* dist/enumerate*)

(def bernoulli-dist dist/bernoulli-dist)
(def gaussian-dist dist/gaussian-dist)
(def uniform-dist dist/uniform-dist)
(def beta-dist dist/beta-dist)
(def gamma-dist dist/gamma-dist)
(def exponential-dist dist/exponential-dist)
(def dirichlet-dist dist/dirichlet-dist)
(def uniform-draw-dist dist/uniform-draw-dist)
(def random-integer-dist dist/random-integer-dist)
(def multinomial-dist dist/multinomial-dist)
(def sample-discrete-dist dist/sample-discrete-dist)
(def binomial-dist dist/binomial-dist)
(def poisson-dist dist/poisson-dist)
(def categorical-dist dist/categorical-dist)

;; ---------------------------------------------------------------------------
;; Math
;; ---------------------------------------------------------------------------

(def log-gamma-fn math/log-gamma-fn)
(def log-beta-fn math/log-beta-fn)
(def log-fact math/log-fact)
(def log-sum-exp math/log-sum-exp)
(def digamma math/digamma)
(def erf math/erf)

;; ---------------------------------------------------------------------------
;; Inference
;; ---------------------------------------------------------------------------

(def condition inference/condition)
(def factor inference/factor)
(def observe inference/observe)
(def rejection-query-fn inference/rejection-query-fn)
(def mh-query-fn inference/mh-query-fn)
(def enumeration-query-fn inference/enumeration-query-fn)
(def importance-query-fn inference/importance-query-fn)
(def conditional-fn inference/conditional-fn)
(def mh-query-scored-fn inference/mh-query-scored-fn)
(def map-query-fn inference/map-query-fn)
(def condition-equal inference/condition-equal)
(def forward-query-fn inference/forward-query-fn)
(def infer inference/infer)

;; ---------------------------------------------------------------------------
;; New distributions
;; ---------------------------------------------------------------------------

(def delta-dist dist/delta-dist)
(def cauchy-dist dist/cauchy-dist)
(def laplace-dist dist/laplace-dist)
(def lognormal-dist dist/lognormal-dist)
(def student-t-dist dist/student-t-dist)
(def mixture-dist dist/mixture-dist)
(def kde-dist dist/kde-dist)
(def entropy dist/entropy)

;; ---------------------------------------------------------------------------
;; Distribution: Marginal
;; ---------------------------------------------------------------------------

(def marginal-dist dist/marginal-dist)

;; ---------------------------------------------------------------------------
;; Utilities
;; ---------------------------------------------------------------------------

(def mem builtins/mem)
(def cache builtins/cache)
(def DPmem builtins/DPmem)
(def sd builtins/sd)
(def mean builtins/mean)
(def variance builtins/variance)
(def weighted-mean builtins/weighted-mean)
(def weighted-variance builtins/weighted-variance)
(def empirical-distribution builtins/empirical-distribution)
(def expectation builtins/expectation)
(def sum builtins/sum)
(def prod builtins/prod)
(def repeat-fn builtins/repeat-fn)
