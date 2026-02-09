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
            [prob.builtins :as builtins]))

;; ---------------------------------------------------------------------------
;; Elementary Random Primitives
;; ---------------------------------------------------------------------------

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

;; ---------------------------------------------------------------------------
;; Inference
;; ---------------------------------------------------------------------------

(def condition inference/condition)
(def factor inference/factor)
(def rejection-query-fn inference/rejection-query-fn)
(def mh-query-fn inference/mh-query-fn)
(def enumeration-query-fn inference/enumeration-query-fn)
(def conditional-fn inference/conditional-fn)

;; ---------------------------------------------------------------------------
;; Utilities
;; ---------------------------------------------------------------------------

(def mem builtins/mem)
(def mean builtins/mean)
(def variance builtins/variance)
(def sum builtins/sum)
(def prod builtins/prod)
(def repeat-fn builtins/repeat-fn)
