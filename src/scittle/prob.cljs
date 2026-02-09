(ns scittle.prob
  "Scittle plugin entry point for prob-cljs.
   Registers prob.core, prob.macros, prob.erp, prob.inference, and
   prob.builtins with Scittle's SCI context."
  {:no-doc true}
  (:require [prob.sci :refer [config]]
            [scittle.core :as scittle]))

(defn init []
  (scittle/register-plugin! ::prob config))
