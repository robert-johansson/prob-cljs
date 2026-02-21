(ns prob.gpu.device
  "WebGPU device initialization.
   Detects browser vs Node.js and provides a unified GPU context."
  (:require [promesa.core :as p]))

(defonce ^:private *ctx* (volatile! nil))

(defn- request-gpu []
  (if (and (exists? js/navigator) (.-gpu js/navigator))
    (.-gpu js/navigator)
    (let [webgpu (js/require "webgpu")
          globals (.-globals webgpu)]
      (js/Object.assign js/globalThis globals)
      (.create webgpu #js []))))

(defn init!
  "Initialize WebGPU. Returns Promise<{:gpu :adapter :device :queue}>.
   Safe to call multiple times — returns cached context after first init."
  []
  (if-let [ctx @*ctx*]
    (p/resolved ctx)
    (p/let [gpu     (request-gpu)
            adapter (.requestAdapter gpu)
            _       (when-not adapter
                      (throw (ex-info "No WebGPU adapter found" {:type ::no-adapter})))
            device  (.requestDevice adapter)
            ctx     {:gpu gpu :adapter adapter :device device :queue (.-queue device)}]
      (vreset! *ctx* ctx)
      ctx)))

(defn ctx
  "Get the active GPU context. Throws if not initialized."
  []
  (or @*ctx*
      (throw (ex-info "GPU not initialized. Call (prob.gpu.device/init!) first."
                      {:type ::not-initialized}))))
