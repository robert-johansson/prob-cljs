(ns prob.gpu.inference
  "GPU-accelerated inference algorithms.
   All functions return Promises resolving to plain ClojureScript data."
  (:require [promesa.core :as p]
            [prob.gpu.tensor :as t]
            [prob.gpu.autograd :as ag]))

;; ---------------------------------------------------------------------------
;; Hamiltonian
;; ---------------------------------------------------------------------------

(defn- hamiltonian
  "H(q,p) = -log_density(q) + 0.5 * sum(p^2)"
  [log-density q p]
  (t/add (t/negative (log-density q))
         (t/multiply (t/scalar-cached 0.5) (t/sum (t/square p)))))

;; ---------------------------------------------------------------------------
;; Leapfrog integrator
;; ---------------------------------------------------------------------------

(defn- leapfrog-step
  "One leapfrog step: half-momentum, full-position, half-momentum.
   Uses fused scaled-add: a + alpha * b in one dispatch."
  [grad-fn q p step-size-t half-step-t]
  (let [g       (grad-fn q)
        p-half  (t/scaled-add half-step-t p g)
        q-new   (t/scaled-add step-size-t q p-half)
        g-new   (grad-fn q-new)
        p-new   (t/scaled-add half-step-t p-half g-new)]
    [q-new p-new]))

(defn- leapfrog
  "Full leapfrog trajectory. Returns [q' p']."
  [grad-fn q p step-size-t half-step-t n-steps]
  (loop [i 0, q q, p p]
    (if (>= i n-steps)
      [q p]
      (let [[q' p'] (leapfrog-step grad-fn q p step-size-t half-step-t)]
        (recur (inc i) q' p')))))

;; ---------------------------------------------------------------------------
;; HMC step with GPU-side accept/reject
;; ---------------------------------------------------------------------------

(defn- hmc-step
  "One HMC step. GPU-side accept/reject via t/where."
  [log-density grad-fn q step-size-t half-step-t n-leapfrog q-shape]
  (let [p           (t/randn q-shape)
        current-H   (hamiltonian log-density q p)
        [q' p']     (leapfrog grad-fn q p step-size-t half-step-t n-leapfrog)
        proposed-H  (hamiltonian log-density q' p')
        log-alpha   (t/subtract current-H proposed-H)
        u           (t/log (t/rand-uniform [1]))
        accept?     (t/greater log-alpha u)
        next-q      (t/where accept? q' q)]
    [next-q accept?]))

;; ---------------------------------------------------------------------------
;; HMC sampler
;; ---------------------------------------------------------------------------

(defn hmc
  "Hamiltonian Monte Carlo.
   opts: {:samples n :step-size f :leapfrog-steps n :burn n :callback fn}
   log-density: (fn [q-tensor] -> scalar-tensor)
   init-params: GPU tensor
   Returns Promise<{:samples [...] :acceptance-rate float}>"
  [opts log-density init-params]
  (let [{:keys [samples step-size leapfrog-steps burn callback grad-fn]
         :or   {burn 0}} opts
        total       (+ samples burn)
        grad-fn     (or grad-fn (ag/grad log-density))
        step-size-t (t/scalar-cached step-size)
        half-step-t (t/scalar-cached (* 0.5 step-size))
        q-shape     (t/shape init-params)
        result      (loop [i 0, q init-params, acc (transient []), accept-sum (t/scalar-cached 0)]
                      (if (>= i total)
                        {:raw-samples (persistent! acc) :accept-sum accept-sum}
                        (let [[q' accept?] (t/with-command-batch*
                                            (fn [] (hmc-step log-density grad-fn q
                                                             step-size-t half-step-t
                                                             leapfrog-steps q-shape)))
                              acc' (if (>= i burn) (conj! acc q') acc)
                              asum (t/add accept-sum accept?)]
                          (when (and callback (zero? (mod i 100)))
                            (callback {:iteration i :total total}))
                          (recur (inc i) q' acc' asum))))]
    (p/let [clj-samples (p/all (mapv t/to-clj (:raw-samples result)))
            acc-rate    (t/to-number (t/divide (:accept-sum result)
                                               (t/scalar total)))]
      {:samples         clj-samples
       :acceptance-rate acc-rate})))
