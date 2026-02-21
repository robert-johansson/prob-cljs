(ns prob.gpu.autograd
  "Reverse-mode automatic differentiation over GPU tensors.
   Provides value-and-grad and grad for computing gradients of scalar-valued functions."
  (:require [prob.gpu.tensor :as t]))

(defn- backward!
  "Walk tape in reverse, accumulate gradients.
   Returns a map of {tensor-id -> gradient-Tensor}."
  [loss-id grad-seed tape-entries]
  (let [grads (volatile! {loss-id grad-seed})]
    (doseq [entry (rseq tape-entries)]
      (when-let [g (get @grads (:output-id entry))]
        (vreset! grads ((:backward entry) g @grads))))
    @grads))

(defn value-and-grad
  "Returns (fn [params] -> [loss-tensor, grad-tensor]).
   f must be (fn [tracked-tensor] -> tracked-scalar)."
  [f]
  (fn [params]
    (let [tape    (volatile! [])
          pid     (t/next-id!)
          tracked (t/->TrackedTensor params pid)]
      (binding [t/*tape* tape]
        (let [result (f tracked)]
          (when-not (t/tracked? result)
            (throw (ex-info "value-and-grad: f must return a tracked tensor" {})))
          (let [loss-raw (:tensor result)
                grads   (backward! (:id result) (t/scalar 1.0) @tape)]
            [loss-raw (get grads pid)]))))))

(defn grad
  "Returns (fn [params] -> grad-tensor).
   f must be (fn [tracked-tensor] -> tracked-scalar)."
  [f]
  (let [vg (value-and-grad f)]
    (fn [params] (second (vg params)))))
