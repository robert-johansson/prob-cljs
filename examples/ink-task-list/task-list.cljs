(ns task-list
  "Ink task list with probabilistic outcomes.
   Port of the nbb ink-task-list example + prob library.

   Setup:
     cd examples/ink-task-list
     npm install
     npx nbb -cp ../../src task-list.cljs"
  (:require [prob.core :refer [flip uniform-draw]]
            ["react" :as React]
            ["ink" :as ink]
            ["ink-task-list" :refer [TaskList Task]]
            ["cli-spinners$default.dots" :as dots]))

;; --- Probabilistic task outcomes ---
;; Each task succeeds with probability 0.7; failure message is random.

(defn random-outcome []
  (if (flip 0.7)
    {:state "success" :status "done!"}
    {:state "error"   :status (uniform-draw ["oops" "failed" "crashed" "timeout"])}))

;; --- Mutable state ---

(def tasks
  #js {"one"   #js {:label "one"   :state "pending" :status "" :spinner dots}
       "two"   #js {:label "two"   :state "pending" :status "" :spinner dots}
       "three" #js {:label "three" :state "pending" :status "" :spinner dots}})

(def state #js {:step 0 :handle nil})

;; --- React element tree ---

(defn build-ui []
  (React/createElement TaskList #js {}
    (React/createElement Task (aget tasks "one"))
    (React/createElement Task (aget tasks "two"))
    (React/createElement Task (aget tasks "three"))))

(defn update-task! [task-name new-state status]
  (let [t (aget tasks task-name)]
    (aset t "state" new-state)
    (aset t "status" status)))

;; --- Initial render ---

(def app (ink/render (build-ui)))

;; --- Transition logic ---

(def outcomes (mapv (fn [_] (random-outcome)) (range 3)))

(defn transition! []
  (let [n (aget state "step")]
    (if (> n 5)
      (do
        (js/clearInterval (aget state "handle"))
        (.unmount app)
        (js/process.exit 0))
      (do
        (case n
          0 (update-task! "one"   "loading" "working...")
          1 (update-task! "two"   "loading" "working...")
          2 (update-task! "three" "loading" "working...")
          3 (update-task! "one"   (:state (outcomes 0)) (:status (outcomes 0)))
          4 (update-task! "two"   (:state (outcomes 1)) (:status (outcomes 1)))
          5 (update-task! "three" (:state (outcomes 2)) (:status (outcomes 2)))
          nil)
        (aset state "step" (inc n))
        (.rerender app (build-ui))))))

;; --- Start ---

(aset state "handle" (js/setInterval transition! 1000))
