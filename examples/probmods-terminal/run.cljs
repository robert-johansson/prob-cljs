(ns run
  "ProbMods Terminal Notebook — paginated walkthrough of Chapters 1-2."
  (:require [lib.terminal :as term]
            [lib.viz :as viz]
            [lib.physics :as physics]
            [chapters.ch01-introduction :as ch01]
            [chapters.ch02-generative :as ch02]))

;; --- Collect all pages ---

(def all-chapters
  [{:title ch01/chapter-title
    :name  ch01/chapter-name
    :pages ch01/pages}
   {:title ch02/chapter-title
    :name  ch02/chapter-name
    :pages ch02/pages}])

(def all-pages
  (vec (mapcat (fn [ch]
                 (map (fn [page]
                        (assoc page
                               :chapter-title (:title ch)
                               :chapter-name (:name ch)))
                      (:pages ch)))
               all-chapters)))

(def total-pages (count all-pages))

;; --- Render a single page ---

(defn render-page
  "Display a single page: progress, prose, code, output."
  [page page-num]
  (term/clear-screen!)

  ;; Progress bar
  (term/print-progress
   (str (:chapter-title page) ": " (:chapter-name page))
   (:section page)
   page-num
   total-pages)

  (println)

  ;; Prose
  (when (:prose page)
    (term/print-prose (:prose page)))

  ;; Code display
  (when (:code page)
    (term/print-code (:code page)))

  ;; Execute
  (when (:run-fn page)
    (term/print-output-header)
    (println)))

(defn execute-page
  "Run the page's function. Returns a promise."
  [page]
  (if (:run-fn page)
    (let [result (try
                   ((:run-fn page))
                   (catch :default e
                     (println (term/red (str "Error: " (.-message e))))
                     nil))]
      (if (:async? page)
        ;; result is a promise
        (-> (js/Promise.resolve result)
            (.catch (fn [e]
                      (println (term/red (str "Error: " (.-message e))))
                      nil)))
        ;; Synchronous — wrap result
        (do
          (when (some? result)
            (println (str "  => " (term/fg 32 (pr-str result)))))
          (js/Promise.resolve nil))))
    (js/Promise.resolve nil)))

;; --- Page loop ---

(defn run-page
  "Render and execute a page. Returns a promise of :next, :rerun, or :quit."
  [page-idx]
  (let [page (nth all-pages page-idx)]
    (render-page page (inc page-idx))
    (-> (execute-page page)
        (.then (fn [_] (term/wait-for-key!))))))

(defn page-loop
  "Drive the pagination loop starting at page-idx."
  [page-idx]
  (if (>= page-idx total-pages)
    ;; Done!
    (do (term/clear-screen!)
        (println)
        (println (term/bold "  Congratulations!"))
        (println)
        (println "  You've completed Chapters 1-2 of Probabilistic Models of Cognition.")
        (println "  Chapter 3 (Conditioning) coming soon.")
        (println)
        (term/show-cursor!)
        (js/Promise.resolve nil))
    ;; Show page
    (-> (run-page page-idx)
        (.then (fn [action]
                 (case action
                   :next   (page-loop (inc page-idx))
                   :rerun  (page-loop page-idx)
                   :quit   (do (term/clear-screen!)
                               (println)
                               (println (term/dim "  Goodbye!"))
                               (println)
                               (term/show-cursor!)
                               (js/Promise.resolve nil))
                   ;; Default: advance
                   (page-loop (inc page-idx))))))))

;; --- Entry point ---

(defn main []
  (term/hide-cursor!)

  ;; Clean exit handlers
  (js/process.on "exit" (fn [] (term/show-cursor!)))
  (js/process.on "SIGINT" (fn []
                             (term/show-cursor!)
                             (.write js/process.stdout "\n")
                             (js/process.exit 0)))

  ;; Title screen
  (term/clear-screen!)
  (println)
  (println (term/bold "  Probabilistic Models of Cognition"))
  (println (term/dim "  Terminal Edition — prob-cljs"))
  (println)
  (println "  Chapters 1-2: Introduction & Generative Models")
  (println (str "  " total-pages " pages | interactive examples | press Enter to begin"))
  (println)
  (println (term/dim "  Controls: [Enter] next  [r] re-run  [q] quit"))
  (println)

  (-> (term/wait-for-any-key!)
      (.then (fn [key]
               (if (or (= key "q") (= key "Q"))
                 (do (term/show-cursor!)
                     (js/process.exit 0))
                 (page-loop 0))))))

(main)
