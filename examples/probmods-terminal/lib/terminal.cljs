(ns lib.terminal
  "Terminal utilities: ANSI styling, prose rendering, key input, pagination.")

;; --- ANSI escape helpers ---

(def esc "\033[")

(defn bold [s] (str esc "1m" s esc "0m"))
(defn dim [s] (str esc "2m" s esc "0m"))
(defn italic [s] (str esc "3m" s esc "0m"))
(defn underline [s] (str esc "4m" s esc "0m"))
(defn fg [code s] (str esc code "m" s esc "0m"))

(def cyan   (partial fg 36))
(def blue   (partial fg 34))
(def yellow (partial fg 33))
(def green  (partial fg 32))
(def red    (partial fg 31))
(def magenta (partial fg 35))
(def gray   (partial fg 90))

;; --- Screen control ---

(defn clear-screen! []
  (.write js/process.stdout (str esc "H" esc "2J")))

(defn hide-cursor! []
  (.write js/process.stdout (str esc "?25l")))

(defn show-cursor! []
  (.write js/process.stdout (str esc "?25h")))

;; --- Word wrapping ---

(defn word-wrap
  "Wrap text to fit within `width` columns, preserving existing newlines."
  [text width]
  (let [lines (.split text "\n")]
    (.join
     (.map lines
       (fn [line]
         (if (<= (count line) width)
           line
           (let [words (.split line " ")
                 result (array)
                 current (atom "")]
             (doseq [word words]
               (if (= @current "")
                 (reset! current word)
                 (if (<= (+ (count @current) 1 (count word)) width)
                   (swap! current #(str % " " word))
                   (do (.push result @current)
                       (reset! current word)))))
             (when (seq @current)
               (.push result @current))
             (.join result "\n")))))
     "\n")))

;; --- Output helpers ---

(defn print-progress
  "Print progress bar at top of screen."
  [chapter-title section-title page-num total-pages]
  (let [bar (str (bold chapter-title)
                 "  "
                 (dim "\u2500")
                 "  "
                 section-title
                 "  "
                 (dim (str "[" page-num "/" total-pages "]")))]
    (println bar)
    (println (dim (apply str (repeat 60 "\u2500"))))))

(defn print-prose
  "Print prose with word-wrapping and basic styling.
   *bold* becomes bold, _italic_ becomes italic."
  [text]
  (let [wrapped (word-wrap text 72)
        ;; Apply inline styling
        styled (-> wrapped
                   (.replace (js/RegExp. "\\*([^*]+)\\*" "g")
                             (fn [_ m] (bold m)))
                   (.replace (js/RegExp. "_([^_]+)_" "g")
                             (fn [_ m] (italic m))))]
    (println styled)))

(defn print-code
  "Print code in a box-drawing border."
  [code-str]
  (let [lines (.split code-str "\n")
        max-len (apply max (map count lines))
        pad (fn [s] (let [n (- max-len (count s))]
                      (str s (apply str (repeat n " ")))))
        top (str (gray (str "\u250c" (apply str (repeat (+ max-len 2) "\u2500")) "\u2510")))
        bot (str (gray (str "\u2514" (apply str (repeat (+ max-len 2) "\u2500")) "\u2518")))]
    (println)
    (println top)
    (doseq [line lines]
      (println (str (gray "\u2502") " " (cyan (pad line)) " " (gray "\u2502"))))
    (println bot)
    (println)))

(defn print-section-header [title]
  (println)
  (println (str (bold (yellow title))))
  (println (dim (apply str (repeat (min (count title) 40) "\u2500"))))
  (println))

(defn print-output-header []
  (println (dim "Output:")))

;; --- Key input ---

(defn- tty?
  "Check if stdin is a TTY (supports raw mode)."
  []
  (boolean (.-isTTY js/process.stdin)))

(defn wait-for-key!
  "Wait for a keypress in raw mode. Returns a promise that resolves to the key.
   Handles Enter, 'r', 'q'. Falls back to line-based input if not a TTY."
  []
  (js/Promise.
   (fn [resolve _reject]
     (println)
     (println (dim "[Enter] next  [r] re-run  [q] quit"))
     (let [stdin js/process.stdin]
       (if (tty?)
         (do
           (.setRawMode stdin true)
           (.resume stdin)
           (.setEncoding stdin "utf8")
           (let [handler (fn handler [key]
                           (.removeListener stdin "data" handler)
                           (.setRawMode stdin false)
                           (.pause stdin)
                           (cond
                             (= key "\u0003")
                             (do (show-cursor!)
                                 (js/process.exit 0))
                             (or (= key "\r") (= key "\n"))
                             (resolve :next)
                             (or (= key "r") (= key "R"))
                             (resolve :rerun)
                             (or (= key "q") (= key "Q"))
                             (resolve :quit)
                             :else
                             (resolve :next)))]
             (.on stdin "data" handler)))
         ;; Not a TTY — read a line from stdin
         (do
           (.resume stdin)
           (.setEncoding stdin "utf8")
           (let [handler (fn handler [data]
                           (.removeListener stdin "data" handler)
                           (.pause stdin)
                           (let [line (.trim (str data))]
                             (cond
                               (= line "q") (resolve :quit)
                               (= line "r") (resolve :rerun)
                               :else (resolve :next))))]
             (.on stdin "data" handler))))))))

(defn wait-for-any-key!
  "Wait for any keypress. Returns a promise."
  []
  (js/Promise.
   (fn [resolve _reject]
     (let [stdin js/process.stdin]
       (if (tty?)
         (do
           (.setRawMode stdin true)
           (.resume stdin)
           (.setEncoding stdin "utf8")
           (let [handler (fn handler [key]
                           (.removeListener stdin "data" handler)
                           (.setRawMode stdin false)
                           (.pause stdin)
                           (when (= key "\u0003")
                             (show-cursor!)
                             (js/process.exit 0))
                           (resolve key))]
             (.on stdin "data" handler)))
         ;; Not a TTY — read a line
         (do
           (.resume stdin)
           (.setEncoding stdin "utf8")
           (let [handler (fn handler [data]
                           (.removeListener stdin "data" handler)
                           (.pause stdin)
                           (resolve (.trim (str data))))]
             (.on stdin "data" handler))))))))
