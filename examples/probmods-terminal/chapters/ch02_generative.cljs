(ns chapters.ch02-generative
  "Chapter 2: Generative Models — 27 examples + 5 exercises across 8 sections."
  (:require [prob.core :as prob]
            [prob.builtins :as builtins]
            [lib.viz :as viz]
            [lib.physics :as physics]))

(def chapter-title "Chapter 2")
(def chapter-name "Generative Models")

(def pages
  [;; ===================================================================
   ;; Section 1: Building Generative Models (5 examples)
   ;; ===================================================================

   {:section "Building Generative Models"
    :prose
    "We wish to describe in formal terms how to generate states of the world --
computations that involve random choices to capture uncertainty about the
process. The simplest random primitive is *flip* which returns *true* or
*false*:"
    :code "(flip)"
    :run-fn (fn [] (prob/flip))}

   {:section "Building Generative Models"
    :prose
    "Each time you run a program you get a _sample_. Press *r* to re-run and see
different samples. Collect many samples in a histogram:"
    :code "(hist (repeatedly 1000 flip) \"Flips\")"
    :run-fn (fn [] (viz/hist (repeatedly 1000 prob/flip) "Flips") nil)}

   {:section "Building Generative Models"
    :prose
    "Other random primitives include *gaussian*, *gamma*, *dirichlet*, etc.
Here we multiply two Gaussians:"
    :code "(* (gaussian 0 1) (gaussian 0 1))"
    :run-fn (fn [] (* (prob/gaussian 0 1) (prob/gaussian 0 1)))}

   {:section "Building Generative Models"
    :prose
    "We can build stochastic functions with *fn* and visualize their
distributions with a density plot:"
    :code "(defn two-gaussians [] (* (gaussian 0 1) (gaussian 0 1)))
(density (repeatedly 100 two-gaussians) \"two-gaussians\")"
    :run-fn (fn []
              (let [two-gaussians (fn [] (* (prob/gaussian 0 1) (prob/gaussian 0 1)))]
                (viz/density (repeatedly 100 two-gaussians) "two-gaussians")
                nil))}

   {:section "Building Generative Models"
    :prose
    "A stochastic function that only sometimes doubles its input:"
    :code "(defn noisy-double [x] (if (flip) x (+ x x)))
(noisy-double 3)"
    :run-fn (fn []
              (let [noisy-double (fn [x] (if (prob/flip) x (+ x x)))]
                (noisy-double 3)))}

   ;; ===================================================================
   ;; Section 2: Example: Flipping Coins (5 examples)
   ;; ===================================================================

   {:section "Flipping Coins"
    :prose
    "A coin is a stochastic function that returns *h* or *t* based on a
weight. Here is a fair coin and a trick coin:"
    :code "(def fair-coin (fn [] (if (flip 0.5) 'h 't)))
(hist (repeatedly 20 fair-coin) \"fair coin\")

(def trick-coin (fn [] (if (flip 0.95) 'h 't)))
(hist (repeatedly 20 trick-coin) \"trick coin\")"
    :run-fn (fn []
              (let [fair-coin  (fn [] (if (prob/flip 0.5) 'h 't))
                    trick-coin (fn [] (if (prob/flip 0.95) 'h 't))]
                (viz/hist (repeatedly 20 fair-coin) "fair coin")
                (viz/hist (repeatedly 20 trick-coin) "trick coin")
                nil))}

   {:section "Flipping Coins"
    :prose
    "We can abstract the coin-making process into a higher-order function.
*make-coin* takes a weight and returns a coin function:"
    :code "(defn make-coin [weight] (fn [] (if (flip weight) 'h 't)))
(def fair-coin (make-coin 0.5))
(def trick-coin (make-coin 0.95))
(def bent-coin (make-coin 0.25))
(hist (repeatedly 20 fair-coin) \"20 fair coin flips\")
(hist (repeatedly 20 trick-coin) \"20 trick coin flips\")
(hist (repeatedly 20 bent-coin) \"20 bent coin flips\")"
    :run-fn (fn []
              (let [make-coin (fn [weight] (fn [] (if (prob/flip weight) 'h 't)))
                    fair-coin  (make-coin 0.5)
                    trick-coin (make-coin 0.95)
                    bent-coin  (make-coin 0.25)]
                (viz/hist (repeatedly 20 fair-coin) "20 fair coin flips")
                (viz/hist (repeatedly 20 trick-coin) "20 trick coin flips")
                (viz/hist (repeatedly 20 bent-coin) "20 bent coin flips")
                nil))}

   {:section "Flipping Coins"
    :prose
    "We can also _transform_ a coin. *bend* takes a coin and returns a new coin
that is biased based on the original coin's output. If the original comes up
heads, the bent coin flips with weight 0.7; if tails, weight 0.1:"
    :code "(defn make-coin [weight] (fn [] (if (flip weight) 'h 't)))
(defn bend [coin]
  (fn [] (if (= (coin) 'h)
           ((make-coin 0.7))
           ((make-coin 0.1)))))
(def fair-coin (make-coin 0.5))
(def bent-coin (bend fair-coin))
(hist (repeatedly 100 bent-coin) \"bent coin\")"
    :run-fn (fn []
              (let [make-coin (fn [weight] (fn [] (if (prob/flip weight) 'h 't)))
                    bend (fn [coin]
                           (fn [] (if (= (coin) 'h)
                                    ((make-coin 0.7))
                                    ((make-coin 0.1)))))
                    fair-coin (make-coin 0.5)
                    bent-coin (bend fair-coin)]
                (viz/hist (repeatedly 100 bent-coin) "bent coin")
                nil))}

   {:section "Flipping Coins"
    :prose
    "Here we see how a distribution of _distributions_ works. We flip a
coin 10 times and count the heads, repeating 1000 times to see the
distribution of counts:"
    :code "(def make-coin (fn [weight] (fn [] (flip weight))))
(def coin (make-coin 0.8))
(def data
  (repeatedly 1000
    (fn [] (count (filter true? (repeatedly 10 coin))))))
(hist data \"Distribution of coin flips\")"
    :run-fn (fn []
              (let [coin (fn [] (prob/flip 0.8))
                    data (repeatedly 1000
                           (fn [] (count (filter true? (repeatedly 10 coin)))))]
                (viz/hist data "Distribution of coin flips")
                nil))}

   ;; ===================================================================
   ;; Section 3: Causal Models in Medical Diagnosis (2 examples)
   ;; ===================================================================

   {:section "Medical Diagnosis"
    :prose
    "Probabilistic programs can describe _causal models_ — here a simple
model of how diseases cause symptoms. Lung cancer is rare (1%), colds
are common (20%), and either can cause a cough:"
    :code "(def lung-cancer (flip 0.01))
(def cold (flip 0.2))
(def cough (or cold lung-cancer))
cough"
    :run-fn (fn []
              (let [lung-cancer (prob/flip 0.01)
                    cold (prob/flip 0.2)
                    cough (or cold lung-cancer)]
                cough))}

   {:section "Medical Diagnosis"
    :prose
    "A more complex diagnosis model with multiple diseases and symptoms.
Each disease has a probability, and each symptom depends on multiple
possible causes:"
    :code "(def lung-cancer (flip 0.01))
(def TB (flip 0.005))
(def stomach-flu (flip 0.1))
(def cold (flip 0.2))
(def other (flip 0.1))

(def cough
  (or (and cold (flip 0.5))
      (and lung-cancer (flip 0.3))
      (and TB (flip 0.7))
      (and other (flip 0.01))))
(def fever
  (or (and cold (flip 0.3))
      (and stomach-flu (flip 0.5))
      (and TB (flip 0.1))
      (and other (flip 0.01))))
(def chest-pain
  (or (and lung-cancer (flip 0.5))
      (and TB (flip 0.5))
      (and other (flip 0.01))))
(def shortness-of-breath
  (or (and lung-cancer (flip 0.5))
      (and TB (flip 0.2))
      (and other (flip 0.01))))

(list \"cough\" cough \"fever\" fever
      \"chest-pain\" chest-pain
      \"shortness-of-breath\" shortness-of-breath)"
    :run-fn (fn []
              (let [lung-cancer (prob/flip 0.01)
                    TB (prob/flip 0.005)
                    stomach-flu (prob/flip 0.1)
                    cold (prob/flip 0.2)
                    other (prob/flip 0.1)
                    cough (or (and cold (prob/flip 0.5))
                              (and lung-cancer (prob/flip 0.3))
                              (and TB (prob/flip 0.7))
                              (and other (prob/flip 0.01)))
                    fever (or (and cold (prob/flip 0.3))
                              (and stomach-flu (prob/flip 0.5))
                              (and TB (prob/flip 0.1))
                              (and other (prob/flip 0.01)))
                    chest-pain (or (and lung-cancer (prob/flip 0.5))
                                   (and TB (prob/flip 0.5))
                                   (and other (prob/flip 0.01)))
                    shortness-of-breath (or (and lung-cancer (prob/flip 0.5))
                                            (and TB (prob/flip 0.2))
                                            (and other (prob/flip 0.01)))]
                (list "cough" cough "fever" fever
                      "chest-pain" chest-pain
                      "shortness-of-breath" shortness-of-breath)))}

   ;; ===================================================================
   ;; Section 4: Prediction, Simulation, and Probabilities (5 examples)
   ;; ===================================================================

   {:section "Prediction & Simulation"
    :prose
    "Generating a pair of random values. Each run of the program produces
a _sample_ from the joint distribution:"
    :code "(list (flip) (flip))"
    :run-fn (fn [] (list (prob/flip) (prob/flip)))}

   {:section "Prediction & Simulation"
    :prose
    "Collecting many samples reveals the _distribution_ — the space of
possible outputs and their probabilities:"
    :code "(defn random-pair [] (list (flip) (flip)))
(hist (repeatedly 1000 random-pair) \"return values\")"
    :run-fn (fn []
              (let [random-pair (fn [] (list (prob/flip) (prob/flip)))]
                (viz/hist (repeatedly 1000 random-pair) "return values")
                nil))}

   {:section "Prediction & Simulation"
    :prose
    "Note the difference: *def* binds once, *defn* generates fresh randomness
each call. Here A and B are fixed for this run:"
    :code "(def A (flip))
(def B (flip))
(def C (list A B))
C"
    :run-fn (fn []
              (let [A (prob/flip)
                    B (prob/flip)]
                (list A B)))}

   {:section "Prediction & Simulation"
    :prose
    "Causal dependence: the value of B depends on A. If A is true,
B flips with weight 0.3; otherwise 0.7:"
    :code "(def A (flip))
(def B (flip (if A 0.3 0.7)))
(list A B)"
    :run-fn (fn []
              (let [A (prob/flip)
                    B (prob/flip (if A 0.3 0.7))]
                (list A B)))}

   {:section "Prediction & Simulation"
    :prose
    "Even simple compositions create interesting distributions. What is the
probability that *(or (flip) (flip))* returns true?"
    :code "(or (flip) (flip))"
    :run-fn (fn [] (or (prob/flip) (prob/flip)))}

   ;; ===================================================================
   ;; Section 5: Stochastic Recursion (1 example)
   ;; ===================================================================

   {:section "Stochastic Recursion"
    :prose
    "Stochastic recursion creates distributions with _unbounded support_.
The geometric distribution: flip a coin, if heads return 0, otherwise
recurse and add 1. The result can be any non-negative integer, but
larger values are exponentially less likely:"
    :code "(defn geometric [p]
  (if (flip p) 0 (+ 1 (geometric p))))
(hist (repeatedly 1000 (fn [] (geometric 0.6)))
      \"Geometric of 0.6\")"
    :run-fn (fn []
              (letfn [(geometric [p]
                        (if (prob/flip p) 0 (+ 1 (geometric p))))]
                (viz/hist (repeatedly 1000 (fn [] (geometric 0.6)))
                          "Geometric of 0.6")
                nil))}

   ;; ===================================================================
   ;; Section 6: Persistent Randomness: mem (5 examples)
   ;; ===================================================================

   {:section "Persistent Randomness: mem"
    :prose
    "A function that draws a random eye color gives a _different_ color
each time it's called — even for the same person:"
    :code "(defn eye-color [person]
  (uniform-draw '(blue green brown)))
(list (eye-color 'bob)
      (eye-color 'alice)
      (eye-color 'bob))"
    :run-fn (fn []
              (let [eye-color (fn [person]
                                (prob/uniform-draw '(blue green brown)))]
                (list (eye-color 'bob)
                      (eye-color 'alice)
                      (eye-color 'bob))))}

   {:section "Persistent Randomness: mem"
    :prose
    "The problem: two calls to *flip* generate _independent_ samples, so
they're unlikely to be equal:"
    :code "(= (flip) (flip))"
    :run-fn (fn [] (= (prob/flip) (prob/flip)))}

   {:section "Persistent Randomness: mem"
    :prose
    "The solution is *mem* (memoize). A memoized function returns the _same_
random value when called with the same arguments:"
    :code "(def mem-flip (mem flip))
(= (mem-flip) (mem-flip))"
    :run-fn (fn []
              (let [mem-flip (prob/mem prob/flip)]
                (= (mem-flip) (mem-flip))))}

   {:section "Persistent Randomness: mem"
    :prose
    "Now *eye-color* is persistent — Bob always has the same eye color within
a run, but Alice can differ from Bob:"
    :code "(def eye-color
  (mem (fn [person] (uniform-draw '(blue green brown)))))
(list (eye-color 'bob)
      (eye-color 'alice)
      (eye-color 'bob))"
    :run-fn (fn []
              (let [eye-color (prob/mem
                                (fn [person]
                                  (prob/uniform-draw '(blue green brown))))]
                (list (eye-color 'bob)
                      (eye-color 'alice)
                      (eye-color 'bob))))}

   {:section "Persistent Randomness: mem"
    :prose
    "With *mem*, each unique argument produces a persistent random value.
Both rows below will be identical — the same sequence of coin flips:"
    :code "(def flip-n (mem (fn [n] (flip))))
(list (list (flip-n 1) (flip-n 12) (flip-n 47) (flip-n 1548))
      (list (flip-n 1) (flip-n 12) (flip-n 47) (flip-n 1548)))"
    :run-fn (fn []
              (let [flip-n (prob/mem (fn [n] (prob/flip)))]
                (list (list (flip-n 1) (flip-n 12) (flip-n 47) (flip-n 1548))
                      (list (flip-n 1) (flip-n 12) (flip-n 47) (flip-n 1548)))))}

   ;; ===================================================================
   ;; Section 7: Bayesian Tug of War (1 example)
   ;; ===================================================================

   {:section "Bayesian Tug of War"
    :prose
    "Combining *mem*, *gaussian*, and higher-order functions to model a
tug-of-war tournament. Each person has a persistent *strength* (via mem).
On each pull, there's a 25% chance of being *lazy* (halving strength).
The team with more total pulling force wins.

Notice: strengths persist across matches (Alice is always Alice-strong)
but laziness varies per pull:"
    :code "(def strength (mem (fn [person] (gaussian 0 1))))
(defn lazy [person] (flip 0.25))
(defn pulling [person]
  (if (lazy person) (/ (strength person) 2) (strength person)))
(defn total-pulling [team] (sum (map pulling team)))
(defn winner [team1 team2]
  (if (< (total-pulling team1) (total-pulling team2))
    team2 team1))

(list \"Tournament:\"
  (winner '(alice bob) '(sue tom))
  (winner '(alice bob) '(sue tom))
  (winner '(alice sue) '(bob tom))
  (winner '(alice sue) '(bob tom))
  (winner '(alice tom) '(bob sue))
  (winner '(alice tom) '(bob sue)))"
    :run-fn (fn []
              (let [strength (prob/mem (fn [person] (prob/gaussian 0 1)))
                    lazy-fn  (fn [person] (prob/flip 0.25))
                    pulling  (fn [person]
                               (if (lazy-fn person)
                                 (/ (strength person) 2)
                                 (strength person)))
                    total-pulling (fn [team] (prob/sum (map pulling team)))
                    winner (fn [team1 team2]
                             (if (< (total-pulling team1) (total-pulling team2))
                               team2 team1))]
                (list "Tournament:"
                      (winner '(alice bob) '(sue tom))
                      (winner '(alice bob) '(sue tom))
                      (winner '(alice sue) '(bob tom))
                      (winner '(alice sue) '(bob tom))
                      (winner '(alice tom) '(bob sue))
                      (winner '(alice tom) '(bob sue)))))}

   ;; ===================================================================
   ;; Section 8: Intuitive Physics (3 examples)
   ;; ===================================================================

   {:section "Intuitive Physics"
    :prose
    "We have a 2D physics simulator via Planck.js. *animate-physics* shows
an animation; *run-physics* returns the final world state.

Objects are specified as: *((shape static? (w h)) (x y))*

First, random falling shapes with static obstacles:"
    :code "(defn dim [] (uniform 5 20))
(defn static-dim [] (uniform 10 50))
(defn shape [] (if (flip) \"circle\" \"rect\"))
(defn xpos [] (uniform 100 (- worldWidth 100)))
(defn ypos [] (uniform 100 (- worldHeight 100)))
(defn make-falling [] (list (list (shape) false (list (dim) (dim)))
                            (list (xpos) 0)))
(defn make-static [] (list (list (shape) true (list (static-dim) (static-dim)))
                           (list (xpos) (ypos))))
(def ground (list (list \"rect\" true (list worldWidth 10))
                  (list (/ worldWidth 2) worldHeight)))
(def world (list ground (make-falling) (make-falling) (make-falling)
                 (make-static) (make-static)))
(animate-physics 1000 world)"
    :run-fn (fn []
              (let [world-w physics/world-width
                    world-h physics/world-height
                    dim (fn [] (prob/uniform 5 20))
                    static-dim (fn [] (prob/uniform 10 50))
                    shape (fn [] (if (prob/flip) "circle" "rect"))
                    xpos (fn [] (prob/uniform 100 (- world-w 100)))
                    ypos (fn [] (prob/uniform 100 (- world-h 100)))
                    make-falling (fn [] (list (list (shape) false (list (dim) (dim)))
                                             (list (xpos) 0)))
                    make-static (fn [] (list (list (shape) true (list (static-dim) (static-dim)))
                                            (list (xpos) (ypos))))
                    ground (list (list "rect" true (list world-w 10))
                                 (list (/ world-w 2) world-h))
                    world (list ground (make-falling) (make-falling) (make-falling)
                                (make-static) (make-static))]
                (physics/animate-physics 1000 world)))
    :async? true}

   {:section "Intuitive Physics"
    :prose
    "Stochastic tower building: each block is placed randomly above the
previous one. The result is a tower of varying stability. Watch as
physics determines whether it stands or falls:"
    :code "(def x-center (/ worldWidth 2))
(def ground (list (list \"rect\" true (list worldWidth 10))
                  (list (/ worldWidth 2) worldHeight)))
(defn dim [] (uniform 10 50))
(defn add-block [prev first?]
  (let [w (dim) h (dim)
        prev-x (first (second prev))
        prev-w (first (nth (first prev) 2))
        prev-y (second (second prev))
        prev-h (second (nth (first prev) 2))]
    (list (list \"rect\" false (list w h))
          (list (if first? x-center
                  (uniform (- prev-x prev-w) (+ prev-x prev-w)))
                (- prev-y prev-h h)))))
(defn make-tower []
  (let [b1 (add-block ground true)
        b2 (add-block b1 false)
        b3 (add-block b2 false)
        b4 (add-block b3 false)
        b5 (add-block b4 false)]
    (list ground b1 b2 b3 b4 b5)))
(animate-physics 1000 (make-tower))"
    :run-fn (fn []
              (let [world-w physics/world-width
                    world-h physics/world-height
                    x-center (/ world-w 2)
                    ground (list (list "rect" true (list world-w 10))
                                 (list (/ world-w 2) world-h))
                    dim (fn [] (prob/uniform 10 50))
                    get-x (fn [obj] (first (second obj)))
                    get-y (fn [obj] (second (second obj)))
                    get-w (fn [obj] (first (nth (vec (first obj)) 2)))
                    get-h (fn [obj] (second (nth (vec (first obj)) 2)))
                    add-block (fn [prev first?]
                                (let [w (dim) h (dim)]
                                  (list (list "rect" false (list w h))
                                        (list (if first? x-center
                                                  (prob/uniform (- (get-x prev) (get-w prev))
                                                                (+ (get-x prev) (get-w prev))))
                                              (- (get-y prev) (get-h prev) h)))))
                    b1 (add-block ground true)
                    b2 (add-block b1 false)
                    b3 (add-block b2 false)
                    b4 (add-block b3 false)
                    b5 (add-block b4 false)
                    tower (list ground b1 b2 b3 b4 b5)]
                (physics/animate-physics 1000 tower)))
    :async? true}

   {:section "Intuitive Physics"
    :prose
    "Tower stability analysis: three pre-defined towers (stable, almost
unstable, and unstable) are each run 10 times with slight random
perturbation. We measure whether each tower falls by checking if the
highest block has moved significantly.

This takes a moment — running 30 physics simulations..."
    :code "(def ground (list (list \"rect\" true (list worldWidth 10))
                  (list (/ worldWidth 2) (+ worldHeight 6))))
(def stable-world (list ground ...))        ;; wide blocks
(def almost-unstable-world (list ground ...)) ;; narrow blocks
(def unstable-world (list ground ...))       ;; offset blocks

(defn does-tower-fall [initial final]
  (let [highest-y (fn [w] (apply min (map get-y w)))]
    (not (< (abs (- (highest-y initial) (highest-y final))) 1))))

(defn noisify [world]
  (map (fn [obj] (if (static? obj) obj
         (list (first obj) (list (uniform (- (get-x obj) 10)
                                         (+ (get-x obj) 10))
                                (get-y obj))))) world))

(hist (repeatedly 10 run-stable) \"stable\")
(hist (repeatedly 10 run-almost) \"almost unstable\")
(hist (repeatedly 10 run-unstable) \"unstable\")"
    :run-fn (fn []
              (let [world-w physics/world-width
                    world-h physics/world-height
                    ground (list (list "rect" true (list world-w 10))
                                 (list (/ world-w 2) (+ world-h 6)))
                    stable-world (list ground
                                       (list (list "rect" false (list 60 22)) (list 175 473))
                                       (list (list "rect" false (list 50 38)) (list 160 413))
                                       (list (list "rect" false (list 40 35)) (list 167 340))
                                       (list (list "rect" false (list 30 29)) (list 177 276))
                                       (list (list "rect" false (list 11 17)) (list 169 230)))
                    almost-unstable-world (list ground
                                                (list (list "rect" false (list 24 22)) (list 175 473))
                                                (list (list "rect" false (list 15 38)) (list 160 413))
                                                (list (list "rect" false (list 11 35)) (list 167 340))
                                                (list (list "rect" false (list 11 29)) (list 177 276))
                                                (list (list "rect" false (list 11 17)) (list 169 230)))
                    unstable-world (list ground
                                         (list (list "rect" false (list 60 22)) (list 175 473))
                                         (list (list "rect" false (list 50 38)) (list 90 413))
                                         (list (list "rect" false (list 40 35)) (list 140 340))
                                         (list (list "rect" false (list 10 29)) (list 177 276))
                                         (list (list "rect" false (list 50 17)) (list 140 230)))
                    get-x (fn [obj] (first (second obj)))
                    get-y (fn [obj] (second (second obj)))
                    static? (fn [obj] (second (first obj)))
                    noisify (fn [world]
                              (map (fn [obj]
                                     (if (static? obj) obj
                                         (list (first obj)
                                               (list (prob/uniform (- (get-x obj) 10)
                                                                   (+ (get-x obj) 10))
                                                     (get-y obj)))))
                                   world))
                    does-tower-fall (fn [iw fw]
                                     (let [hy (fn [w] (apply min (map get-y (remove static? w))))]
                                       (not (< (js/Math.abs (- (hy iw) (hy fw))) 1))))
                    ;; Pre-settle worlds so does-tower-fall measures actual toppling
                    settle (fn [world] (physics/run-physics 1000 world))
                    settled-stable (settle stable-world)
                    settled-almost (settle almost-unstable-world)
                    settled-unstable (settle unstable-world)
                    run-trial (fn [settled-world]
                                (let [noisy (noisify settled-world)
                                      final (physics/run-physics 1000 noisy)]
                                  (does-tower-fall settled-world final)))]
                (viz/hist (repeatedly 10 #(run-trial settled-stable)) "stable")
                (viz/hist (repeatedly 10 #(run-trial settled-almost)) "almost unstable")
                (viz/hist (repeatedly 10 #(run-trial settled-unstable)) "unstable")
                nil))}

   ;; ===================================================================
   ;; Exercises (5)
   ;; ===================================================================

   {:section "Exercises"
    :prose
    "*Exercise 1:* Show that these three programs have the same marginal
distribution on return values (all produce true with probability 0.4):"
    :code ";; Program A
(if (flip) (flip 0.7) (flip 0.1))

;; Program B
(flip (if (flip) 0.7 0.1))

;; Program C
(flip 0.4)"
    :run-fn (fn []
              (let [prog-a (fn [] (if (prob/flip) (prob/flip 0.7) (prob/flip 0.1)))
                    prog-b (fn [] (prob/flip (if (prob/flip) 0.7 0.1)))
                    prog-c (fn [] (prob/flip 0.4))]
                (viz/hist (repeatedly 1000 prog-a) "Program A")
                (viz/hist (repeatedly 1000 prog-b) "Program B")
                (viz/hist (repeatedly 1000 prog-c) "Program C")
                nil))}

   {:section "Exercises"
    :prose
    "*Exercise 2:* Why do these two programs give different distributions?
The first binds a _single_ flip; the second calls flip _each time_:"
    :code ";; Program 1: one flip, used three times
(def foo (flip))
(list foo foo foo)

;; Program 2: three independent flips
(defn foo [] (flip))
(list (foo) (foo) (foo))"
    :run-fn (fn []
              (println "  Program 1 (def — one flip, three uses):")
              (let [foo (prob/flip)]
                (println "   " (list foo foo foo)))
              (println)
              (println "  Program 2 (defn — three independent flips):")
              (let [foo (fn [] (prob/flip))]
                (println "   " (list (foo) (foo) (foo))))
              nil)}

   {:section "Exercises"
    :prose
    "*Exercise 3:* Why doesn't this work for medical diagnosis?
Each call to *lung-cancer* or *cold* generates a _new_ random value.
The fix is to use *mem* so that each person's disease status is consistent:"
    :code ";; Broken: each symptom check re-rolls disease status
(defn lung-cancer [person] (flip 0.01))
(defn cold [person] (flip 0.2))
(defn cough [person] (or (cold person) (lung-cancer person)))
(list (cough 'bob) (cough 'alice))

;; Fixed: use mem for persistent properties
;; (def lung-cancer (mem (fn [person] (flip 0.01))))
;; (def cold (mem (fn [person] (flip 0.2))))"
    :run-fn (fn []
              (println "  Broken version (no mem):")
              (let [lung-cancer (fn [person] (prob/flip 0.01))
                    cold (fn [person] (prob/flip 0.2))
                    cough (fn [person] (or (cold person) (lung-cancer person)))]
                (println "   " (list (cough 'bob) (cough 'alice))))
              (println)
              (println "  Fixed version (with mem):")
              (let [lung-cancer (prob/mem (fn [person] (prob/flip 0.01)))
                    cold (prob/mem (fn [person] (prob/flip 0.2)))
                    cough (fn [person] (or (cold person) (lung-cancer person)))]
                (println "   " (list (cough 'bob) (cough 'alice)
                                     "bob-again:" (cough 'bob))))
              nil)}

   {:section "Exercises"
    :prose
    "*Exercise 4:* Evaluate the *bend* function step by step. What is the
expected probability that a bent fair coin comes up heads?

P(h) = P(original=h) * P(h|0.7) + P(original=t) * P(h|0.1)
     = 0.5 * 0.7 + 0.5 * 0.1 = 0.4"
    :code "(defn make-coin [weight] (fn [] (if (flip weight) 'h 't)))
(defn bend [coin]
  (fn [] (if (= (coin) 'h) ((make-coin 0.7)) ((make-coin 0.1)))))
(def fair-coin (make-coin 0.5))
(def bent-coin (bend fair-coin))
(hist (repeatedly 1000 bent-coin) \"bent coin (expect ~40% heads)\")"
    :run-fn (fn []
              (let [make-coin (fn [weight] (fn [] (if (prob/flip weight) 'h 't)))
                    bend (fn [coin]
                           (fn [] (if (= (coin) 'h)
                                    ((make-coin 0.7))
                                    ((make-coin 0.1)))))
                    fair-coin (make-coin 0.5)
                    bent-coin (bend fair-coin)]
                (viz/hist (repeatedly 1000 bent-coin) "bent coin (expect ~40% heads)")
                nil))}

   {:section "Exercises"
    :prose
    "*Exercise 5:* What is the probability that the geometric distribution
returns 5? Think about it: geometric(p) returns 5 when you get 5 tails
followed by 1 heads: P(5) = (1-p)^5 * p

For p=0.6: P(5) = 0.4^5 * 0.6 = 0.006144"
    :code "(defn geometric [p]
  (if (flip p) 0 (+ 1 (geometric p))))
(hist (repeatedly 10000 (fn [] (geometric 0.6)))
      \"Geometric of 0.6\")"
    :run-fn (fn []
              (letfn [(geometric [p]
                        (if (prob/flip p) 0 (+ 1 (geometric p))))]
                (viz/hist (repeatedly 10000 (fn [] (geometric 0.6)))
                          "Geometric of 0.6")
                (let [count-5 (count (filter #(= % 5) (repeatedly 10000 #(geometric 0.6))))]
                  (println (str "  Empirical P(5) = " (.toFixed (/ count-5 10000) 4)
                                "  (theory: 0.0061)")))
                nil))}])
