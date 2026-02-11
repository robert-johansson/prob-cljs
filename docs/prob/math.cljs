(ns prob.math
  "Mathematical special functions for probabilistic programming.
   Pure ClojureScript, zero dependencies.")

;; ---------------------------------------------------------------------------
;; Log-Gamma function (Lanczos approximation, g=7, n=9)
;; ---------------------------------------------------------------------------

(def ^:private lanczos-g 7)

(def ^:private lanczos-coefficients
  [0.99999999999980993
   676.5203681218851
   -1259.1392167224028
   771.32342877765313
   -176.61502916214059
   12.507343278686905
   -0.13857109526572012
   9.9843695780195716e-6
   1.5056327351493116e-7])

(defn log-gamma-fn
  "Log of the Gamma function. Lanczos approximation, accurate to ~15 digits.
   Returns js/Infinity for non-positive integers (poles of Gamma)."
  [z]
  (if (<= z 0)
    js/Infinity
    (let [z (dec z)
          x (loop [i 1
                   acc (first lanczos-coefficients)]
              (if (>= i (count lanczos-coefficients))
                acc
                (recur (inc i)
                       (+ acc (/ (nth lanczos-coefficients i)
                                 (+ z i))))))
          t (+ z lanczos-g 0.5)]
      (+ (* 0.5 (js/Math.log (* 2 js/Math.PI)))
         (* (+ z 0.5) (js/Math.log t))
         (- t)
         (js/Math.log x)))))

;; ---------------------------------------------------------------------------
;; Log-Beta function (derived from log-gamma)
;; ---------------------------------------------------------------------------

(defn log-beta-fn
  "Log of the Beta function: ln(B(a,b)) = ln(Gamma(a)) + ln(Gamma(b)) - ln(Gamma(a+b))."
  [a b]
  (+ (log-gamma-fn a) (log-gamma-fn b) (- (log-gamma-fn (+ a b)))))

;; ---------------------------------------------------------------------------
;; Log-factorial (derived from log-gamma)
;; ---------------------------------------------------------------------------

(defn log-fact
  "Log-factorial: ln(n!) = ln(Gamma(n+1))."
  [n]
  (log-gamma-fn (inc n)))

;; ---------------------------------------------------------------------------
;; Log-Sum-Exp (numerically stable)
;; ---------------------------------------------------------------------------

(defn log-sum-exp
  "Numerically stable log(sum(exp(xs))).
   Two-argument form: log(exp(a) + exp(b)).
   One-argument form: log(sum_i exp(x_i)) for a collection."
  ([a b]
   (cond
     (= a ##-Inf) b
     (= b ##-Inf) a
     :else (let [mx (max a b)]
             (+ mx (js/Math.log (+ (js/Math.exp (- a mx))
                                   (js/Math.exp (- b mx))))))))
  ([xs]
   (let [xs (vec xs)]
     (if (empty? xs)
       ##-Inf
       (let [mx (apply max xs)]
         (if (= mx ##-Inf)
           ##-Inf
           (+ mx (js/Math.log
                  (reduce (fn [acc x] (+ acc (js/Math.exp (- x mx))))
                          0.0 xs)))))))))

;; ---------------------------------------------------------------------------
;; Digamma function (recurrence + asymptotic expansion)
;; ---------------------------------------------------------------------------

(defn digamma
  "Digamma function psi(x) = d/dx ln(Gamma(x)).
   Uses recurrence to shift x >= 6, then asymptotic series.
   Returns js/NaN for x <= 0."
  [x]
  (if (<= x 0)
    js/NaN
    ;; Recurrence: psi(x) = psi(x+1) - 1/x
    ;; Shift until x >= 6, then use asymptotic expansion
    (loop [x x
           result 0.0]
      (if (>= x 6)
        (let [inv-x  (/ 1.0 x)
              inv-x2 (* inv-x inv-x)]
          ;; psi(x) ~ ln(x) - 1/(2x) - 1/(12x^2) + 1/(120x^4) - 1/(252x^6) + 1/(240x^8)
          (+ result
             (js/Math.log x)
             (* -0.5 inv-x)
             (* inv-x2
                (+ (/ -1.0 12.0)
                   (* inv-x2
                      (+ (/ 1.0 120.0)
                         (* inv-x2
                            (+ (/ -1.0 252.0)
                               (* inv-x2
                                  (/ 1.0 240.0))))))))))
        (recur (inc x) (- result (/ 1.0 x)))))))

;; ---------------------------------------------------------------------------
;; Error function (Abramowitz and Stegun approximation 7.1.26)
;; ---------------------------------------------------------------------------

(defn erf
  "Error function. Abramowitz & Stegun polynomial approximation.
   Maximum error ~1.5e-7."
  [x]
  (let [sign (if (neg? x) -1.0 1.0)
        x    (js/Math.abs x)
        t    (/ 1.0 (+ 1.0 (* 0.3275911 x)))
        poly (* t (+ 0.254829592
                     (* t (+ -0.284496736
                              (* t (+ 1.421413741
                                      (* t (+ -1.453152027
                                               (* t 1.061405429)))))))))]
    (* sign (- 1.0 (* poly (js/Math.exp (- (* x x))))))))
