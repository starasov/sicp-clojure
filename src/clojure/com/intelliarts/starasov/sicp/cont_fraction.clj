(ns com.intelliarts.starasov.sicp.cont_fraction)

(defn cont-fraction
  "Computes the value of the k-term finite continued fraction (recursive process)."
  ([next-n next-d k] (cont-fraction next-n next-d k 1))
  ([next-n next-d k i]
  (if
    (> k i) (+ (next-d i) (/ (next-n i) (cont-fraction next-n next-d k (inc i))))
    1)))

(defn cont-fraction-iter
  "Computes the value of the k-term finite continued fraction (iterative process)."
  [next-n next-d k]

  (defn iter [result i]
    (if
    (> i 0) (iter (+ (next-d i) (/ (next-n i) result)) (dec i))
    result))

  (iter (/ (next-n k) (next-d k)) k))

; Calculating golden ratio using continued fraction.
(println (cont-fraction (fn [x] 1.0) (fn [x] 1.0) 1000))
(println (cont-fraction-iter (fn [x] 1.0) (fn [x] 1.0) 1000))

; Generates sequence 1, 2, 1, 1, 4, 1, 1, 6, 1, 1, 8, ...
(defn exp-next-d [x]
  (cond
    (= x 1) 1
    (= x 2) 2
    (zero? (mod (- x 2) 3)) (* 2 (+ 1 (quot (- x 2) 3)))
    :else 1))

; Calculates continued fraction expansion for exp - 2.
(println (cont-fraction (fn [x] 1.0) exp-next-d 1000))

; Approximation to the tangent function based on Lambert's formula
(defn tn-cf [x k]
  (defn tan-next-d [k]
    (+ k (dec k)))
  (/ x (cont-fraction (fn [i] (- (* x x))) tan-next-d k)))

(println (tn-cf 1.0 1000))