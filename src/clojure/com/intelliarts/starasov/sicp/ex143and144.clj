(ns com.intelliarts.starasov.sicp.ex143and144
  (:use com.intelliarts.starasov.sicp.ex142))

; Write a procedure that takes as inputs a procedure that computes f and a positive integer n
; and returns the procedure that computes the nth repeated application of f.
(defn repeated [f n]
  (if (> n 1)
    (compose f (repeated f (dec n)))
    (fn [x] (f x))))

(println ((repeated square 2) 5))


(defn avg [& args]
  (/ (reduce + args) (count args)))

(defn smooth [f]
  (def dx 1e-5)
  (fn [x] (avg (f x) (f (- x dx)) (f (+ x dx)))))

(defn n-fold-smooth [f n]
  (repeated (smooth f) n))

(def x (list 1 2))
(println (first x))
(println (last x))
