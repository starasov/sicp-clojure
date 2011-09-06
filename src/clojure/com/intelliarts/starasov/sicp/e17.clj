(ns com.intelliarts.starasov.sicp.e17
  (:use clojure.contrib.math))

(defn average [y x]
  (/ (+ x y) 2))

(defn improve-sqrt [guess x]
  (average (/ x guess) guess))

(defn improve-qubert [guess x]
  (/ (+ (/ x (* guess guess)) (* 2 guess)) 3))

(defn good-enough? [guess x improve-fn]
  (< (abs (- guess (improve-fn guess x))) 1e-4))

(defn my-root-iter
  [guess x improve-fn]
    (if (good-enough? guess x improve-fn)
      guess
      (my-root-iter (improve-fn guess x) x improve-fn)))

(defn my-sqrt [x]
  (my-root-iter 1.0 x improve-sqrt))

(defn my-qubert [x]
  (my-root-iter 1.0 x improve-qubert))

(def numbers [0.00045, 2, 4, 10, 345678, 9436678867, 999465622095698472])
(println (map sqrt numbers))
(println (map my-sqrt numbers))

(println (map (fn [x] (expt x 1/3)) numbers))
(println (map my-qubert numbers))