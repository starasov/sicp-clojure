(ns com.intelliarts.starasov.sicp.pairs-alternative-pow
  (:use clojure.test)
  (:use clojure.contrib.math))

"Exercise 2.5.
Show that we can represent pairs of nonnegative integers using only numbers and arithmetic
operations if we represent the pair a and b as the integer that is the product 2^a*3^b. Give the corresponding
definitions of the procedures cons, car, and cdr. "

(defn cons-impl [a b]
  (* (expt 2 a) (expt 3 b)))

(defn pair-iter [base, result, n]
  (if
    (= 1 (rem n base)) result
    (pair-iter base (inc result) (quot n base))))

(defn car [p]
  (pair-iter 2, 0, p))

(defn cdr [p]
  (pair-iter 3, 0, p))

(deftest test-car
  (is (= 0 (car (cons-impl 0 2))))
  (is (= 1 (car (cons-impl 1 2))))
  (is (= 6 (car (cons-impl 6 2)))))

(deftest test-cdr
  (is (= 0 (cdr (cons-impl 2 0))))
  (is (= 1 (cdr (cons-impl 2 1))))
  (is (= 6 (cdr (cons-impl 2 6)))))

(run-tests)