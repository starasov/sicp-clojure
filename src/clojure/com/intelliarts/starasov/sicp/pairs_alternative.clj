(ns com.intelliarts.starasov.sicp.pairs-alternative
  (:use clojure.test))

"Exercise 2.4.
Here is an alternative procedural representation of pairs. For this representation, verify that
(car (cons x y)) yields x for any objects x and y."

(defn cons-impl [x y]
  (fn [m] (m x y)))

(defn car [z]
  (z (fn [p q] p)))

(defn cdr [z]
  (z (fn [p q] q)))

(deftest test-cons-and-car
  (is (= 1 (car (cons-impl 1 1))))
  (is (= :s (car (cons-impl :s 1))))
  (is (= :s (car (cons-impl :s 1)))))

(deftest test-cons-and-cdr
  (is (= 1 (cdr (cons-impl 1 1))))
  (is (= :d (cdr (cons-impl :s :d)))))

(run-tests)
