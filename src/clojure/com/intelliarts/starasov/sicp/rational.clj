(ns com.intelliarts.starasov.sicp.rational
  (:use clojure.contrib.math))

(defn gcd-impl [a b]
  (if (= b 0)
      a
      (gcd b (mod a b))))

(defn adjust-rat-sign [n d]
  (cond
    (and (< n 0) (< d 0)) (list (abs n) (abs d))
    (or (< n 0) (< d 0)) (list (- (abs n)) (abs d))
    :else (list n d)))

(defn make-rat [n d]
  (let [g (gcd-impl n d) [a b] (adjust-rat-sign n d)]
    (list (/ a g) (/ b g))))

(defn numer [x]
  (first x))

(defn denom [x]
  (last x))

(defn print-rat [x]
  (println (numer x) "/" (denom x)))

(print-rat (make-rat 3 3))
(print-rat (make-rat -3 3))
(print-rat (make-rat 3 -3))
(print-rat (make-rat -3 -3))


