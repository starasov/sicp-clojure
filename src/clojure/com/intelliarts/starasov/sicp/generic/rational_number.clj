(ns clojure.com.intelliarts.starasov.sicp.generic.rational-number
  (:use clojure.com.intelliarts.starasov.sicp.generic.common)
  (:use clojure.contrib.math))

(defn numer [x]
  (first x))

(defn denom [x]
  (second x))

(defn tag [x]
  (attach-tag :rational-number x))

(defn make [n d]
  (let [g (gcd n d)]
    [(/ n g) (/ d g)]))

(defn add [x y]
  (make
    (+ (* (numer x) (denom y))
      (* (numer y) (denom x)))
    (* (denom x) (denom y))))

(defn sub [x y]
  (make
    (- (* (numer x) (denom y))
      (* (numer y) (denom x)))
    (* (denom x) (denom y))))

(defn mul [x y]
  (make (* (numer x) (numer y))
    (* (denom x) (denom y))))

(defn div [x y]
  (make (* (numer x) (denom y))
    (* (denom x) (numer y))))

(defn eq? [x y]
  (and (= (numer x) (numer y))
    (= (denom x) (denom y))))

(defn is-zero? [x]
  (and (= (numer x) 0)
    (= (denom x) 0)))

(register :make :rational-number
  (fn [n d] (tag (make n d))))

(register :add [:rational-number :rational-number]
  (fn [n d] (tag (add n d))))

(register :sub [:rational-number :rational-number]
  (fn [n d] (tag (sub n d))))

(register :mul [:rational-number :rational-number]
  (fn [n d] (tag (mul n d))))

(register :div [:rational-number :rational-number]
  (fn [n d] (tag (div n d))))

(register :eq? [:rational-numer :rational-number] eq?)
(register :is-zero? [:rational-numer] is-zero?)

(register :raise [:scheme-number]
  (fn [x] (tag (make x 1))))

(defn make-rational-number [n d]
  ((lookup :make :rational-number) n d))