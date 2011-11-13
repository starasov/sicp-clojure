(ns clojure.com.intelliarts.starasov.sicp.generic.poly-spare-term-list
  (:use clojure.com.intelliarts.starasov.sicp.generic.common)
  (:require [clojure.com.intelliarts.starasov.sicp.generic.arithmetic :as a]))

(defn tag [x]
  (attach-tag :spare x))

(defn make-term [order coef]
  [order coef])

(defn make-term-list [terms]
  (tag terms))

(defn order [term]
  (first term))

(defn coef [term]
  (second term))

(defn adjoint-term [term term-list]
  (if (zero? (coef term))
    term-list
    (cons term term-list)))

(defn add-terms [t1 t2]
  (cond
    (empty? t1) t2
    (empty? t2) t1
    :else (let [order-1 (order (first t1)) coef-1 (coef (first t1))
                order-2 (order (first t2)) coef-2 (coef (first t2))]
            (cond
              (= order-1 order-2) (adjoint-term (make-term order-1 (+ coef-1 coef-2)) (add-terms (rest t1) (rest t2)))
              (< order-1 order-2) (adjoint-term (first t2) (add-terms t1 (rest t2)))
              (> order-1 order-2) (adjoint-term (first t1) (add-terms (rest t1) t2))))))

(defn mul-term-by-terms [t terms]
  (if (empty? terms)
    []
    (let [current (first terms)]
      (adjoint-term (make-term (+ (order t) (order current)) (* (coef t) (coef current))) (mul-term-by-terms t (rest terms))))))

(defn mul-terms [t1 t2]
  (if (empty? t1)
    []
    (add-terms (mul-term-by-terms (first t1) t2) (mul-terms (rest t1) t2))))

(defn negate-term-list [term-list]
  (if (empty? term-list)
    []
    (cons (make-term (order (first term-list)) (- (coef (first term-list)))) (negate-term-list (rest term-list)))))

(defn sub-terms [t-list1 t-list2]
  (add-terms t-list1 (negate-term-list t-list2)))

(defn div-terms [t-list1 t-list2]
  (do
    (if (empty? t-list1)
      (list [] [])
      (let [t1 (first t-list1) t2 (first t-list2)]
        (if (> (order t2) (order t1))
          (list [] t-list1)
          (let [new-o (- (order t1) (order t2))
                new-c (/ (coef t1) (coef t2))
                rest-of-result (div-terms (sub-terms t-list1 (mul-term-by-terms (make-term new-o new-c) t-list2)) t-list2)]
              (list (adjoint-term (make-term new-o new-c) (first rest-of-result)) (second rest-of-result))))))))

(println (div-terms [(make-term 5 1) (make-term 0 -1)] [(make-term 2 1) (make-term 0 -1)]))

(register :make-term :spare make-term)
(register :make-term-list :spare make-term-list)

(register :negate-term-list [:spare]
  (fn [t] (tag (negate-term-list t))))

(register :add-terms [:spare :spare]
  (fn [t1 t2] (tag (add-terms t1 t2))))

(register :mul-terms [:spare :spare]
  (fn [t1 t2] (tag (mul-terms t1 t2))))
