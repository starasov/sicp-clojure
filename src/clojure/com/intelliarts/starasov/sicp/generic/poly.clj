(ns clojure.com.intelliarts.starasov.sicp.generic.poly
  (:use clojure.com.intelliarts.starasov.sicp.generic.common)
  (:require clojure.com.intelliarts.starasov.sicp.generic.arithmetic :as a))

(defn make-poly [variable term-list]
  [variable term-list])

(defn variable [poly]
  (first poly))

(defn term-list [poly]
  (second poly))

(defn variable? [v]
  (symbol? v))

(defn same-variable? [v1 v2]
  (and (symbol? v1) (symbol? v2) (= v1 v2)))

(defn adjoint-term [term term-list]
  (if (zero? (coef term))
    term-list
    (cons term term-list)))

(defn first-term [term-list]
  (first term-list))

(defn rest-terms [term-list]
  (rest term-list))

(defn make-term [order coef]
  [order coef])

(defn order [term]
  (first term))

(defn coef [term]
  (second term))

(defn add-terms [t1 t2]
  (cond
    (empty? t1) t2
    (empty? t2) t1
    (= (first t1) (first t2)) (cons (make-term (order t1) (a/add (coef t1) (coef t2))) (add-terms (rest t1) (rest t2)))
    (< (first t1) (first t2)) (adjoint-term (first t2) (add-terms t1 (rest t2)))
    (> (first t1) (first t2)) (adjoint-term (first t1) (add-terms (rest t1) t2))))

(defn mul-term-by-terms [t terms]
  (if (empty? terms)
    []
    (let [current (first terms)]
      (adjoint-term (make-term (+ (order t) (order current)) (a/mul (coef t) (coef current)))))))

(defn mul-terms [t1 t2]
  (cond
    (empty? t1) []
    (add-terms (mul-term-by-terms (first t1) t2) (mul-terms (rest t1) t2))))

(defn add-poly [p1 p2]
  (if (same-variable? (variable p1) (variable p2))
    (make-poly (variable p1) (add-terms (term-list p1) (term-list p2)))
    (throw (IllegalStateException. (str "Polys not in same var - p1=" p1 " p2=" p2)))))

(defn mul-poly [p1 p2]
  (if (same-variable? (variable p1) (variable p2))
    (make-poly (variable p1) (mul-terms (term-list p1) (term-list p2)))
    (throw (IllegalStateException. (str "Polys not in same var - p1=" p1 " p2=" p2)))))
