(ns clojure.com.intelliarts.starasov.sicp.generic.poly
  (:use clojure.com.intelliarts.starasov.sicp.generic.common)
  (:require [clojure.com.intelliarts.starasov.sicp.generic.arithmetic :as a]
            [clojure.com.intelliarts.starasov.sicp.generic.scheme-number :as sn]
            [clojure.com.intelliarts.starasov.sicp.generic.poly-dense-term-list :as dense]
            [clojure.com.intelliarts.starasov.sicp.generic.poly-spare-term-list :as spare]))

(defn tag [x]
  (attach-tag :polynominal x))

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

(defn add-terms [t1 t2]
  (apply-generic :add-terms t1 t2))

(defn mul-terms [t1 t2]
  (apply-generic :mul-terms t1 t2))

(defn negate-term-list [t]
  (apply-generic :negate-term-list t))

(defn add-poly [p1 p2]
  (if (same-variable? (variable p1) (variable p2))
    (make-poly (variable p1) (add-terms (term-list p1) (term-list p2)))
    (throw (IllegalStateException. (str "Polys not in same var - p1=" p1 " p2=" p2)))))

(defn mul-poly [p1 p2]
  (if (same-variable? (variable p1) (variable p2))
    (make-poly (variable p1) (mul-terms (term-list p1) (term-list p2)))
    (throw (IllegalStateException. (str "Polys not in same var - p1=" p1 " p2=" p2)))))

(defn negate-poly [p]
  (make-poly (variable p) (negate-term-list (term-list p))))

(register :add [:polynominal :polynominal]
  (fn [p1 p2] (tag (add-poly p1 p2))))

(register :mul [:polynominal :polynominal]
  (fn [p1 p2] (tag (mul-poly p1 p2))))

(register :sub [:polynominal :polynominal]
  (fn [p1 p2] (add-poly p1 (negate-poly p2))))

"Exercise 2.87.
Install =zero? for polynomials in the generic arithmetic package. This will allow
adjoin-term to work for polynomials with coefficients that are themselves polynomials."

(register :is-zero? [:polynominal]
  (fn [p] (empty? (term-list p))))

"Exercise 2.88.
Extend the polynomial system to include subtraction of polynomials. (Hint: You may find
it helpful to define a generic negation operation.)"

(register :negate [:polynominal]
  (fn [p] (tag (negate-poly p))))

(register :make-poly :polynominal
  (fn [variable term-list] (tag (make-poly variable term-list))))

(defn make-polynominal [variable term-list]
  ((lookup :make-poly :polynominal) variable term-list))