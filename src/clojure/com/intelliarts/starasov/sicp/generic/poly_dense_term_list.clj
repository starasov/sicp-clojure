(ns clojure.com.intelliarts.starasov.sicp.generic.poly-dense-term-list
  (:use clojure.com.intelliarts.starasov.sicp.generic.common)
  (:require [clojure.com.intelliarts.starasov.sicp.generic.arithmetic :as a]))

(defn tag [x]
  (attach-tag :dense x))

(defn make-term-list [terms]
  (tag terms))

(defn add-terms [t1 t2]
  (cond
    (empty? t1) t2
    (empty? t2) t1
    :else (cons (a/add (first t1) (first t2)) (add-terms (rest t1) (rest t2)))))

(defn mul-term-by-terms [t terms]
  (if (empty? terms)
    []
    (let [current (first terms)]
      (cons (a/mul t (first terms)) (mul-term-by-terms t (rest terms))))))

(defn mul-terms [t1 t2]
  (if (empty? t1)
    []
    (add-terms (mul-term-by-terms (first t1) t2) (mul-terms (rest t1) t2))))

(defn negate-term-list [term-list]
  (if (empty? term-list)
    []
    (cons (a/negate (first term-list)) (negate-term-list (rest term-list)))))

(register :make-term-list :dense make-term-list)

(register :negate-term-list [:spare]
  (fn [t] (tag (negate-term-list t))))

(register :add-terms [:dense :dense]
  (fn [t1 t2] (tag (add-terms t1 t2))))

(register :mul-terms [:dense :dense]
  (fn [t1 t2] (tag (mul-terms t1 t2))))
