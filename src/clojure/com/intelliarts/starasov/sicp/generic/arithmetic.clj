(ns clojure.com.intelliarts.starasov.sicp.generic.arithmetic
  (:use clojure.com.intelliarts.starasov.sicp.generic.common))

(defn add [x y] (apply-generic :add x y))
(defn sub [x y] (apply-generic :sub x y))
(defn mul [x y] (apply-generic :mul x y))
(defn div [x y] (apply-generic :div x y))
(defn eq? [x y] (apply-generic :eq? x y))
(defn is-zero? [x] (apply-generic :is-zero? x))

