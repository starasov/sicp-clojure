(ns clojure.com.intelliarts.starasov.sicp.generic.arithmetic
  (:use clojure.com.intelliarts.starasov.sicp.generic.common))

(defn add [x y] (apply-generic :add x y))
(defn sub [x y] (apply-generic :sub x y))
(defn mul [x y] (apply-generic :mul x y))
(defn div [x y] (apply-generic :div x y))
(defn exp [x y] (apply-generic :exp x y))
(defn raise [x] (apply-generic :raise x))

"Exercise 2.79.
Define a generic equality predicate equ? that tests the equality of two numbers, and install
it in the generic arithmetic package. This operation should work for ordinary numbers, rational numbers,
and complex numbers."
(defn eq? [x y] (apply-generic :eq? x y))

"Exercise 2.80.
Define a generic predicate =zero? that tests if its argument is zero, and install it in the
generic arithmetic package. This operation should work for ordinary numbers, rational numbers, and complex numbers."
(defn is-zero? [x] (apply-generic :is-zero? x))

