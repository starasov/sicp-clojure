(ns com.intelliarts.starasov.sicp.nested-mappings
  (:use clojure.test)
  (:use com.intelliarts.starasov.sicp.ex121)
  (:use com.intelliarts.starasov.sicp.sequence))

"Exercise 2.40.

Define a procedure `unique-pairs` that, given an integer n, generates the sequence of
pairs (i,j) with 1 <= j < i <= n. Use unique-pairs to simplify the definition of `prime-sum-pairs` given
above."

(defn enumerate-interval [start end]
  (defn iter-impl [current result]
    (if (>= current start) (iter-impl (dec current) (cons current result)) result))
  (iter-impl (dec end) []))

(defn flatmap [proc seq]
  (accumulate concat [] (map proc seq)))

(defn unique-pairs [n]
  (flatmap
    (fn [i] (map (fn [j] (list i j)) (enumerate-interval 1 n))) (enumerate-interval 1 n)))

(defn prime-sum? [pair]
  (prime? (+ (first pair) (second pair))))

(defn make-pair-sum [pair]
  (list (first pair) (second pair) (+ (first pair) (second pair))))

(defn prime-sum-pairs [n]
  (map make-pair-sum (filter prime-sum? (unique-pairs n))))

"Exercise 2.41.
Write a procedure to find all ordered triples of distinct positive integers i, j, and k less than
or equal to a given integer n that sum to a given integer s."

(defn unique-triplets [n]
  (flatmap
    (fn [i] (map (fn [j] (cons i j)) (unique-pairs n))) (enumerate-interval 1 n)))

(defn filter-triplets [n s]
  (filter (fn [triplet] (<= (accumulate + 0 triplet) s)) (unique-triplets n)))
