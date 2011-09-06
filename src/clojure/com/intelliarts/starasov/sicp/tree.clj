(ns com.intelliarts.starasov.sicp.tree
  (:use clojure.test))

"Exercise 2.30.
Define a procedure square-tree analogous to the square-list procedure of
exercise 2.21. That is, square-list should behave as follows:

  (square-tree
   (list 1
         (list 2 (list 3 4) 5)
         (list 6 7)))
  (1 (4 (9 16) 25) (36 49))

Define square-tree both directly (i.e., without using any higher-order procedures)"

(defn square-tree-simple [tree]
  (cond
    (not (list? tree)) (* tree tree)
    (empty? tree) (list)
    :else (cons (square-tree-simple (first tree)) (square-tree-simple (rest tree)))))

(deftest square-tree-test-simple
  (def initial-list (list 1 (list 2 (list 3 4) 5) (list 6 7)))
  (def expected-list (list 1 (list 4 (list 9 16) 25) (list 36 49)))
  (is (= expected-list (square-tree-simple initial-list))))

" and also by using map and recursion."

(defn square-tree-map [tree]
  (defn square-tree [sub-tree]
    (cond
      (not (list? sub-tree)) (* sub-tree sub-tree)
      (empty? sub-tree) (list)
      :else (square-tree-map sub-tree)))
  (map square-tree tree))

(deftest square-tree-test-map
  (def initial-list (list 1 (list 2 (list 3 4) 5) (list 6 7)))
  (def expected-list (list 1 (list 4 (list 9 16) 25) (list 36 49)))
  (is (= expected-list (square-tree-map initial-list))))

"Exercise 2.31.
Abstract your answer to exercise 2.30 to produce a procedure tree-map with the property
that square-tree could be defined as:

  (define (square-tree tree) (tree-map square tree))
"
(defn tree-map [f tree]
  (defn tree-map-impl [sub-tree]
    (cond
      (not (list? sub-tree)) (f sub-tree)
      (empty? sub-tree) (list)
      :else (tree-map f sub-tree)))
  (map tree-map-impl tree))

(defn square-tree-map-2 [tree] (tree-map #(* % %) tree))

(deftest square-tree-test-map
  (def initial-list (list 1 (list 2 (list 3 4) 5) (list 6 7)))
  (def expected-list (list 1 (list 4 (list 9 16) 25) (list 36 49)))
  (is (= expected-list (square-tree-map-2 initial-list))))

"Exercise 2.32.
We can represent a set as a list of distinct elements, and we can represent the set of all
subsets of the set as a list of lists. For example, if the set is (1 2 3), then the set of all subsets is (()
(3) (2) (2 3) (1) (1 3) (1 2) (1 2 3)). Complete the following definition of a procedure
that generates the set of subsets of a set and give a clear explanation of why it works"

(defn subsets [s]
  (cond
    (number? s) s
    (empty? s) (list nil)
    :else (let [r (subsets (rest s))]
            (println r s)
            (concat r (map #(list % (first s)) r)))))

(println "result: " (subsets (list 1 2 3)))

(def r (list 1 2))
(def s (list 3 4))
;(println (concat r (map #(list % (first s))  r)))

;(run-tests)
