(ns clojure.com.intelliarts.starasov.sicp.sets.sets-and-information-retrieval
  (:use clojure.com.intelliarts.starasov.sicp.sets.binary-tree)
  (:use clojure.test))

"Exercise 2.66.

Implement the lookup procedure for the case where the set of records is structured as a
binary tree, ordered by the numerical values of the keys."

(defn make-tree-entry [key value]
  (list key value))

(defn entry-key [entry]
  (first entry))

(defn value [entry]
  (second entry))

(defn lookup-binary-tree [tree k]
  (cond
    (empty? tree) false
    (< (entry-key (entry tree)) k) (lookup-binary-tree (right-branch tree) k)
    (> (entry-key (entry tree)) k) (lookup-binary-tree (left-branch tree) k)
    (= (entry-key (entry tree)) k) (entry tree)))

(deftest lookup-binary-tree-test
  (def t (make-tree
           (make-tree-entry 7 "seven")
           (make-tree (make-tree-entry 3 "three") (make-leaf (make-tree-entry 1 "one")) (make-leaf (make-tree-entry 5 "five")))
           (make-tree (make-tree-entry 10 "ten") (make-leaf (make-tree-entry 8 "eight")) (make-leaf (make-tree-entry 12 "twelve")))))
  (is (false? (lookup-binary-tree t 2)))
  (is (= (list 7 "seven") (lookup-binary-tree t 7)))
  (is (= (list 1 "one") (lookup-binary-tree t 1)))
  (is (= (list 12 "twelve") (lookup-binary-tree t 12))))

;(run-all-tests)