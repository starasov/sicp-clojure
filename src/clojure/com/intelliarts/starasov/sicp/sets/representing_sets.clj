(ns clojure.com.intelliarts.starasov.sicp.sets.representing-sets
  (:use clojure.com.intelliarts.starasov.sicp.sets.binary-tree))

"One way to represent a set is as a list of its elements in which no element appears more than once. The
empty set is represented by the empty list. In this representation, element-of-set? is similar to the
procedure memq of section 2.3.1. It uses equal? instead of eq? so that the set elements need not be
symbols"

(defn element-of-set? [x set]
  (cond
    (or (nil? x) (empty? set)) false
    (= x (first set)) true
    :else (element-of-set? x (rest set))))

"Using this, we can write adjoin-set. If the object to be adjoined is already in the set, we just return the
set. Otherwise, we use cons to add the object to the list that represents the set:"

(defn adjoin-set [x set]
  (if (element-of-set? x set)
    set
    (cons x set)))

"For intersection-set we can use a recursive strategy. If we know how to form the intersection of
set2 and the cdr of set1, we only need to decide whether to include the car of set1 in this. But this
depends on whether (car set1) is also in set2. Here is the resulting procedure:"

(defn intersection-set [set1 set2]
  (cond
    (or (empty? set1) (empty? set2)) []
    (element-of-set? (first set1) set2) (cons (first set1) (intersection-set (rest set1) set2))
    :else (intersection-set (rest set1) set2)))

"Exercise 2.59. Implement the union-set operation for the unordered-list representation of sets."

(defn union-set [set1 set2]
  (cond
    (empty? set1) set2
    (element-of-set? (first set1) set2) (union-set (rest set1) set2)
    :else (cons (first set1) (union-set (rest set1) set2))))

"Sets as ordered lists"

(defn element-of-ordered-set? [x set]
  (cond
    (or (nil? x) (empty? set)) false
    (= x (first set)) true
    (< x (first set)) false
    :else (element-of-ordered-set? x (rest set))))

(defn intersection-ordered-set [set1 set2]
  (cond
    (or (empty? set1) (empty? set2)) []
    (= (first set1) (first set2)) (cons (first set1) (intersection-ordered-set (rest set1) (rest set2)))
    (< (first set1) (first set2)) (intersection-ordered-set (rest set1) set2)
    (> (first set1) (first set2)) (intersection-ordered-set set1 (rest set2))))

(defn union-ordered-set [set1 set2]
  (cond
    (empty? set1) set2
    (empty? set2) set1
    (= (first set1) (first set2)) (cons (first set1) (union-ordered-set (rest set1) (rest set2)))
    (< (first set1) (first set2)) (cons (first set1) (union-ordered-set (rest set1) set2))
    (> (first set1) (first set2)) (cons (first set2) (union-ordered-set set1 (rest set2)))))

(def t (make-tree
         (make-tree (make-leaf 1) 3 (make-leaf 5))
         7
         (make-tree '() 9 (make-leaf 11))))

(defn tree-to-list1 [tree]
  (if (empty? tree)
    '()
    (concat (tree-to-list1 (left-branch tree)) (cons (entry tree) (tree-to-list1 (right-branch tree))))))

(defn tree-to-list2 [tree]
  (defn copy-to-list [tree result]
    (if (empty? tree)
      result
      (copy-to-list (left-branch tree) (cons (entry tree) (copy-to-list (right-branch tree) result)))))
  (copy-to-list tree '()))

"Exercise 2.64.

The following procedure list->tree converts an ordered list to a balanced binary tree.
The helper procedure partial-tree takes as arguments an integer n and list of at least n elements and
constructs a balanced tree containing the first n elements of the list. The result returned by partial-
tree is a pair (formed with cons) whose car is the constructed tree and whose cdr is the list of
elements not included in the tree."

(defn partial-tree [elts n]
  (if (= n 0)
    (cons '() elts)
    (let [left-size (quot (- n 1) 2)
          left-result (partial-tree elts left-size)
          left-tree (first left-result)
          non-left-elts (rest left-result)
          right-size (- n (+ left-size 1))
          this-entry (first non-left-elts)
          right-result (partial-tree (rest non-left-elts) right-size)
          right-tree (first right-result)
          remaining-elts (rest right-result)]
      (cons (make-tree this-entry left-tree right-tree) remaining-elts))))

(defn list->tree [elements]
  (first (partial-tree elements (count elements))))

(println (list->tree [1 3 5 7 9 11]))

;#1 [1 3 5 7 9 11] 6
;     left-size: (6 - 1) % 2 = 2
;     left-result: #2 [ ([[] 1 []] 3 []) [5 7 9 11]]
;     left-tree: [[[] 1 []] 3 []]
;     non-left-elts: [5 7 9 11]
;     right-size: (6 - (2 + 1)) = 3
;     this-entry: 5
;     right-result: #7 [(([] 7 []) 9 ([] 11 [])) []]
;     remaining-elts: []
;     <- [(([[] 1 []] 3 []) 5 (([] 7 []) 9 ([] 11 []))) []]
;
;#2 [1 3 5 7 9 11] 2
;     left-size: (2 - 1) % 2 = 1
;     left-result: #3 [([] 1 []) [3 5 7 9 11]
;     left-tree: [[] 1 []]
;     non-left-elts: [3 5 7 9 11]
;     right-size: (2 - (1 + 1)) = 0
;     this-entry: 3
;     right-result: #5 [[] [5 7 9 11]]
;     right-tree: []
;     remaining-elts: [5 7 9 11]
;     <- [ ([[] 1 []] 3 []) [5 7 9 11]]
;
;#3 [1 3 5 7 9 11] 1
;     left-size: (1 - 1) % 2 = 0
;     left-result: #4 [[] [1 3 5 7 9 11]]
;     left-tree: []
;     non-left-elts: [1 3 5 7 9 11]
;     right-size: (1 - 1) = 0
;     this-entry: 1
;     right-result: #5 [[] [3 5 7 9 11]]
;     right-tree: []
;     remaining-elts: [3 5 7 9 11]
;     <- [([] 1 []) [3 5 7 9 11]
;
;#4 [1 3 5 7 9 11] 0
;      <- [[] [1 3 5 7 9 11]]
;
;#5 [3 5 7 9 11] 0
;      <- [[] [3 5 7 9 11]]
;
;#6 [5 7 9 11] 0
;      <- [[] [5 7 9 11]]
;
;#7 [7 9 11] 3
;     left-size: (3 - 1) % 2 = 1
;     left-result: #8 [([] 7 []) [9 11]
;     left-tree: [[] 7 []];
;     non-left-elts: [9 11]
;     right-size: (3 - (1 + 1)) = 1
;     this-entry: 9
;     right-result: #11 [([] 11 []) []]
;     right-tree: [[] 11 []]
;     remaining-elts: []
;     <- [(([] 7 []) 9 ([] 11 [])) []]
;
;#8 [7 9 11] 1
;     left-size: (1 - 1) % 2 = 0
;     left-result: #9 [[] [7 9 11]]
;     left-tree: []
;     non-left-elts: [7 9 11]
;     right-size: (1 - 1) = 0
;     this-entry: 7
;     right-result: #10 [[] [9 11]]
;     right-tree: []
;     remaining-elts: [9 11]
;     <- [([] 7 []) [9 11]]
;
;#9 [7 9 11] 0
;      <- [[] [7 9 11]]
;
;#10 [9 11] 0
;      <- [[] [9 11]]
;
;#11 [11] 1
;     left-size: (1 - 1) % 2 = 0
;     left-result: #12 [[] [11]]
;     left-tree: []
;     non-left-elts: [11]
;     right-size: (1 - 1) = 0
;     this-entry: 11
;     right-result: #13 [[] []]
;     right-tree: []
;     remaining-elts: []
;     <- [([] 11 []) []]
;
;#12 [11] 0
;      <- [[] [11]]
;
;#13 [] 0
;      <- [[] []]
;

"Exercise 2.65.

Use the results of exercises 2.63 and 2.64 to give O(n) implementations of union-set
and intersection-set for sets implemented as (balanced) binary trees."

(defn union-binary-set [set1 set2]
  (list->tree (union-ordered-set (tree-to-list1 set1) (tree-to-list1 set2))))

(defn intersection-binary-set [set1 set2]
  (list->tree (intersection-ordered-set(tree-to-list1 set1) (tree-to-list1 set2))))
