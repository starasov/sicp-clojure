(ns com.intelliarts.starasov.sicp.binary-mobile
  (:use clojure.test))

"Exercise 2.29.  A binary mobile consists of two branches, a left branch and a right branch. Each branch is a
rod of a certain length, from which hangs either a weight or another binary mobile. We can represent a
binary mobile using compound data by constructing it from two branches (for example, using list):

(define (make-mobile left right)
  (list left right))

A branch is constructed from a length (which must be a number) together with a structure, which
may be either a number (representing a simple weight) or another mobile:

(define (make-branch length structure)
  (list length structure))

a.  Write the corresponding selectors left-branch and right-branch, which return the branches of a
mobile, and branch-length and branch-structure, which return the components of a branch."

(defn make-mobile [left right]
  (list left right))

(defn left-branch [mobile]
  (first mobile))

(defn right-branch [mobile]
  (last mobile))

(defn make-branch [lenght structure]
  (list lenght structure))

(defn branch-length [branch]
  (first branch))

(defn branch-structure [branch]
  (last branch))

(defn leaf-structure? [structure]
  (not (list? structure)))

"b.  Using your selectors, define a procedure total-weight that returns the total weight of a mobile."

(defn total-weight [m]
    (if
      (leaf-structure? m) m
      (+ (total-weight (branch-structure (left-branch m))) (total-weight (branch-structure (right-branch m))))))

(deftest test-total-weight
  (def mobile (make-mobile
                (make-branch 1
                  (make-mobile
                    (make-branch 1 2)
                    (make-branch 1 3)))
                (make-branch 1 4)))
  (is (= 9 (total-weight mobile))))

"c. A mobile is said to be balanced if the torque applied by its top-left branch is equal to that applied by its
top-right branch (that is, if the length of the left rod multiplied by the weight hanging from that rod is equal
to the corresponding product for the right side) and if each of the submobiles hanging off its branches is
balanced. Design a predicate that tests whether a binary mobile is balanced."

(defn momentum [m]
    (if
      (leaf-structure? m) m
      (let [left-len (branch-length (left-branch m))
            left-struct (branch-structure (left-branch m))
            right-len (branch-length (right-branch m))
            right-struct (branch-structure (right-branch m))]
        (+ (* left-len (momentum left-struct)) (* right-len (momentum right-struct))))))

(defn is-balanced? [m]
  (let [left-len (branch-length (left-branch m))
        left-struct (branch-structure (left-branch m))
        right-len (branch-length (right-branch m))
        right-struct (branch-structure (right-branch m))]
    (= (* left-len (momentum left-struct)) (* right-len (momentum right-struct)))))

(deftest test-is-balanced?
  (def balanced-mobile (make-mobile
                (make-branch 1
                  (make-mobile
                    (make-branch 1 2)
                    (make-branch 1 2)))
                (make-branch 1 4)))
  (def unbalanced-mobile (make-mobile
                (make-branch 2
                  (make-mobile
                    (make-branch 1 2)
                    (make-branch 1 2)))
                (make-branch 1 4)))
  (is (is-balanced? balanced-mobile))
  (is (not (is-balanced? unbalanced-mobile))))

(run-tests)