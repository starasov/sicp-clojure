(ns com.intelliarts.starasov.sicp.list
  (:use clojure.test))

"Exercise 2.17.
Define a procedure last-pair that returns the list that contains only the last element of a
given (nonempty) list."

(defn last-pair [l]
  (defn iter [current, tail]
    (if
      (nil? (first tail)) current
      (iter (first tail) (rest tail))))
  (iter nil l))

(deftest test-last-pair
  (is (nil? (last-pair (list))))
  (is (= 1 (last-pair (list 1))))
  (is (= 3 (last-pair (list 1 2 3)))))


"Exercise 2.18.
Define a procedure reverse that takes a list as argument and returns a list of the same
elements in reverse order."

(defn reverse-impl [l]
  (defn iter [tail]
    (if
      (empty? tail) (list)
      (concat (iter (rest tail)) (list (first tail)))))
  (iter l))

(deftest test-reverse-impl
  (is (= (list) (reverse-impl (list))))
  (is (= (list 1) (reverse-impl (list 1))))
  (is (= (list 3 2 1) (reverse-impl (list 1 2 3)))))


"Exercise 2.20.
Write a procedure same-parity that takes one or more integers and returns a list of
all the arguments that have the same even-odd parity as the first argument. For example,
(same-parity 1 2 3 4 5 6 7)
(1 3 5 7)
(same-parity 2 3 4 5 6 7)
(2 4 6)"

(defn same-parity [x & args]
  (defn same-parity? [x y] (= (mod x 2) (mod y 2)))
  (defn iter [l]
    (cond
      (empty? l) (list)
      (same-parity? x (first l)) (cons (first l) (iter (rest l)))
      :else (iter (rest l))))
  (cons x (iter args)))

(deftest test-same-parity
  (is (= (list 1) (same-parity 1)))
  (is (= (list 1 3) (same-parity 1 2 3)))
  (is (= (list 2 4) (same-parity 2 3 4 5))))

"Exercise 2.21.
The procedure square-list takes a list of numbers as argument and returns a list of the
squares of those numbers."

(defn square-list-1 [items]
  (if (empty? items)
    nil
    (cons (* (first items) (first items)) (square-list-1 (rest items)))))

(defn square-list-2 [items]
  (map #(* % %) items))

(deftest test-square-list
  (is (= (list 1 4 9) (square-list-1 (list 1 2 3))))
  (is (= (list 1 4 9) (square-list-2 (list 1 2 3)))))

"Exercise 2.23.
The procedure for-each is similar to map. It takes as arguments a procedure and a list of
elements. However, rather than forming a list of the results, for-each just applies the procedure to each
of the elements in turn, from left to right. The values returned by applying the procedure to the elements are
not used at all -- for-each is used with procedures that perform an action, such as printing. For example,

`(for-each (lambda (x) (newline) (display x)) (list 57 321 88))
  57
  321
  88`

The value returned by the call to for-each (not illustrated above) can be something arbitrary, such as
true. Give an implementation of for-each."

(defn for-each [f items]
  (if (not (empty? items))
    (do (f (first items))
      (for-each f (rest items)))))

"Exercise 2.25.
Give combinations of cars and cdrs that will pick 7 from each of the following lists."

(deftest test-7
  (def ex1 (list 1 3 (list 5 7) 9))
  (def ex2 (list (list 7)))
  (def ex3 (list 1 (list 2 (list 3 (list 4 (list 5 (list 6 7)))))))
  (is (= 7 (first (rest (first (rest (rest ex1)))))))
  (is (= 7 (first (first ex2))))
  (is (= 7 (first (rest (first (rest (first (rest (first (rest (first (rest (first (rest ex3)))))))))))))))

"Exercise 2.26.
Suppose we define x and y to be two lists:
`(define x (list 1 2 3))`
`(define y (list 4 5 6))`
What result is printed by the interpreter in response to evaluating each of the following expressions:
(append x y)
(cons x y)
(list x y)"

(deftest text-ops
  (def x (list 1 2 3))
  (def y (list 4 5 6))
  (is (= (list 1 2 3 4 5 6) (concat x y)))
  (is (= (list (list 1 2 3) 4 5 6) (cons x y)))
  (is (= (list (list 1 2 3) (list 4 5 6)) (list x y))))

"Exercise 2.27.
Modify your reverse procedure of exercise 2.18 to produce a deep-reverse
procedure that takes a list as argument and returns as its value the list with its elements reversed and with all
sublists deep-reversed as well."

(defn deep-reverse [l]
  (defn iter [tail]
    (cond
      (not (list? tail)) tail
      (empty? tail) ()
      :else (concat (iter (rest tail)) (list (iter (first tail))))))
  (iter l))

(deftest test-deep-reverse
  (is (= (list 3 2 1) (deep-reverse (list 1 2 3))))
  (is (= (list (list 4 3) (list 2 1)) (deep-reverse (list (list 1 2) (list 3 4))))))

"Exercise 2.28.
Write a procedure fringe that takes as argument a tree (represented as a list) and returns a
list whose elements are all the leaves of the tree arranged in left-to-right order. For example,
(define x (list (list 1 2) (list 3 4)))
(fringe x)
(1 2 3 4)
(fringe (list x x))
(1 2 3 4 1 2 3 4)"

(defn fringe [x]
  (defn iter [tail]
    (cond
      (not (list? tail)) (list tail)
      (empty? tail) ()
      :else (concat (iter (first tail)) (iter (rest tail)))))
  (iter x))

(deftest test-fringe
  (def x (list (list 1 2) (list 3 (list 4))))
  (is (= (list 1 2 3 4) (fringe x)))
  (is (= (list 1 2 3 4 1 2 3 4)) (fringe (list x x))))

(run-tests)