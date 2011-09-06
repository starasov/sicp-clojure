(ns com.intelliarts.starasov.sicp.sequence
  (:use clojure.test))

(defn accumulate [op initial seq]
  (if (empty? seq)
    initial
    (op (first seq) (accumulate op initial (rest seq)))))

"Exercise 2.33.
Fill in the missing expressions to complete the following definitions of some basic list-
manipulation operations as accumulations:

`(define (map p sequence)
  (accumulate (lambda (x y) <??>) nil sequence))`"

(defn map-impl [p sequence]
  (accumulate (fn [x y] (cons (p x) y)) nil sequence))

(deftest map-impl-test
  (is (= (list 1 4 9) (map-impl #(* % %) (list 1 2 3))))
  (is (= nil (map-impl #(* % %) (list)))))

"`(define (append seq1 seq2)
  (accumulate cons <??> <??>))`"

(defn append-impl [seq1 seq2]
  (accumulate cons seq2 seq1))

(deftest append-impl-test
  (is (= (list 1 2 3 4 5 6) (append-impl (list 1 2 3) (list 4 5 6)))))

"`(define (length sequence)
  (accumulate <??> 0 sequence))`"

(defn length-impl [sequence]
  (accumulate (fn [x, y] (+ 1 y)) 0 sequence))

(deftest length-impl-test
  (is (= 3 (length-impl (list 1 2 3)))))

"Exercise 2.34.
Evaluating a polynomial in x at a given value of x can be formulated as an accumulation.
We evaluate the polynomial using a well-known algorithm called Horner's rule, which structures the computation as
In other words, we start with `a_n`, multiply by `x`, add `a_(n-1)`, multiply by `x`, and so on, until we reach a 0.
Fill in the following template to produce a procedure that evaluates a polynomial using Horner's rule. Assume
that the coefficients of the polynomial are arranged in a sequence, from `a_0` through `a_n`.

`(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms) <??>)
              0
              coefficient-sequence))`

For example, to compute 1 + 3x + 5x3 + x5 at x = 2 you would evaluate:

`(horner-eval 2 (list 1 3 0 5 0 1))`"

(defn horner-eval [x coefficient-sequence]
  (accumulate (fn [this-coeff higher-terms] (+ this-coeff (* higher-terms x)))
              0
              coefficient-sequence))

(deftest horner-eval-test
  (is (= 1 (horner-eval 2 [1])))
  (is (= 3 (horner-eval 2 [1 1])))
  (is (= 79 (horner-eval 2 [1 3 0 5 0 1]))))

"Exercise 2.35.
Redefine count-leaves from section 2.2.2 as an accumulation."

(defn count-leaves [t]
  (accumulate + 0 (map (fn [x] (if (list? x) (count-leaves x) x)) t)))

(deftest count-leaves-test
  (def t (list 1 (list 2 (list 3 4) 5) (list 6 7)))
  (is (= 28 (count-leaves t))))

"Exercise 2.36.
The procedure accumulate-n is similar to accumulate except that it takes as its third
argument a sequence of sequences, which are all assumed to have the same number of elements. It applies
the designated accumulation procedure to combine all the first elements of the sequences, all the second
elements of the sequences, and so on, and returns a sequence of the results. For instance, if s is a sequence
containing four sequences, ((1 2 3) (4 5 6) (7 8 9) (10 11 12)), then the value of
(accumulate-n + 0 s) should be the sequence (22 26 30). Fill in the missing expressions in the
following definition of accumulate-n:

`(define (accumulate-n op init seqs)
  (if (null? (car seqs))      nil
      (cons (accumulate op init <??>)
            (accumulate-n op init <??>))))`"

(defn accumulate-n [op init seqs]
  (if (empty? (first seqs)) (list)
      (cons (accumulate op init (map first seqs))
            (accumulate-n op init (map rest seqs)))))

(deftest accumulate-n-test
  (def s [[1 2 3] [4 5 6] [7 8 9] [10 11 12]])
  (is (= [22 26 30] (accumulate-n + 0 s))))

"Exercise 2.37.
Suppose we represent vectors v = (v_i) as sequences of numbers, and matrices m = (m_ij) as
sequences of vectors (the rows of the matrix).
For example, the matrix

[1 2 3 4
 4 5 6 6
 6 7 8 9]

is represented as the sequence ((1 2 3 4) (4 5 6 6) (6 7 8 9)). With this representation, we
can use sequence operations to concisely express the basic matrix and vector operations."

(defn dot-product [v w]
  (accumulate + 0 (map * v w)))

(defn matrix-*-vector [m v]
  (map #(dot-product % v) m))

(deftest matrix-*-vector-test
  (def m [[1 2 3 4] [4 5 6 6] [6 7 8 9]])
  (def v [1 1 1 1])
  (is (= [10 21 30] (matrix-*-vector m v))))

(defn transpose [mat]
  (accumulate-n cons (list) mat))

(deftest transpose-test
  (def m [[1 2 3 4] [4 5 6 6] [6 7 8 9]])
  (def expected [[1 4 6] [2 5 7] [3 6 8] [4 6 9]])
  (is (= expected (transpose m))))

(defn matrix-*-matrix [m n]
  (let [cols (transpose n)]
    (map #(matrix-*-vector cols %) m)))

(deftest matrix-*-matrix-test
  (def a [[0 4 -2] [-4 -3 0]])
  (def b [[0 1] [1 -1] [2 3]])
  (def expected [[0 -10] [-3 -1]])
  (is (= expected (matrix-*-matrix a b))))

"Exercise 2.38.
The accumulate procedure is also known as fold-right, because it combines the
first element of the sequence with the result of combining all the elements to the right. There is also a fold-
left, which is similar to fold-right, except that it combines elements working in the opposite
direction"

(defn fold-left [op initial sequence]
  (defn iter [result tail]
    (if (empty? tail)
        result
        (iter (op result (first tail))
              (rest tail))))
  (iter initial sequence))

(def fold-right accumulate)

(fold-right / 1 (list 1 2 3))
(fold-left / 1 (list 1 2 3))
(fold-right list nil (list 1 2 3))
(fold-left list nil (list 1 2 3))

"Exercise 2.39.
Complete the following definitions of reverse (exercise 2.18) in terms of fold-right
and fold-left from exercise 2.38"

(defn rreverse [sequence]
  (fold-right (fn [x y] (concat y [x])) (list) sequence))

(deftest rreverse-test
  (is (= [3 2 1] (rreverse [1 2 3]))))

(defn lreverse [sequence]
  (fold-left (fn [x y] (cons y x)) (list) sequence))

(deftest lreverse-test
  (is (= [3 2 1] (lreverse [1 2 3]))))

;(run-tests)