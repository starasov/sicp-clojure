(ns clojure.com.intelliarts.starasov.sicp.sets.huffman-tree
  (:use clojure.test))

(defn third [list]
  (first (rest (rest list))))

(deftest third-test
  (is (= 3 (third [1 2 3]))))

(defn make-leaf [symbol weight]
  (list 'leaf symbol weight))

(defn leaf? [object]
  (= (first object) 'leaf))

(deftest leaf?-test
  (is (leaf? (make-leaf 'A 1)))
  (is (not (leaf? (list '() '() 'A 1)))))

(defn symbol-leaf [x]
  (second x))

(deftest symbol-leaf-test
  (def leaf (make-leaf 'A 1))
  (is (= 'A (symbol-leaf leaf))))

(defn weight-leaf [x]
  (last x))

(deftest weight-leaf-test
  (def leaf (make-leaf 'A 1))
  (is (= 1 (weight-leaf leaf))))

(defn symbols [tree]
  (if (leaf? tree)
    (list (symbol-leaf tree))
    (third tree)))

(defn weight [tree]
  (if (leaf? tree)
    (third tree)
    (last tree)))

(defn left-branch [tree]
  (first tree))

(defn right-branch [tree]
  (second tree))

(defn make-code-tree [left right]
  (list left
    right
    (concat (symbols left) (symbols right))
    (+ (weight left) (weight right))))

(defn choose-branch [bit branch]
  (cond (= bit 0) (left-branch branch)
    (= bit 1) (right-branch branch)
    :else (throw (RuntimeException. (str "Bad bit value: " bit)))))

(defn adjoin-set [x set]
  (cond (empty? set) (list x)
    (< (weight x) (weight (first set))) (cons x set)
    :else (cons (first set) (adjoin-set x (rest set)))))

(defn make-leaf-set [pairs]
  (if (empty? pairs)
    '()
    (let [pair (first pairs) symbol (first pair) frequency (second pair)]
      (adjoin-set (make-leaf symbol frequency)
        (make-leaf-set (rest pairs))))))

"The following procedure implements the decoding algorithm. It takes as arguments a list of zeros and ones,
together with a Huffman tree."

(defn decode [bits tree]
  (defn decode-impl [bits subtree]
    (if (empty? bits)
      '()
      (let [branch (choose-branch (first bits) subtree)]
        (println)
        (if (leaf? branch)
          (cons (symbol-leaf branch) (decode-impl (rest bits) tree))
          (decode-impl (rest bits) branch)))))
  (decode-impl bits tree))

(def sample-tree (make-code-tree (make-leaf 'A 4)
                   (make-code-tree
                     (make-leaf 'B 2)
                     (make-code-tree
                       (make-leaf 'D 1)
                       (make-leaf 'C 1)))))

(def sample-message [0 1 1 0 0 1 0 1 0 1 1 1 0])

"Exercise 2.67.
Define an encoding tree and a sample message"

(deftest decode-test
  (is (= '(A D A B B C A) (decode sample-message sample-tree))))

"Exercise 2.68.

The encode procedure takes as arguments a message and a tree and produces the list of
bits that gives the encoded message."

(defn encode-character [character tree]
  (if (leaf? tree)
    '()
    (let [left (left-branch tree) right (right-branch tree)]
      (if (some #(= % character) (symbols left))
        (cons 0 (encode-character character left))
        (cons 1 (encode-character character right))))))

(defn encode [message tree]
  (if (empty? message)
    '()
    (concat (encode-character (first message) tree) (encode (rest message) tree))))

(deftest encode-test
  (is (= sample-message (encode '(A D A B B C A) sample-tree))))

"Exercise 2.69.  The following procedure takes as its argument a list of symbol-frequency pairs (where no
symbol appears in more than one pair) and generates a Huffman encoding tree according to the Huffman
algorithm.

(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

Make-leaf-set is the procedure given above that transforms the list of pairs into an ordered set of
leaves. Successive-merge is the procedure you must write, using make-code-tree to successively
merge the smallest-weight elements of the set until there is only one element left, which is the desired
Huffman tree. (This procedure is slightly tricky, but not really complicated. If you find yourself designing a
complex procedure, then you are almost certainly doing something wrong. You can take significant
advantage of the fact that we are using an ordered set representation.)"

(def pairs [['A 4] ['B 2] ['C 1] ['D 1]])

(defn successive-merge [leaf-set]
  (let [small-right (first leaf-set) small-left (second leaf-set)]
    (cond
      (nil? small-left) (first (adjoin-set small-right (rest leaf-set)))
      :else (successive-merge (adjoin-set (make-code-tree small-left small-right) (rest (rest leaf-set)))))))

(defn generate-huffman-tree [pairs]
  (successive-merge (make-leaf-set pairs)))

"Exercise 2.70.

The following eight-symbol alphabet with associated relative frequencies was designed to
efficiently encode the lyrics of 1950s rock songs. (Note that the ``symbols`` of an ``alphabet` need
not be individual letters).

A    2 NA  16
BOOM 1 SHA  3
GET  2 YIP  9
JOB  2 WAH  1

Use generate-huffman-tree (exercise 2.69) to generate a corresponding Huffman tree, and use
encode (exercise 2.68) to encode the following message:

Get a job
Sha na na na na na na na na
Get a job
Sha na na na na na na na na
Wah yip yip yip yip yip yip yip yip yip
Sha boom"

(def lyrics-pairs [['A 2] ['NA 16] ['BOOM 1] ['SHA 3] ['GET 2] ['YIP 9] ['JOB 2] ['WAH 1]])
(def lyrics-tree (generate-huffman-tree lyrics-pairs))
(def lyrics-message ['GET 'A 'JOB
                     'SHA 'NA 'NA 'NA 'NA 'NA 'NA 'NA 'NA
                     'GET 'A 'JOB
                     'SHA 'NA 'NA 'NA 'NA 'NA 'NA 'NA 'NA
                     'WAH 'YIP 'YIP 'YIP 'YIP 'YIP 'YIP 'YIP 'YIP 'YIP
                     'SHA 'BOOM])
(def lyrics-encoded (encode lyrics-message lyrics-tree))

(println (reduce str lyrics-encoded))
(println lyrics-message)

;(run-all-tests)