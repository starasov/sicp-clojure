(ns clojure.com.intelliarts.starasov.sicp.quotation)

(defn memq [item x]
  "Takes two arguments, a symbol and a list. If the symbol is not contained in the list (i.e., is not eq? to any
  item in the list), then memq returns false. Otherwise, it returns the sublist of the list beginning with the first
  occurrence of the symbol."
  (cond
    (empty? x) false
    (= item (first x)) x
    :else (memq item (rest x))))

;(println (memq 'apple '(pear banana prune)))
;(println (memq 'apple '(x (apple sauce) y apple pear)))

(println "(list 'a 'b 'c) - " (list 'a 'b 'c))
(println "'(a b c) - " '(a b c))
(println "(list (list 'george)) - " (list (list 'george)))
(println "(first '((x1 x2) (y1 y2))) - " (first '((x1 x2) (y1 y2))))
(println "(rest '((x1 x2) (y1 y2))) - " (rest '((x1 x2) (y1 y2))))
(println "(list? (first '(a short list))) - " (list? (first '(a short list))))

"Exercise 2.54.

Two lists are said to be equal? if they contain equal elements arranged in the same order.

For example,

 `(equal? '(this is a list) '(this is a list))`

is true, but

`(equal? '(this is a list) '(this (is a) list))`

is false.

To be more precise, we can define equal? recursively in terms of the basic eq? equality of
symbols by saying that a and b are equal? if they are both symbols and the symbols are eq?, or if they
are both lists such that (car a) is equal? to (car b) and (cdr a) is equal? to (cdr b). Using
this idea, implement equal? as a procedure."

(defn equal? [seq1 seq2]
  (defn rest-impl [seq]
    (if (empty? seq) nil (rest seq)))

  (defn are-both-seq? [seq1 seq2]
    (and (list? seq1) (list? seq2)))

  (if
    (are-both-seq? seq1 seq2) (and (equal? (first seq1) (first seq2)) (equal? (rest-impl seq1) (rest-impl seq2)))
    (= seq1 seq2)))

(println (equal? '(1 (2) 3) '(1 (2) 3)))

"Exercise 2.55.
Eva Lu Ator types to the interpreter the expression:

(println ''abracadabra)

To her surprise, the interpreter prints back quote. Explain."

(println ''abracadabra)
(println (quote (quote abracadabra)))