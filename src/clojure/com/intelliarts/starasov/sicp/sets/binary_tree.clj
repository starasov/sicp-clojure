(ns clojure.com.intelliarts.starasov.sicp.sets.binary-tree)

(defn entry [tree]
  (first tree))

(defn left-branch [tree]
  (second tree))

(defn right-branch [tree]
  (first (rest (rest tree))))

(defn make-tree [entry left right]
  (list entry left right))

(defn make-leaf [entry]
  (make-tree entry '() '()))

