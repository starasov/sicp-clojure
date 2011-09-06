(ns com.intelliarts.starasov.sicp.ex122
  (:use clojure.contrib.math)
  (:use com.intelliarts.starasov.sicp.ex121))

;(time (take 1000 (for [n (range 1 1000) :when (prime? n)] n)))
;(time (take 1000 (for [n (range 1000 10000) :when (prime? n)] n)))
;(time (take 1000 (for [n (range 10000 100000) :when (prime? n)] n)))
;(time (take 1000 (for [n (range 100000 1000000) :when (prime? n)] n)))

(time (take 10000 (for [n (range 1000000 10000000) :when (prime? n)] n)))
(time (take 10000 (for [n (range 10000000 100000000) :when (prime? n)] n)))
(time (take 10000 (for [n (range 100000000 1000000000) :when (prime? n)] n)))
(time (take 10000 (for [n (range 1000000000 10000000000) :when (prime? n)] n)))
(time (take 10000 (for [n (range 10000000000 100000000000) :when (prime? n)] n)))
