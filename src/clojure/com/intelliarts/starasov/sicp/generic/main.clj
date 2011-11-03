(ns clojure.com.intelliarts.starasov.sicp.generic.main
  (:require [clojure.com.intelliarts.starasov.sicp.generic.arithmetic :as a]
            [clojure.com.intelliarts.starasov.sicp.generic.complex-number :as cn]
            [clojure.com.intelliarts.starasov.sicp.generic.scheme-number :as sn]
            [clojure.com.intelliarts.starasov.sicp.generic.rational-number :as ratn]
            [clojure.com.intelliarts.starasov.sicp.generic.real-number :as realn]))

(println (a/add (cn/make-complex-from-real-imag 1 1) 1))
;(println (add (make-real 1) (make-rational-number 1 1)))
