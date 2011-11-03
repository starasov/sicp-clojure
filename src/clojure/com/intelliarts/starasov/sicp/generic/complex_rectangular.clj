(ns clojure.com.intelliarts.starasov.sicp.generic.complex-rectangular
  (:use clojure.contrib.math)
  (:use clojure.com.intelliarts.starasov.sicp.generic.core)
  (:use clojure.com.intelliarts.starasov.sicp.generic.common)
  (:require [clojure.contrib.generic.math-functions :as mf]))

(defn real-part [z]
  (first z))

(defn imag-part [z]
  (second z))

(defn make-from-real-imag [x y]
  [x y])

(defn magnitude [z]
  (sqrt (+ (square (real-part z))
          (square (imag-part z)))))

(defn angle [z]
  (mf/atan (imag-part z) (real-part z)))

(defn make-from-mag-ang [r a]
  [(* r (mf/cos a)) (* r (mf/sin a))])

(defn tag [x]
  (attach-tag :rectangular x))

(register :real-part [:rectangular] real-part)
(register :imag-part [:rectangular] imag-part)
(register :magnitude [:rectangular] magnitude)
(register :angle [:rectangular] angle)

(register :make-from-real-imag :rectangular
  (fn [x y] (tag (make-from-real-imag x y))))

(register :make-from-mag-ang :rectangular
  (fn [r a] (tag (make-from-mag-ang r a))))