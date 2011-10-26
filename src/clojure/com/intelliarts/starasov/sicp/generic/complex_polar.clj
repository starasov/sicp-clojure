(ns clojure.com.intelliarts.starasov.sicp.generic.complex-polar
  (:use clojure.contrib.math)
  (:use clojure.com.intelliarts.starasov.sicp.generic.core)
  (:use clojure.com.intelliarts.starasov.sicp.generic.common)
  (:require [clojure.contrib.generic.math-functions :as mf]))

(defn install-polar-package []
  (defn magnitude [z]
    (first z))

  (defn angle [z]
    (second z))

  (defn make-from-mag-ang [r a]
    [r a])

  (defn real-part [z]
    (* (magnitude z) (mf/cos (angle z))))

  (defn imag-part [z]
    (* (magnitude z) (mf/sin (angle z))))

  (defn make-from-real-imag [x y]
    (cons (sqrt (+ (square x) (square y)))
      (mf/atan y x)))

  (defn tag [x]
    (attach-tag :polar x))

  (register :real-part [:polar] real-part)
  (register :imag-part [:polar] imag-part)
  (register :magnitude [:polar] magnitude)
  (register :angle [:polar] angle)

  (register :make-from-real-imag :polar
    (fn [x y] (tag (make-from-real-imag x y))))

  (register :make-from-mag-ang :polar
    (fn [r a] (tag (make-from-mag-ang r a))))

  :done)

