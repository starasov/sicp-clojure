(ns clojure.com.intelliarts.starasov.sicp.generic.complex-number
  (:use clojure.com.intelliarts.starasov.sicp.generic.common)
  (:require [clojure.contrib.generic.math-functions :as mf]
            [clojure.com.intelliarts.starasov.sicp.generic.real-number :as rn]
            [clojure.com.intelliarts.starasov.sicp.generic.complex-polar :as cp]
            [clojure.com.intelliarts.starasov.sicp.generic.complex-rectangular :as cr]))

(defn angle [z]
  (apply-generic :angle z))

(defn real-part [z]
  (apply-generic :real-part z))

(defn imag-part [z]
  (apply-generic :imag-part z))

(defn magnitude [z]
  (apply-generic :magnitude z))

(defn make-from-real-imag [x y]
  ((lookup :make-from-real-imag :rectangular) x y))

(defn make-from-mag-ang [r a]
  ((lookup :make-from-mag-ang :polar) r a))

(defn -add [z1 z2]
  (make-from-real-imag (+ (real-part z1) (real-part z2))
    (+ (imag-part z1) (imag-part z2))))

(defn -sub [z1 z2]
  (make-from-real-imag (- (real-part z1) (real-part z2))
    (- (imag-part z1) (imag-part z2))))

(defn -mul [z1 z2]
  (make-from-mag-ang (* (magnitude z1) (magnitude z2))
    (+ (angle z1) (angle z2))))

(defn -div [z1 z2]
  (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
    (- (angle z1) (angle z2))))

(defn -eq? [z1 z2]
  (and (= (real-part z1) (real-part z2))
    (= (imag-part z1) (imag-part z2))))

(defn -is-zero? [x]
  (and (= (real-part x) 0)
    (= (imag-part x) 0)))

(defn tag [z]
  (attach-tag :complex z))

(register :add [:complex :complex]
  (fn [z1 z2] (tag (-add z1 z2))))

(register :sub [:complex :complex]
  (fn [z1 z2] (tag (-sub z1 z2))))

(register :mul [:complex :complex]
  (fn [z1 z2] (tag (-mul z1 z2))))

(register :div [:complex :complex]
  (fn [z1 z2] (tag (-div z1 z2))))

(register :angle [:complex] angle)
(register :real-part [:complex] real-part)
(register :imag-part [:complex] imag-part)
(register :magnitude [:complex] magnitude)
(register :eq? [:complex :complex] -eq?)
(register :is-zero? [:complex] -is-zero?)

(register :make-from-real-imag :complex
  (fn [x y] (tag (make-from-real-imag x y))))

(register :make-from-mag-ang :complex
  (fn [r a] (tag (make-from-mag-ang r a))))

(register :make-from-mag-ang :complex
  (fn [r a] (tag (make-from-mag-ang r a))))

(register :raise [:real-number]
  (fn [x] (tag (make-from-real-imag (rn/real-value x) 0.0))))

(register :raise [:scheme-number]
  (fn [x] (tag (make-from-real-imag x 0))))

(defn make-complex-from-real-imag [x y]
  ((lookup :make-from-real-imag :complex) x y))

(defn make-complex-from-mag-ang [r a]
  ((lookup :make-from-mag-ang :complex) r a))