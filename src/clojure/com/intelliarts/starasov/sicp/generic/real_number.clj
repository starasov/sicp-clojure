(ns clojure.com.intelliarts.starasov.sicp.generic.real-number
  (:use clojure.com.intelliarts.starasov.sicp.generic.common)
  (:require [clojure.com.intelliarts.starasov.sicp.generic.rational-number :as ratn]))

(defn real-value [x]
  (first x))

(defn make-real [x]
  ((lookup :make-real :real-number) x))

(defn tag [x]
  (attach-tag :real-number x))

(defn make [x]
  [(float x)])

(register :add [:real-number :real-number]
  (fn [x y] (tag (make (+ (first x) (first y))))))

(register :sub [:real-number :real-number]
  (fn [x y] (tag (- x y))))

(register :mul [:real-number :real-number]
  (fn [x y] (tag (* x y))))

(register :div [:real-number :real-number]
  (fn [x y] (tag (/ x y))))

(register :eq? [:real-number :real-number] =)
(register :is-zero? [:real-number] zero?)

(register :raise [:rational-number]
  (fn [x] (tag (make (/ (ratn/numer x) (ratn/denom x))))))

(register :make-real :real-number
  (fn [x] (tag (make x))))

(register :raise [:scheme-number]
  (fn [x] (tag (make x))))