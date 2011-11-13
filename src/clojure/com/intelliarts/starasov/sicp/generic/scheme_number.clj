(ns clojure.com.intelliarts.starasov.sicp.generic.scheme-number
  (:use clojure.com.intelliarts.starasov.sicp.generic.common))

(register :add [:scheme-number :scheme-number] +)
(register :sub [:scheme-number :scheme-number] -)
(register :mul [:scheme-number :scheme-number] *)
(register :div [:scheme-number :scheme-number] /)
(register :eq? [:scheme-number :scheme-number] =)
(register :is-zero? [:scheme-number] zero?)
(register :negate [:scheme-number] -)