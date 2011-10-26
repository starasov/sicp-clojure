(ns clojure.com.intelliarts.starasov.sicp.generic.scheme_number
  (:use clojure.com.intelliarts.starasov.sicp.generic.common))

(defn install-scheme-number-package []

  (register :add [:scheme-number :scheme-number] +)
  (register :sub [:scheme-number :scheme-number] -)
  (register :mul [:scheme-number :scheme-number] *)
  (register :div [:scheme-number :scheme-number] /)
  (register :eq? [:scheme-number :scheme-number] =)
  (register :is-zero? [:scheme-number] zero?)

  :done)

(install-scheme-number-package)