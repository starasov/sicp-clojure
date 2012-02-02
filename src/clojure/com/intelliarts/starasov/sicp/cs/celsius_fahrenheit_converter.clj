(ns clojure.com.intelliarts.starasov.sicp.cs.celsius-fahrenheit-converter
  (:require [clojure.com.intelliarts.starasov.sicp.cs.connector :as c]
            [clojure.com.intelliarts.starasov.sicp.cs.blocks :as b]))

;(defn celsius-fahrenheit-converter [c f]
;  (let [u (c/make-connector)
;        v (c/make-connector)
;        w (c/make-connector)
;        x (c/make-connector)
;        y (c/make-connector)]
;    (b/multiplier c w u)
;    (b/multiplier v x u)
;    (b/adder v y f)
;    (b/constant 9 w)
;    (b/constant 5 x)
;    (b/constant 32 y)))
;
;(def C (c/make-connector))
;(def F (c/make-connector))
;
;(b/probe C "Celsius temp")
;(b/probe F "Fahrenheit temp")
;(celsius-fahrenheit-converter C F)

;(def a1 (c/make-connector "a1"))
;(def a2 (c/make-connector "a2"))

;(def a1-state (ref {:id :a1 :value nil :informant nil :constraints []}))
;(def a2-state (ref {:id :a2 :value nil :informant nil :constraints []}))

(def a1 (c/make-connector))
(def a2 (c/make-connector))
;(def sum (c/make-connector))

;(b/constant 3 a1)
;(b/constant 5 a2)
;(b/probe sum "Sum`")
;(b/adder a1 a2 sum)

(c/set-value! a1 3 :a)
(c/set-value! a2 5 :a)

(c/print-dump a1)
(c/print-dump a2)

