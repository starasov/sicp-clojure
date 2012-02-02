(ns clojure.com.intelliarts.starasov.sicp.dc.simulation
  (:require [clojure.com.intelliarts.starasov.sicp.dc.wire :as w]
            [clojure.com.intelliarts.starasov.sicp.dc.circuit :as c]
            [clojure.com.intelliarts.starasov.sicp.dc.agenda :as a]))

(defn probe [wire name]
  (defn probe-proc []
    (println (str "[" name "] " "time: " (a/current-time w/the-agenda) ", signal: " (w/get-signal wire))))
  (w/add-action! wire probe-proc))

(def a (w/make-wire))
(def b (w/make-wire))
(def s (w/make-wire))
(def c (w/make-wire))

(probe s "sum")
(probe c "carry")

(c/half-adder a b s c)

(w/set-signal! a 1)
(w/propagate)

(w/set-signal! b 1)
(w/propagate)
