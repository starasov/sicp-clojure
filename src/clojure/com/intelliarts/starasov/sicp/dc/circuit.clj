(ns clojure.com.intelliarts.starasov.sicp.dc.circuit
  (:require [clojure.com.intelliarts.starasov.sicp.dc.wire :as w]))

(def inverter-delay 2)
(def and-gate-delay 3)
(def or-gate-delay 5)

(defn logical-not [x]
  (cond
    (= x 0) 1
    (= x 1) 0
    :else (throw (IllegalStateException. (str "Unsupported value '" x "' for logical not")))))

(defn inverter [input output]
  (defn invert-input []
    (let [new-value (logical-not (w/get-signal input))]
      (w/after-delay inverter-delay (fn [] (w/set-signal! output new-value)))))
  (w/add-action! input invert-input))

(defn logical-and [x1 x2]
  (if (and (= x1 x2) (= x1 1)) 1 0))

(defn and-gate [a1 a2 output]
  (defn and-proc []
    (let [new-value (logical-and (w/get-signal a1) (w/get-signal a2))]
      (w/after-delay and-gate-delay (fn [] (w/set-signal! output new-value)))))
  (w/add-action! a1 and-proc)
  (w/add-action! a2 and-proc))

"Exercise 3.28.

Define an or-gate as a primitive function box. Your or-gate constructor should be
similar to and-gate."

(defn logical-or [x1 x2]
  (if (or (= x1 1) (= x2 1)) 1 0))

(defn or-gate [a1 a2 output]
  (defn or-proc []
    (let [new-value (logical-or (w/get-signal a1) (w/get-signal a2))]
      (w/after-delay or-gate-delay (fn [] (w/set-signal! output new-value)))))
  (w/add-action! a1 or-proc)
  (w/add-action! a2 or-proc))

"Exercise 3.29.

Another way to construct an or-gate is as a compound digital logic device, built from and-
gates and inverters. Define a procedure or-gate that accomplishes this. What is the delay time of the or-
gate in terms of and-gate-delay and inverter-delay?"

(defn or-gate-2 [a1 a2 output]
  (let [a1-inverted (w/make-wire) a2-inverted (w/make-wire) pre-inverted-output (w/make-wire)]
    (inverter a1 a1-inverted)
    (inverter a2 a2-inverted)
    (and-gate a1-inverted a2-inverted pre-inverted-output)
    (inverter pre-inverted-output output)))

(defn half-adder [a b s c]
  (let [d (w/make-wire) e (w/make-wire)]
    (or-gate a b d)
    (and-gate a b c)
    (inverter c e)
    (and-gate d e s)))