(ns clojure.com.intelliarts.starasov.sicp.dc.wire
  (:require [clojure.com.intelliarts.starasov.sicp.dc.agenda :as a]))

(def the-agenda (a/make-agenda))

(defn make-wire []
  (ref {:signal 0 :actions []}))

(defn get-signal [wire]
  (@wire :signal 0))

(defn set-signal! [wire signal]
  (if (not (= (get-signal wire) signal))
    (do
      (dosync (alter wire assoc :signal signal))
      (doall (for [a (@wire :actions)] (a))))))

(defn add-action! [wire action]
  (dosync (alter wire assoc :actions (cons action (@wire :actions))))
  (action))

(defn after-delay [delay proc]
  (a/add-to-agenda! the-agenda (+ delay (a/current-time the-agenda)) proc))

(defn propagate []
  (if (not (a/empty-agenda? the-agenda))
    (let [first-item (a/first-agenda-item the-agenda)]
      (first-item)
      (a/remove-first-agenda-item! the-agenda)
      (propagate))))