(ns clojure.com.intelliarts.starasov.sicp.dc.agenda
  (:require [clojure.com.intelliarts.starasov.sicp.dc.segment :as s]))

(defn make-agenda []
  (ref {:current-time 0 :segments (s/make-segments)}))

(defn empty-agenda? [agenda]
  (s/empty-segments? (@agenda :segments)))

(defn current-time [agenda]
  (@agenda :current-time))

(defn set-current-time! [agenda time]
  (dosync (alter agenda assoc :current-time time)))

(defn first-agenda-item [agenda]
  (if (empty-agenda? agenda)
    (throw (IllegalStateException. (str "Empty agenda " agenda)))
    (let [front (s/first-segment (@agenda :segments))]
      (set-current-time! agenda (s/segment-time front))
      (dosync (alter agenda assoc :current-time (s/segment-time front)))
      (first (s/segment-queue front)))))

(defn remove-first-agenda-item! [agenda]
  (s/remove-first-segment! (@agenda :segments)))

(defn add-to-agenda! [agenda time action]
  (s/add-to-segments! (@agenda :segments) time action))