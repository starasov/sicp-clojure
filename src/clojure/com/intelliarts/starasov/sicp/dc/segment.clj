(ns clojure.com.intelliarts.starasov.sicp.dc.segment)

(defn make-segments []
  (ref {}))

(defn empty-segments? [segments]
  (empty? @segments))

(defn segment-time [segment]
  (first segment))

(defn segment-queue [segment]
  (second segment))

(defn first-segment [segments]
  (let [first-segment-time (apply min (keys @segments))]
    [first-segment-time (@segments first-segment-time)]))

(defn remove-first-segment! [segments]
  (if (empty-segments? segments)
    (throw (IllegalStateException. (str "Empty segments " segments)))
    (let [segment (first-segment segments) rest-queue (rest (segment-queue segment)) time (segment-time segment)]
      (dosync
        (if (empty? rest-queue)
          (alter segments dissoc time)
          (alter segments assoc time rest-queue))))))

(defn add-to-segments! [segments time action]
  (if (nil? (@segments time))
    (dosync
      (alter segments assoc time [])
      (add-to-segments! segments time action))
    (dosync (alter segments assoc time (concat (@segments time) [action])))))
