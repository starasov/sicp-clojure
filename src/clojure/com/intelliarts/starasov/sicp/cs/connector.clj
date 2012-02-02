(ns clojure.com.intelliarts.starasov.sicp.cs.connector)

;(defn inform-about-value [constraint]
;  (constraint :on-new-value ))
;
;(defn inform-about-no-value [constraint]
;  (constraint :on-lost-value ))
;
;(defn has-value? [connector]
;  (connector :has-value? ))
;
;(defn value [connector]
;  (connector :value ))
;
;(defn set-value! [connector new-value setter]
;  ((connector :set-value! ) new-value setter))
;
;(defn forget-value! [connector retractor]
;  ((connector :forget-value! ) retractor))
;
;(defn connect [connector constraint]
;  ((connector :connect ) constraint))
;
;(defn get-state [connector]
;  (connector :get-state ))
;
;(defn print-dump [connector]
;  (println (get-state connector)))

(defn make-connector []
  (let [state (ref {:value nil :informant nil :constraints []})]

    (defn x [new-value]
      (dosync (alter state assoc :value new-value)))

    (defn dump []
      (println state))

    (defn dispatch [request]
      (cond
        (= :set-value! request) x
        (= :get-state request) state))

    dispatch))

(def a1 (make-connector))
(def a2 (make-connector))

((a1 :set-value!) 3)
((a2 :set-value!) 3)

(println (a1 :get-state))
(println (a2 :get-state))
