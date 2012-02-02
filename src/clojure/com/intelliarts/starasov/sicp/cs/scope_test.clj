(ns clojure.com.intelliarts.starasov.sicp.cs.scope-test)

(defn value [connector]
  (connector :value ))

(defn set-value! [connector new-value]
  ((connector :set-value! ) new-value))

(defn -make-connector [state]
  (defn -set-value! [new-value]
    (dosync (alter state assoc :value new-value)))

  (defn -dispatch [request]
    (cond
      (= :value request) (@state :value )
      (= :set-value! request) -set-value!
      :else (throw (IllegalStateException. (str "Unsupported connector operation " request)))))

  -dispatch)

(defn make-connector []
  (-make-connector (ref {:value nil :informant nil :constraints []})))

(def c1 (make-connector))
(set-value! c1 3)

(def c2 (make-connector))
(set-value! c2 4)

(println (value c1))
(println (value c2))

