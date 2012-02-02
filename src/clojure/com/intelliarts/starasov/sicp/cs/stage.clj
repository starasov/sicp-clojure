(ns clojure.com.intelliarts.starasov.sicp.cs.stage)

(defn make-connector []
  (let [state (ref {:value nil :informant nil :constraints []})]
    (println "initial state" state)

;    (defn -has-value? []
;      (not (nil? (@state :informant ))))

;    (defn -inform [except proc]
;      (doall (for [c (@state :constraints ) :when (not (= c except))] (proc c))))

    (defn -set-value! [new-value setter]
;      (cond
;        (not (-has-value?))
        (dosync
          (println "set value state" state)
          (alter state assoc :value new-value)))
;          (alter state assoc :informant setter)
;          (-inform setter inform-about-value)
;)
;        (not (= (@state :value ) new-value))
;        (throw (IllegalStateException. (str "Contradiction " (@state :value ) " " new-value)))))

;    (defn -forget-value! [retractor]
;      (if (= (@state :informant ) retractor)
;        (dosync
;          (alter state assoc :informant nil)
;          (-inform retractor inform-about-no-value))))

;    (defn -connect [constraint]
;      (if (not (some #(= %1 constraint) (@state :constraints )))
;        (dosync (alter state assoc :constraints (cons constraint (@state :constraints )))))
;      (if (-has-value?)
;        (do
;          (println "[inform-about-value] state=" @state)
;          (inform-about-value constraint))))

    (defn -dispatch [request]
      (cond
;        (= :has-value? request) (-has-value?)
;        (= :value request) (@state :value )
        (= :get-state request) state
        (= :set-value! request) -set-value!
;        (= :forget-value! request) -forget-value!
;        (= :connect request) -connect
        :else (throw (IllegalStateException. (str "Unsupported connector operation " request)))))

    -dispatch))

