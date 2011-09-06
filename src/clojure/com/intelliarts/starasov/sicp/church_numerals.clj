(ns com.intelliarts.starasov.sicp.church-numerals)

(defn S
  ([& args] (do (println "1") #())))

(defn Z
  ([& args] (do (println "0"))))

(defn zero
  [& args]
  (fn [f] (fn [x] x)))

(defn add-1 [n]
  (fn [f] (fn [x] (f ((n f) x)))))

(defn add-2 [n]
  (fn [f] (fn [x] (f (f ((n f) x))))))

(defn two-impl []
  (fn [f] (fn [x] (f (f (x x))))))

(defn one-impl []
  (fn [f] (fn [x] (f (x x)))))

;((((zero) S) Z))
;((((add-1 zero) S) Z))
;((((add-2 zero) S) Z))
;((((two-impl) S) Z))
;((((one-impl) S) Z))
