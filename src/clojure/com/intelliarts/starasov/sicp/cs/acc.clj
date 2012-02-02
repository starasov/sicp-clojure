(ns clojure.com.intelliarts.starasov.sicp.cs.acc)

(defn accumulator []
  (let [x (ref 0)]
    (fn []
      (dosync (alter x inc)))))

(defn make-connector []
  (let [z (ref 0)]
    (defn add [] (dosync (ref-set z (inc @z))))
    add)
  (let [state (ref {:value nil :informant nil :constraints []})]

    (defn x []
      (dosync (ref-set state {}))
      state)

    (defn dump []
      (println state))

    (defn dispatch [request]
      (cond
        (= :x request) x
        (= :dump request) dump))

    dispatch))


;(def my-accumulator-1 (accumulator))
;(def my-accumulator-2 (accumulator))
;(println (my-accumulator-1))
;(println (my-accumulator-1))
;(println (my-accumulator-2))
;(println (my-accumulator-2))

(def c1 (make-connector))
(def c2 (make-connector))

(println ((c1 :x )))
(println ((c2 :x )))

((c1 :dump ))
((c2 :dump ))

