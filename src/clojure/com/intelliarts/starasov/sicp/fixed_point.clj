(ns com.intelliarts.starasov.sicp.fixed-point
  (:use clojure.contrib.math))

(def tolerance 0.00001)
(defn noop-trace [next])

(defn fixed-point
  ([f first-guess] (fixed-point f first-guess noop-trace))
  ([f first-guess trace]
    (defn close-enough? [v1 v2]
      (< (abs (- v1 v2)) tolerance))
    (defn try-it [guess]
      (let [next (f guess)]
        (do
          ;(trace next)
          (if (close-enough? guess next)
              next
              (try-it next)))))
    (try-it first-guess)))



(defn -main [& args]
  (do
    ; Golden ratio finding by  x -> 1 + 1/x fixed point approximation.
    ; @see SICP Ex. 1.35
    (fixed-point #(double (+ 1 (/ 1 %))) 1.0 #(println %))

    ; Solution to x^x = 1000 by finding a fixed point of x -> log(1000)/log(x)
    ; @see SICP Ex. 1.36
    (fixed-point #(/ (Math/log 1000) (Math/log %)) 1.1 #(println %))))
