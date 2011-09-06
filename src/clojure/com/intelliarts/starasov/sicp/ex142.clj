(ns com.intelliarts.starasov.sicp.ex142)

; Let f and g be two one-argument functions. The composition f after g is defined to be the
; function x -> f(g(x)). Define a procedure compose that implements composition.
(defn compose [f g]
  (fn [x] (f (g x))))

(defn square [x]
  (* x x))

(println ((compose square inc) 6))


