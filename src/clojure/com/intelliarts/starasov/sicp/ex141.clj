(ns com.intelliarts.starasov.sicp.ex141)

; Define a procedure double that takes a procedure of one argument as argument and
; returns a procedure that applies the original procedure twice.
(defn double-f [f]
  (fn [x] (f (f x))))

(println ((double-f inc) 1))
(println (((double-f (double-f double-f)) inc) 5))
