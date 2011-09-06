(ns com.intelliarts.starasov.sicp.e111)

(defn f-rec [x]
  (if (< x 3)
    x
    (+ (f-rec (- x 1)) (* 2 (f-rec (- x 2))) (* 3 (f-rec (- x 3))))))

(defn f-iter [x-1, x-2, x-3, x, n]
  (if (= x n)
    x-3
    (f-iter (+ x-1 (* 2 x-2) (* 3 x-3)) x-1 x-2 (+ x 1) n)))

(defn f-imp [x]
  (f-iter 2 1 0 0 x))

(println (f-rec 7))
(println (f-imp 7))