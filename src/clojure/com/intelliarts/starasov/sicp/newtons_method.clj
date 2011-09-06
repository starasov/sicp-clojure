(ns com.intelliarts.starasov.sicp.newtons-method
  (:use com.intelliarts.starasov.sicp.fixed-point))

(def dx 0.00001)

; Derivative procedure
(defn deriv [g]
  (fn [x]
    (/ (- (g (+ x dx)) (g x))
       dx)))

(defn newton-transform [g]
  (fn [x]
    (- x (/ (g x) ((deriv g) x)))))

; Newton's method as a fixed-point process
(defn newtons-method [g guess]
  (fixed-point (newton-transform g) guess))

; Square-root procedure based on Newton's method.
(defn sqrt-newtons [x]
  (newtons-method (fn [y] (- (* y y) x)) 1.0))

; Cubic procedure x3 + ax2 + bx + c
(defn cubic [a b c]
  (fn [x] (+ (* x x x) (* a x x) (* b x) c)))

; Approximates square root with Newton's method
(println (sqrt-newtons 2))

; Approximates zeros of the cubic with Newton's method.
(println (newtons-method (cubic 1 1 1) 1.0))



