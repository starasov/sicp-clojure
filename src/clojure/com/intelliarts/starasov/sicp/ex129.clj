(ns com.intelliarts.starasov.sicp.ex129)

(defn cube [x] (* x x x))

(defn accumulate [combiner null-value term a next b]
  (if (> a b)
    null-value
    (combiner (term a) (accumulate combiner null-value term (next a) next b))))

(defn accumulate-iter [combiner null-value term a next b]
  (defn iter [a result]
    (if (> a b)
        result
        (iter (next a) (combiner result (term a)))))
  (iter a null-value))

(defn sum-acc [term a next b]
  (accumulate + 0 term a next b))

(defn product-acc [term a next b]
  (accumulate * 1 term a next b))

(defn sum-acc-iter [term a next b]
  (accumulate-iter + 0 term a next b))

(defn product-acc-iter [term a next b]
  (accumulate-iter * 1 term a next b))

(defn sum [term a next b]
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(defn sum-iter [term a next b]
  (defn iter [a result]
    (if (> a b)
        result
        (iter (next a) (+ result (term a)))))
  (iter a 0))

(defn product [term a next b]
  (if (> a b)
      1
      (* (term a)
         (product term (next a) next b))))

(defn product-iter [term a next b]
  (defn iter [a result]
    (if (> a b)
        result
        (iter (next a) (* result (term a)))))
  (iter a 1))

(defn integral [f a b dx]
  (defn add-dx [x] (+ x dx))
  (* (sum-iter f (+ a (/ dx 2.0)) add-dx b) dx))

(defn integral-simpsons [f a b dx n]
  (def h (/ (- b a) n))
  (defn fk [k] (f (+ a (* k h))))
  (defn sum-f [idx]
    (cond
      (odd? idx) (* 4 (fk idx))
      :else (* 2 (fk idx))))

  (* (+ (f a) (sum-iter sum-f 0 inc n) (f b)) (/ h 3.0)))


(defn factorial [x]
  (product identity 1 inc x))

(defn pi-calc [n]
  (defn num-next [x] (+ x (mod x 2)))
  (defn div-next [x] (inc (num-next x)))
  (double (/ (product num-next 1 inc n) (product div-next 0 inc (dec n)))))
