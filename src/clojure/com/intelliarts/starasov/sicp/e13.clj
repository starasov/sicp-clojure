(ns com.intelliarts.starasov.sicp.e13)

(defn max-squares
  [a, b, c]
  (cond
    (and (< a b) (< a c)) (+ (* b b) (* c c))
    (and (< b a) (< b c)) (+ (* a a) (* c c))
    :else (+ (* a a) (* b b))))

(defn max-squares-2
  [a, b, c]
  (let [nums [a, b, c]]
    (reduce + (map (fn [x] (* x x)) (rest (sort nums))))))

(println (max-squares 1 2 3))
(println (max-squares-2 1 2 3))
