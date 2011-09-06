(ns com.intelliarts.starasov.sicp.ex116)

(defn fast-expt [b n]
  (cond
    (< n 2) b
    (odd? n) (* b (fast-expt b (dec n)))
    :else (* (fast-expt b (quot n 2)) (fast-expt b (quot n 2)))))

(defn fast-expt-iter [a b n]
  (cond
    (< n 2) a
    (odd? n) (fast-expt-iter (* a b) b (dec n))
    :else (fast-expt-iter (* a b b) b (quot n 2))))

(println (fast-expt-iter 1 2 5))
(println (fast-expt 2 5))
