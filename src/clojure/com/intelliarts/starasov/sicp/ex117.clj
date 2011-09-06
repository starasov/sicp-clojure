(ns com.intelliarts.starasov.sicp.ex117)

(defn halve-num [a]
  (quot a 2))

(defn double-num [a]
  (* 2 a))

(defn fast?-mult [a b]
  (cond
    (< b 2) a
    (odd? b) (+ a (fast?-mult a (dec b)))
    :else (double-num (fast?-mult a (halve-num b)))))

(defn fast?-mult-iter [acc a b]
  (cond
    (< b 1) acc
    (odd? b) (fast?-mult-iter (+ acc a) a (dec b))
    :else (fast?-mult-iter (+ (double-num acc) a) a (halve-num b))))

(println (* 33 32))
(println (fast?-mult 33 32))
(println (fast?-mult-iter 0 33 32))
