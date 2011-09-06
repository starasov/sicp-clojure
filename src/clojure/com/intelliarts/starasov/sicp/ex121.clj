(ns com.intelliarts.starasov.sicp.ex121)

(defn find-divisor [n test-divisor]
  (cond
    (> (* test-divisor test-divisor) n) n
    (zero? (mod n test-divisor)) test-divisor
    :else (find-divisor n (inc test-divisor))))

(defn smallest-divisor [n]
  (find-divisor n 2))

(defn prime? [n]
  (zero? (mod (smallest-divisor n) n)))

(defn square [n] (* n n))

(defn check-nontrivial-sqrt [n m]
  (let [x (mod (square n) m)]
    (if (and (not (= n 1)) (not (= n (- m 1))) (= x 1))
        0
        x)))

(defn expmod [base exp m]
  (cond
    (zero? exp) 1
    (odd? exp) (mod (* base (expmod base (dec exp) m)) m)
    :else (check-nontrivial-sqrt (expmod base (/ exp 2) m) m)))

(defn fermat-test [n]
  (defn try-it [a]
    (= (expmod a n n) a))
  (def rnd (new  java.util.Random))
  (try-it (+ 1 (. rnd nextInt (dec n)))))

(defn fast-prime? [n times]
  (cond
    (zero? times) true
    (fermat-test n) (fast-prime? n (dec times))
    :else false))

; 561, 1105, 1729, 2465, 2821, and 6601
;(println 561 "prime?" (prime? 561) "fast-prime?" (fast-prime? 561 10))
;(println 1105 "prime?" (prime? 1105) "fast-prime?" (fast-prime? 1105 10))
;(println 1729 "prime?" (prime? 1729) "fast-prime?" (fast-prime? 1729 10))
;(println 2465 "prime?" (prime? 2465) "fast-prime?" (fast-prime? 2465 10))
;(println 2821 "prime?" (prime? 2821) "fast-prime?" (fast-prime? 2821 10))
;(println 6601 "prime?" (prime? 6601) "fast-prime?" (fast-prime? 6601 10))
