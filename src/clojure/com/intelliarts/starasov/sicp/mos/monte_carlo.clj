(ns clojure.com.intelliarts.starasov.sicp.mos.monte-carlo
  (:use clojure.contrib.math))

(defn random-in-range [low high]
  (let [range (- high low)]
    (+ low (rand-int range))))

(defn cesaro-test []
   (= (gcd (rand-int 2000000) (rand-int 2000000)) 1))

(defn monte-carlo [trials experiment]
  (defn iter [trials-remaining trials-passed]
    (cond
      (= trials-remaining 0) (/ trials-passed trials)
      (experiment) (iter (- trials-remaining 1) (+ trials-passed 1))
      :else (iter (- trials-remaining 1) trials-passed)))
  (iter trials 0))

(defn estimate-pi [trials]
  (sqrt (/ 6 (monte-carlo trials cesaro-test))))

"Exercise 3.5.

Monte Carlo integration is a method of estimating definite integrals by means of Monte
Carlo simulation. Consider computing the area of a region of space described by a predicate P(x, y) that is
true for points (x, y) in the region and false for points not in the region. For example, the region contained
within a circle of radius 3 centered at (5, 7) is described by the predicate that tests whether (x - 5)2 + (y -
7)2< 32. To estimate the area of the region described by such a predicate, begin by choosing a rectangle
that contains the region. For example, a rectangle with diagonally opposite corners at (2, 4) and (8, 10)
contains the circle above. The desired integral is the area of that portion of the rectangle that lies in the
region. We can estimate the integral by picking, at random, points (x,y) that lie in the rectangle, and testing
P(x, y) for each point to determine whether the point lies in the region. If we try this with many points,
then the fraction of points that fall in the region should give an estimate of the proportion of the rectangle
that lies in the region. Hence, multiplying this fraction by the area of the entire rectangle should produce
an estimate of the integral."

(defn estimate-integral [x1 y1 x2 y2 trials pred]
  (defn integral-experiment []
    (let [x-rand (random-in-range x2 x1) y-rand (random-in-range y2 y1)]
      (pred x-rand y-rand)))
  (* (- x2 x1) (- y2 y1) (monte-carlo trials integral-experiment)))

(defn predicate [x y]
  (<= (+ (* (- x 5) (- x 5)) (* (- y 7) (- y 7))) (* 3 3)))

(println (float (estimate-integral 2 4 8 10 2000 predicate)))