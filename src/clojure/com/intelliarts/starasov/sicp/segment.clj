(ns com.intelliarts.starasov.sicp.segment)

"Ex 2.2: Consider the problem of representing line segments in a plane. Each segment is represented
as a pair of points: a starting point and an ending point. Define a constructor make-segment and
selectors start-segment and end-segment that define the representation of segments in terms of
points. Furthermore, a point can be represented as a pair of numbers: the x coordinate and the y coordinate.
Accordingly, specify a constructor make-point and selectors x-point and y-point that define this
representation. Finally, using your selectors and constructors, define a procedure midpoint-segment
that takes a line segment as argument and returns its midpoint (the point whose coordinates are the average
of the coordinates of the endpoints)."

(defn make-point [x y]
  (list x y))

(defn x-point [point]
  (first point))

(defn y-point [point]
  (last point))

(defn make-segment [start-point, end-point]
  (list start-point end-point))

(defn start-segment [segment]
  (first segment))

(defn end-segment [segment]
  (last segment))

(defn mid-point [segment]
  (let [x1 (x-point (start-segment segment))
        y1 (y-point (start-segment segment))
        x2 (x-point (end-segment segment))
        y2 (y-point (end-segment segment))]
   (make-point (/ (+ x1 x2) 2) (/ (+ y1 y2) 2))))

(def seg (make-segment (make-point 0 0) (make-point 2 2)))
(println (mid-point seg))
