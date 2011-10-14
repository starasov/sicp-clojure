(ns clojure.com.intelliarts.starasov.sicp.picture-language)

(defn right-split [painter n]
  """
    +----+----+
    |    | p1 |
    | p0 |----|
    |    | p1 |
    +----+----+
  """
  (if (= n 0)
      painter
      (let [smaller (right-split painter (- n 1))]
        (beside painter (below smaller smaller)))))

"Exercise 2.44.

Define the procedure up-split used by corner-split. It is similar to right-
split, except that it switches the roles of below and beside."

(defn up-split [painter n]
  """
    +----+----+
    | p1 | p2 |
    +----+----+
    |    p0   |
    +----+----+
  """
  (if (= n 0)
      painter
      (let [smaller (up-split painter (- n 1))]
        (below painter (beside smaller smaller)))))

"Exercise 2.45.

Right-split and up-split can be expressed as instances of a general splitting
operation. Define a procedure split with the property that evaluating:

`(define right-split (split beside below))`
`(define up-split (split below beside))`

"

(defn split [combine-op small-op]
  (defn split-impl [painter n]
    (if (= n 0)
      painter
      (let [smaller (split-impl painter (dec n))]
        (combine-op (small-op smaller smaller)))))
  split-impl)

(def right-split-2 (split beside below))
(def up-split-2 (split below beside))

"Exercise 2.46.

A two-dimensional vector v running from the origin to a point can be represented as a pair
consisting of an x-coordinate and a y-coordinate. Implement a data abstraction for vectors by giving a
constructor make-vect and corresponding selectors xcor-vect and ycor-vect. In terms of your
selectors and constructor, implement procedures add-vect, sub-vect, and scale-vect that perform
the operations vector addition, vector subtraction, and multiplying a vector by a scalar."

(defn make-vect [x y]
  (list x y))

(defn xcor-vect [v]
  (first v))

(defn ycor-vect [v]
  (second v))

(defn add-vect [v1 v2]
  (make-vect (+ (xcor-vect v1) (xcor-vect v2)) (+ (ycor-vect v1) (ycor-vect v2))))

(defn sub-vect [v1 v2]
  (make-vect (- (xcor-vect v1) (xcor-vect v2)) (- (ycor-vect v1) (ycor-vect v2))))

(defn scale-vect [k v]
  (make-vect (* k (xcor-vect v)) (* k (ycor-vect v))))

"Exercise 2.47.

Here are two possible constructors for frames:

`(define (make-frame origin edge1 edge2)
  (list origin edge1 edge2))`

`(define (make-frame origin edge1 edge2)
  (cons origin (cons edge1 edge2)))`

For each constructor supply the appropriate selectors to produce an implementation for frames."

(defn make-frame [origin edge1 edge2]
  (list origin edge1 edge2))

(defn origin-frame [f]
  (first f))

(defn edge1-frame [f]
  (second f))

(defn edge2-frame [f]
  (last f))

(defn make-frame-2 [origin edge1 edge2]
  (cons origin (cons edge1 edge2)))

(defn origin-frame-2 [f]
  (first f))

(defn edge1-frame-2 [f]
  (second f))

(defn edge2-frame-2 [f]
  (rest (rest f)))

"Exercise 2.48.

A directed line segment in the plane can be represented as a pair of vectors -- the vector
running from the origin to the start-point of the segment, and the vector running from the origin to the end-
point of the segment. Use your vector representation from exercise 2.46 to define a representation for
segments with a constructor make-segment and selectors start-segment and end-segment. "

(defn make-segement [start end]
  (list start end))

(defn start-segment [segment]
  (first segment))

(defn end-segment [segment]
  (second segment))

(defn draw-line [start end]
  (println "line ->" "start" start "end" end))

(defn frame-coord-map [frame]
  (fn [v]
    (add-vect
     (origin-frame frame)
     (add-vect (scale-vect (xcor-vect v)
                           (edge1-frame frame))
               (scale-vect (ycor-vect v)
                           (edge2-frame frame))))))

(defn segments->painter [segment-list]
  (fn [frame]
    (map
     (fn [segment]
       (draw-line
        ((frame-coord-map frame) (start-segment segment))
        ((frame-coord-map frame) (end-segment segment))))
     segment-list)))

(defn outline [frame]
  (segments->painter [(make-segement (make-vect 0 0) (make-vect 0 1))
                      (make-segement (make-vect 0 1) (make-vect 1 1))
                      (make-segement (make-vect 1 1) (make-vect 1 0))
                      (make-segement (make-vect 1 0) (make-vect 0 0))]))

(defn x-shape [frame]
  (segments->painter [(make-segement (make-vect 0 0) (make-vect 1 1))
                      (make-segement (make-vect 0 1) (make-vect 1 0))]))

(defn midpoints [frame]
  (segments->painter [(make-segement (make-vect 0 0.5) (make-vect 0.5 1))
                      (make-segement (make-vect 0.5 1) (make-vect 1 0.5))
                      (make-segement (make-vect 1 0.5) (make-vect 0.5 0))
                      (make-segement (make-vect 0.5 0) (make-vect 0 0.5))]))

(defn transform-painter [painter origin corner1 corner2]
  (fn [frame]
    (let [m (frame-coord-map frame)]
      (let [new-origin (m origin)]
        (painter
         (make-frame new-origin
                     (sub-vect (m corner1) new-origin)
                     (sub-vect (m corner2) new-origin)))))))

"Exercise 2.50.

Define the transformation flip-horiz, which flips painters horizontally, and
transformations that rotate painters counterclockwise by 180 degrees and 270 degrees."

(defn flip-horiz [painter]
  (transform-painter painter
    (make-vect 1.0 0.0)
    (make-vect 0.0 0.0)
    (make-vect 1.0 1.0)))

(defn rotate180 [painter]
  (transform-painter painter
    (make-vect 0.0 0.0)
    (make-vect 1.0 1.0)
    (make-vect 0.0 0.0)))

(defn rotate270 [painter]
  (transform-painter painter
    (make-vect 0.0 0.0)
    (make-vect 1.0 1.0)
    (make-vect 0.0 0.0)))

(defn beside [painter1 painter2]
  """
    +----+----+
    | p1 | p2 |
    +----+----+
  """
  (let [split-point (make-vect 0.5 0.0)]
    (let [paint-left
           (transform-painter painter1
                              (make-vect 0.0 0.0)
                              split-point
                              (make-vect 0.0 1.0))
          paint-right
           (transform-painter painter2
                              split-point
                              (make-vect 1.0 0.0)
                              (make-vect 0.5 1.0))]
      (fn [frame]
        (paint-left frame)
        (paint-right frame)))))

(defn below-1 [p1 p2]
  """
    +----+
    | p1 |
    |----|
    | p2 |
    +----+
  """
  (let [split-point (make-vect 0.0 0.5)]
    (let [paint-top
           (transform-painter painter1
                              (make-vect 0.0 0.0)
                              (make-vect 0.0 1.0)
                              split-point)
          paint-bottom
           (transform-painter painter2
                              split-point
                              (make-vect 0.5 1.0)
                              (make-vect 0.0 1.0))]
      (fn [frame]
        (paint-top frame)
        (paint-bottom frame)))))

(defn below-2 [p1 p2]
  """
    +----+
    | p1 |
    |----|
    | p2 |
    +----+
  """
  (rotate180 (beside (rotate270 p2) (rotate270 p1))))
