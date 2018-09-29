(ns hero.math.box
  (:require [hero.math.core :as math]))

(defprotocol IVolume
  (point-inside? [v pt] "return true if the point is inside")
  (centroid [v] "return the central point of this volume"))

(defprotocol IAABB
  (left [b] "return the left (lowest x)")
  (right [b] "return the right (highest x)")
  (bottom [b] "return the bottom (lowest y)")
  (top [b] "return the top (highest y)")
  (back [b] "return the front (lowest z)")
  (front [b] "return the front (highest z)"))

(defrecord AABB [x0 y0 z0 x1 y1 z1]
  IAABB
  (left [_] x0)
  (right [_] x1)
  (bottom [_] y0)
  (top [_] y1)
  (back [_] z0)
  (front [_] z1)
  IVolume
  (point-inside? [_ [x y z]]
    (and (<= x0 x x1)
         (<= y0 y y1)
         (<= z0 z z1)))
  (centroid [_]
    [(/ (+ x0 x1) 2)
     (/ (+ y0 y1) 2)
     (/ (+ z0 z1) 2)]))
