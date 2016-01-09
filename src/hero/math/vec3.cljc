(ns hero.math.vec3
  (:require [hero.math.core :as math])
  (:refer-clojure :exclude [divide zero?])
  )

(defprotocol Vector3
  (add [a b] "Add vectors")
  (subtract [a b] "Subtract vectors")
  (scale [a s] "Scale by the scalar s")
  (dot [a b] "Dot product")
  (normalize [a] "return a unit vector in the same direction as a")
  (magnitude [a] [a m] "return the length of a or return a vector with the magnitude m in the same direction as a")
  (multiply [a b] "entrywise (hadamard) product")
  (divide [a b] "entrywise (hadamard) division")
  (cross [a b] "vector cross product")
  (zero? [a] "return true if the vector is zero"))


(extend-protocol Vector3
  #?(:clj clojure.lang.PersistentVector
     :cljs cljs.core/PersistentVector)
  (add [[ax ay az] [bx by bz]]
    [(+ ax bx) (+ ay by) (+ az bz)])
  (subtract [[ax ay az] [bx by bz]]
    [(- ax bx) (- ay by) (- az bz)])
  (scale [[x y z] s]
    [(* x s) (* y s) (* z s)])
  (dot [[ax ay az] [bx by bz]]
    (+ (* ax bx)
       (* ay by)
       (* az bz)))
  (normalize [[x y z :as a]]
    (let [m (magnitude a)]
      [(/ x m) (/ y m) (/ z m)]))
  (magnitude
    ([[x y z]]
     (math/sqrt (+ (* x x) (* y y) (* z z))))
    ([a m]
     (let [mag (magnitude a)]
       (if (= mag 0)
         a
         (scale a (/ m mag))))))
  (multiply [[ax ay az] [bx by bz]]
    [(* ax bx) (* ay by) (* az bz)])
  (divide [[ax ay az] [bx by bz]]
    [(/ ax bx) (/ ay by) (/ az bz)])
  (cross [[ax ay az] [bx by bz]]
    ;;|i  j  k |
    ;;|ax ay az| = |ay az|    |ax az|    |ax ay|
    ;;|bx by bz|   |by bz|i - |bx bz|j + |bx by|k
    [(- (* ay bz) (* az by)) (- (- (* ax bz) (* az bx))) (- (* ax by) (* ay bx))])
  (zero? [[ax ay az]]
    (and (math/zero? ax)
         (math/zero? ay)
         (math/zero? az))))

(defn component
  "Return the component of a onto b (a scalar)"
  [a b]
    (dot a (normalize b)))

(defn angle
  "Return the angle between a and b in radians"
  [a b]
  (math/acos (/ (dot a b) (* (magnitude a) (magnitude b)))))

(comment
 (angle [1 0 0] [0 -1 0])

 )

;; TODO - support float arrays


