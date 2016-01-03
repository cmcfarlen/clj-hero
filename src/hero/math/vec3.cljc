(ns hero.math.vec3
  (:require [hero.math.core :as math]))

(defprotocol Vector3
  (add [a b] "Add vectors")
  (subtract [a b] "Subtract vectors")
  (scale [a s] "Scale by the scalar s")
  (dot [a b] "Dot product")
  (normalize [a] "return a unit vector in the same direction as a")
  (magnitude [a] "return the length of a")
  (multiply [a b] "pointwise product"))


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
  (magnitude [[x y z]]
    (math/sqrt (+ (* x x) (* y y) (* z z))))
  (multiply [[ax ay az] [bx by bz]]
    [(* ax bx) (* ay by) (* az bz)]))

;; TODO - support float arrays
