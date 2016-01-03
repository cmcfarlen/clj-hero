(ns hero.math.vec2
  (:require [hero.math.core :as math])
  )


(defprotocol Vector2
  (add [a b] "Add vectors")
  (subtract [a b] "Subtract vectors")
  (scale [a s] "Scale by the scalar s")
  (dot [a b] "Dot product")
  (normalize [a] "return a unit vector in the same direction as a")
  (magnitude [a] "return the length of a"))


(extend-protocol Vector2
  #?(:clj clojure.lang.PersistentVector
     :cljs cljs.core/PersistentVector)
  (add [[ax ay] [bx by]]
    [(+ ax bx) (+ ay by)])
  (subtract [[ax ay] [bx by]]
    [(- ax bx) (- ay by)])
  (scale [[x y] s]
    [(* x s) (* y s)])
  (dot [[ax ay] [bx by]]
    (+ (* ax bx)
       (* ay by)))
  (normalize [[x y :as a]]
    (let [m (magnitude a)]
      [(/ x m) (/ y m)]))
  (magnitude [[x y]]
    (math/sqrt (+ (* x x) (* y y)))))

;; TODO - support float arrays
