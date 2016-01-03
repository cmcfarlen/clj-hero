(ns hero.math.core)

(def sqrt #? (:cljs (.-sqrt js/Math)
              :clj (fn [x] (Math/sqrt x))))

(def round #? (:cljs (.-round js/Math)
              :clj (fn [x] (Math/round x))))

(def floor #? (:cljs (.-floor js/Math)
              :clj (fn [x] (Math/floor x))))
