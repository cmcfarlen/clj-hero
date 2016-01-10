(ns hero.math.core
  (:refer-clojure :exclude [zero?])
  )

(def sqrt #? (:cljs (.-sqrt js/Math)
              :clj (fn [x] (Math/sqrt x))))

(def round #? (:cljs (.-round js/Math)
              :clj (fn [x] (Math/round x))))

(def floor #? (:cljs (.-floor js/Math)
              :clj (fn [x] (Math/floor x))))

(def abs #? (:cljs (.-abs js/Math)
             :clj (fn [x] (Math/abs x))))

(def sin #? (:cljs (.-sin js/Math)
             :clj (fn [x] (Math/sin x))))

(def cos #? (:cljs (.-cos js/Math)
             :clj (fn [x] (Math/cos x))))

(def asin #? (:cljs (.-asin js/Math)
              :clj (fn [x] (Math/asin x))))

(def acos #? (:cljs (.-acos js/Math)
              :clj (fn [x] (Math/acos x))))

(def atan2 #? (:cljs (.-atan2 js/Math)
               :clj (fn [y x] (Math/atan2 y x))))

(def π #? (:cljs (.-PI js/Math)
           :clj Math/PI))

(def τ (* 2 π))


(defn f=
  ([a b]
   (<= (abs (- a b)) 0.0000001))
  ([a b e]
   (<= (abs (- a b)) e)))

(defn zero?
  [v]
  (<= (abs v) 0.0000001))
