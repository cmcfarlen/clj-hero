(ns hero.math.vec2-test
  (:require [clojure.test :refer :all]
            [hero.math.core :as math]
            [hero.math.vec2 :as vec2]))

(deftest test-vec2-persistent-vector
  (testing "vec2"
    (is (= (vec2/add [1 1] [1 1]) [2 2]))
    (is (= (vec2/subtract [1 1] [1 1]) [0 0]))
    (is (= (vec2/scale [1 1] 4) [4 4]))
    (is (math/f= (-> [3 5]
                     vec2/normalize
                     vec2/magnitude) 1))))
