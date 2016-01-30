(ns hero.core
  (:require [hero.input.core :as input]
            [hero.input.browser :as browser]
            [hero.input.keys :as keys]
            [hero.math.vec2 :as vec2]
            [hero.math.vec3 :as vec3]
            [hero.math.core :as math :refer [π τ]]
            [hero.world.core :as world]
            [hero.render.core :as render])
  (:import [goog.events KeyCodes]))

(enable-console-print!)

(defn camera-setup
  "Setup the camera with an origin, viewable width and height and a viewport"
  [entity [x y w h] [vx vy vw vh]]
  (let [aspect-ratio (/ vw vh)
        h' (/ w aspect-ratio)
        sw (/ vw w)
        sh (/ vh h')]
    (println [sw sh] "h'" h')
    (assoc entity
           :camera/viewport [vx vy vw vh]
           :camera/origin [x y]
           :camera/scale [sw sh])))

(defn camera-pan
  [cam _ [x y]]
  (update-in cam [:camera/origin] (fn [[cx cy]] [(+ cx x) (+ cy y)])))

(defn wall
  [id p n length]
  (-> (world/entity id)
      (assoc :wall/p p)
      (assoc :wall/n (vec3/normalize n))
      (assoc :wall/length length)))


(defn game-init
  []
  (-> (world/create [10 10 10])
      (world/spawn [5 5 0] (world/entity :player))
      (world/spawn [2 2 0] (world/entity 42))
      (world/spawn [0 0 0] (-> (world/entity :camera)
                                         (camera-setup
                                          [0.0 0.0 50.0 50.0]
                                          [0 0 (/ 1920 2) (/ 1080 2)])))
      (world/spawn [3 6 0] (wall :w3 [3 6 3] [0 -1 0] 10))
      (world/spawn [3 3 0] (wall :w1 [3 3 3] [0 1 0] 10))
      (world/spawn [7 7 0] (wall :w2 [7 7 7] [-1 1 0] 20))))

(defonce game-state (atom (game-init)))

(comment
 (vec3/magnitude [1 0 0] 50)

 (world/update-entity @game-state :player world/accellerate-entity 0.016 [0 0 0])

 (world/get-entity @game-state :camera)

 )

(defn game-update
  [world input]
  (let [down (-> input :keys :down)
        mouse-down (-> input :mouse :down)
        acc (cond-> [0 0 0]
              (down keys/a)
              (vec3/add [-1 0 0])
              (down keys/d)
              (vec3/add [1 0 0])
              (down keys/s)
              (vec3/add [0 -1 0])
              (down keys/w)
              (vec3/add [0 1 0]))
        acc (vec3/magnitude acc (if (down keys/shift)
                                  500.0
                                  50.0))]
    (cond-> (world/update-entity world :player world/accellerate-entity (-> input :Δt) acc)
      (mouse-down 0)
      (world/update-entity :camera camera-pan (-> input :mouse :Δp)))))



(def game-context (.getContext (.getElementById js/document "game") "2d"))

(defonce current-input (input/recording-input (browser/browser-input) game-state))

(defn game-step
  [ts]
  (let [input (input/next-input current-input ts)]
    #_(render/scene-render game-context (world/get-entity (swap! game-state game-update input) :camera) [[:line {:stroke "#fff"} [0 0 0] [50 50 50]]])
    (render/game-render game-context (swap! game-state game-update input))))

(defn startup-game-loop
  []
  (.requestAnimationFrame js/window
                          (fn tick [ts]
                            (game-step ts)
                            (.requestAnimationFrame js/window tick))))

(defonce startup (startup-game-loop))
