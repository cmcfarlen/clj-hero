(ns hero.core
  (:require [hero.input.core :as input]
            [hero.input.browser :as browser]
            [hero.input.keys :as keys])
  (:import [goog.events KeyCodes]))

(enable-console-print!)


(defn camera-setup
  "Setup the camera with an origin, viewable width and height and a viewport"
  [[x y w h] [vx vy vw vh]]
  (let [aspect-ratio (/ vw vh)
        h' (/ w aspect-ratio)
        sw (/ vw w)
        sh (/ vh h')]
    (println [sw sh] "h'" h')
    {:viewport [vx vy vw vh]
     :origin [x y]
     :scale [sw sh]}))

(defn camera-pan
  [cam [x y]]
  (update-in cam [:origin] (fn [[cx cy]] [(+ cx x) (+ cy y)])))

(defn game-init
  []
  {:camera (camera-setup [0.0 0.0 100.0 100.0] [0 0 (/ 1920 2) (/ 1080 2)])
   :thing {:p [25 25]}})

(defonce game-state (atom (game-init)))


(comment

 (swap! game-state update-in [:camera] camera-pan 10 10)
 )

(defn game-update
  [world input]
  (let [down (-> input :keys :down)
        mouse-down (-> input :mouse :down)]
    (cond-> world
      (down keys/a)
      (update-in [:thing :p 0] - 0.5)
      (down keys/d)
      (update-in [:thing :p 0] + 0.5)
      (down keys/s)
      (update-in [:thing :p 1] - 0.5)
      (down keys/w)
      (update-in [:thing :p 1] + 0.5)
      (mouse-down 0)
      (update-in [:camera] camera-pan (-> input :mouse :Δp))
      )

    ))

(defn setup-viewport
  [ctx {:keys [viewport origin scale] :as cam}]
  (let [[ox oy] origin
        [sx sy] scale]

  (.translate ctx ox oy)
  (.translate ctx 0 (/ 1080 2))
  (.scale ctx sx (- sy))))


(defn game-render
  [ctx world]
  (let [canvas (.-canvas ctx)
        width (.-width canvas)
        height (.-height canvas)
        [scale-x scale-y] (-> world :camera :scale)
        ]
    (.resetTransform ctx)

    (set! (.-fillStyle ctx) "#aaa")
    (.fillRect ctx 0 0 width height)

    (setup-viewport ctx (:camera world))

    (set! (.-strokeStyle ctx) "red")
    (set! (.-lineWidth ctx) (/ 1.0 scale-x))
    (.beginPath ctx)
    (.moveTo ctx 0 -1000)
    (.lineTo ctx 0 1000)
    (.stroke ctx)

    (set! (.-strokeStyle ctx) "blue")
    (.beginPath ctx)
    (.moveTo ctx -1000 0)
    (.lineTo ctx 1000 0)
    (.stroke ctx)


    (set! (.-fillStyle ctx) "#000")
    (let [[x y] (-> world :thing :p)]
      (.fillRect ctx x y 10 10))))

(def game-context (.getContext (.getElementById js/document "game") "2d"))

(defonce current-input (input/recording-input (browser/browser-input) game-state))

(defn game-step
  [ts]
  (let [input (input/next-input current-input ts)]
    (game-render game-context (swap! game-state game-update input))))

(defn startup-game-loop
  []
  (.requestAnimationFrame js/window
                          (fn tick [ts]
                            (game-step ts)
                            (.requestAnimationFrame js/window tick))))

(defonce startup (startup-game-loop))
