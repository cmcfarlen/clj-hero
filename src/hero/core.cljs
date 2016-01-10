(ns hero.core
  (:require [hero.input.core :as input]
            [hero.input.browser :as browser]
            [hero.input.keys :as keys]
            [hero.math.vec2 :as vec2]
            [hero.math.vec3 :as vec3]
            [hero.math.core :as math :refer [π τ]]
            [hero.world.core :as world])
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
      (world/spawn [[0 0 0] [5 5 0]] (world/entity :player))
      (world/spawn [[1 0 0] [2 2 0]] (world/entity 42))
      (world/spawn [[0 0 0] [0 0 0]] (-> (world/entity :camera)
                                         (camera-setup
                                          [0.0 0.0 50.0 50.0]
                                          [0 0 (/ 1920 2) (/ 1080 2)])))
      (world/spawn [[0 0 0] [3 3 0]] (wall :w1 [3 3 3] [0 1 0] 10))
      (world/spawn [[0 0 0] [7 7 0]] (wall :w2 [7 7 7] [-1 1 0] 10))
      )) 
(defonce game-state (atom (game-init)))

(comment
 (vec3/magnitude [1 0 0] 50)

 (world/update-entity @game-state :player world/accellerate-entity 0.016 [0 0 0])
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

(defn setup-viewport!
  [ctx {:keys [camera/viewport camera/origin camera/scale] :as cam}]
  (let [[ox oy] origin
        [sx sy] scale]

  (.translate ctx ox oy)
  (.translate ctx 0 (/ 1080 2))
  (.scale ctx sx (- sy))))


(defn line-width!
  [ctx w]
  (set! (.-lineWidth ctx) w)
  ctx)

(defn stroke-color!
  [ctx color]
  (set! (.-strokeStyle ctx) color)
  ctx)

(defn stroke-style!
  [ctx style]
  (set! (.-strokeStyle ctx) style)
  ctx)

(defn stroke-rect!
  [ctx x y w h]
  (.strokeRect ctx x y w h)
  ctx)

(defn line!
  [ctx [x0 y0] [x1 y1]]
  (.beginPath ctx)
  (.moveTo ctx x0 y0)
  (.lineTo ctx x1 y1)
  (.stroke ctx)
  ctx)

(defn axis!
  [ctx]
  (-> ctx
      (stroke-color! "red")
      (line! [0 -1000] [0 1000])
      (stroke-color! "blue")
      (line! [-1000 0] [1000 0])))

(defn fill-style!
  [ctx style]
  (set! (.-fillStyle ctx) style)
  ctx)

(defn fill-rect!
  [ctx x y w h]
  (.fillRect ctx x y w h)
  ctx)

(defn fill-circle!
  [ctx cx cy r]
  (.beginPath ctx)
  (.arc ctx cx cy r 0 τ)
  (.fill ctx)
  ctx)

(defn vector!
  [ctx p v]
  ; TODO: project onto XY plane
  (let [r (math/atan2 (v 1) (v 0))
        m (vec3/magnitude v)]
    (.save ctx)
    (.translate ctx (p 0) (p 1))
    (.rotate ctx r)
    (.beginPath ctx)
    (.moveTo ctx 0 0)
    (.lineTo ctx m 0)
    (.translate ctx m 0)
    (.moveTo ctx -0.25 -0.25)
    (.lineTo ctx 0 0)
    (.lineTo ctx -0.25 0.25)
    (.stroke ctx)
    (.restore ctx))
  ctx)

(defn chunk->scene
  [[[cx cy cz] [x y z]] [sx sy sz]]
  ; TODO: vector math here
  [(+ x (* cx sx))
   (+ y (* cy sy))
   (+ z (* cz sz))])

(defn game-render
  [ctx world]
  (let [canvas (.-canvas ctx)
        camera (world/get-entity world :camera)
        width (.-width canvas)
        height (.-height canvas)
        [scale-x scale-y] (:camera/scale camera)]
    (.resetTransform ctx)

    (-> ctx
        (fill-style! "#aaa")
        (fill-rect! 0 0 width height))

    (setup-viewport! ctx camera)

    (-> ctx
        (line-width! (/ 1.0 scale-x))
        (axis!))

    (comment (let [vs [[1 0 0]
              [1 1 0]
              [0 1 0]
              [-1 1 0]
              [-1 0 0]
              [-1 -1 0]
              [0 -1 0]
              [1 -1 0]]]
      (stroke-style! ctx "blue")
      (doseq [v (map #(vec3/magnitude % 5) vs)]
        (vector! ctx [8 5 0] v)
        )
      )

    (stroke-style! ctx "blue")
    (vector! ctx [2 3 0] [1 0 0])
    (stroke-style! ctx "red")
    (vector! ctx [2 3 0] [0 1 0])
    (stroke-style! ctx "green")
    (vector! ctx [2 3 0] [-1 0 0])
    (stroke-style! ctx "yellow")
    (vector! ctx [2 3 0] [0 -1 0]))

    (let [[cw ch cz :as cs] (-> world :chunk-dim)]
      ;; draw chunks as rects
      (doseq [[cx cy cz] (-> world :chunks keys)]
        (-> ctx
            (stroke-style! "lightgray")
            (stroke-rect! (* cw cx) (* ch cy) cw ch)))

      ;; draw walls as lines and normals


      ;; draw the entities as dots
      (fill-style! ctx "#00b")
      (doseq [e (-> world :entities vals)]
        (cond
         (:wall/n e)
         (let [p (chunk->scene (:world/position e) cs)
               n (:wall/n e)
               v (-> world :entities :player :world/velocity)
               l (/ (:wall/length e) 2)
               w (vec3/magnitude (vec3/cross n [0 0 1]) l)]
           (-> ctx
             (stroke-style! "white")
             (vector! p n)
             (stroke-style! "black")
             (line! (vec3/subtract p w) (vec3/add p w)))
           (when (and v
                      (not (vec3/zero? v))
                      (> 0 (vec3/dot v n))
                      )
             (let [pp (chunk->scene (-> world :entities :player :world/position) cs)
                   c (vec3/intersect-line-plane p n pp v)
                   d (vec3/distance c p)]
               (when (< d l)
                 (if (< (vec3/distance c pp) 0.5)
                   (do
                    (vector! ctx pp (vec3/reflect v n))
                    (fill-style! ctx "red"))
                   (fill-style! ctx "green"))
                 (fill-circle! ctx (c 0) (c 1) 0.25)))))

         :else
         (let [[[cx cy] [x y z]] (:world/position e)
              v (:world/velocity e)]
          (fill-circle! ctx (+ (* cw cx) x) (+ (* ch cy) y) 0.5)
          (when (and v (not (vec3/zero? v)))
            (vector! ctx [(+ (* cw cx) x) (+ (* ch cy) y) 0] v))
          ))))))

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
