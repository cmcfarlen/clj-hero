(ns hero.render.core
  (:require [hero.math.vec3 :as vec3]
            [hero.math.core :as math :refer [π τ]]
            [hero.world.core :as world]))

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


(defn set-options!
  [ctx opts]
  (when opts
    (doseq [[o v] opts]
      (println "opt " o v)
      (case o
        :stroke
        (stroke-style! ctx v)
        :fill
        (fill-style! ctx v)
        :color
        (do
         (fill-style! ctx v)
         (stroke-style! ctx v))
        :width
        (line-width! ctx v)))))

(defmulti draw-command! (fn [_ cmd & args] cmd))

(defmethod draw-command! :line
  [ctx _ [a b]]
  (println "draw line: " a b)
  (line! ctx a b))

(defn draw-scene-item!
  [ctx item]
  (.save ctx)
  (let [[command & [opts & params :as all-params]] item
        [opts params] (if (map? opts)
                        [opts params]
                        [{} all-params])]
    (println command opts params)
    (set-options! ctx opts)
    (draw-command! ctx command params))
  (.restore ctx))

(defn scene-render
  [ctx camera scene]
  (let [canvas (.-canvas ctx)
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

    (doseq [e scene]
      (draw-scene-item! ctx e))))

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
         (let [p (:world/position e)
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
                      (> 0 (vec3/dot v n)))
             (let [pp (-> world :entities :player :world/position)
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
         (let [[x y z] (:world/position e)
              v (:world/velocity e)]
          (fill-circle! ctx x y 0.5)
          (when (and v (not (vec3/zero? v)))
            (vector! ctx [x y 0] v))))))))
