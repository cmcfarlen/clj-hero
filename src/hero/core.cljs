(ns hero.core
  (:require [hero.input.core :as input]
            [hero.input.browser :as browser]
            [hero.input.keys :as keys]
            [hero.math.vec2 :as vec2]
            [hero.math.vec3 :as vec3]
            [hero.math.core :as math])
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

(defn entity
  [id]
  {:entity/id id})

(defn world-create
  [[dim-x dim-y dim-z :as dim]]
  {:chunk-dim dim
   :chunks {}
   :entities {}})

(defn world-get-chunk
  [world [[chunk-x chunk-y chunk-z :as chunk-p] [rel-x rel-y rel-z] :as world-pos]]
  (get-in world [:chunks chunk-p]))

(defn world-chunk
  [[chunk-x chunk-y chunk-z :as loc]]
  )

(defn world-spawn-entity
  [world [[chunk-x chunk-y chunk-z :as chunk-p] [rel-x rel-y rel-z :as rel-p] :as world-pos] {:keys [entity/id] :as entity}]
  (-> world
      (update-in [:chunks chunk-p] (fn [chunk]
                                     (if chunk
                                       (update-in chunk [:entities] conj id)
                                       {:chunk-pos chunk-p
                                        :entities #{id}})))
      (assoc-in [:entities id] (assoc entity :world/position world-pos))))

(defn fixup-chunk-offset
  [rel dim]
  (math/floor (/ rel dim)))

(defn recanonicalize-position
  [[[chunk-x chunk-y chunk-z :as old-chunk] [rel-x rel-y rel-z :as old-rel] :as old-position] [dim-x dim-y dim-z :as dim]]
  (let [offsets [(fixup-chunk-offset rel-x dim-x) (fixup-chunk-offset rel-y dim-y) (fixup-chunk-offset rel-z dim-z)]]
    [(vec3/add old-chunk offsets) (vec3/subtract old-rel (vec3/multiply offsets dim))]))

(comment
 (recanonicalize-position [[5 5 5] [-0.245 0.456 20.432]] [10 10 10])
 (fixup-chunk-offset -0.245 10)
 )

(defn world-rechunk-entity
  [world entity-id]
  (let [old-position (-> world :entities (get entity-id) :world/position)
        new-position (recanonicalize-position old-position (:chunk-dim world))]
    (-> world
        (update-in [:chunks (first old-position) :entities] disj entity-id)
        (update-in [:chunks (first new-position)] (fn [chunk]
                                                    (if chunk
                                                      (update-in chunk [:entities] conj entity-id)
                                                      {:chunk-pos (first new-position)
                                                       :entities #{entity-id}})))
        (update-in [:entities entity-id] assoc :world/position new-position))))

(defn world-update-entity
  "Update an entity in the world.  f is a fn that takes the entity, the world and any arguments"
  [world entity-id f & args]
  (-> world
      (update-in [:entities entity-id] #(apply f % world args))
      (world-rechunk-entity entity-id)))

(defn world-entity
  [world entity-id]
  (get-in world [:entities entity-id]))

(defn world-move-entity-by
  "Move an entity in world position by the given vec3 delta."
  [entity world delta]
  (println entity)
  (update-in entity [:world/position 1] vec3/add delta))

(defn game-init
  []
  (-> (world-create [10 10 10])
      (world-spawn-entity [[0 0 0] [5 5 2]] (entity :player))
      (world-spawn-entity [[1 0 0] [2 2 2]] (entity 42))
      (world-spawn-entity [[0 0 0] [0 0 0]] (-> (entity :camera)
                                                (camera-setup
                                                 [0.0 0.0 50.0 50.0]
                                                 [0 0 (/ 1920 2) (/ 1080 2)]))))) 
(defonce game-state (atom (game-init)))

(defn game-update
  [world input]
  (let [down (-> input :keys :down)
        mouse-down (-> input :mouse :down)]
    (let [v (* (if (down keys/shift)
                 12.5171
                 1.400) (-> input :Δt))]
      (cond-> world
        (down keys/a)
        (world-update-entity :player world-move-entity-by [(- v) 0 0])
        (down keys/d)
        (world-update-entity :player world-move-entity-by [v 0 0])
        (down keys/s)
        (world-update-entity :player world-move-entity-by [0 (- v) 0])
        (down keys/w)
        (world-update-entity :player world-move-entity-by [0 v 0])
        (mouse-down 0)
        (world-update-entity :camera camera-pan (-> input :mouse :Δp))))))

(defn setup-viewport
  [ctx {:keys [camera/viewport camera/origin camera/scale] :as cam}]
  (let [[ox oy] origin
        [sx sy] scale]

  (.translate ctx ox oy)
  (.translate ctx 0 (/ 1080 2))
  (.scale ctx sx (- sy))))


(defn game-render
  [ctx world]
  (let [canvas (.-canvas ctx)
        camera (world-entity world :camera)
        width (.-width canvas)
        height (.-height canvas)
        [scale-x scale-y] (:camera/scale camera)]
    (.resetTransform ctx)

    (set! (.-fillStyle ctx) "#aaa")
    (.fillRect ctx 0 0 width height)

    (setup-viewport ctx camera)

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

    (let [[cw ch] (-> world :chunk-dim)]
      ;; draw chunks as rects
      (doseq [[cx cy cz] (-> world :chunks keys)]
        (set! (.-strokeStyle ctx) "lightgray")
        (.strokeRect ctx (* cw cx) (* ch cy) cw ch))
      ;; draw the entities as dots
      (doseq [e (-> world :entities vals)]
        (let [[[cx cy] [x y z]] (:world/position e)]
          (set! (.-fillStyle ctx) "#00b")
          (.beginPath ctx)
          (.arc ctx (+ (* cw cx) x) (+ (* ch cy) y) 0.5 0 (* 2 (.-PI js/Math)))
          (.fill ctx))))))

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
