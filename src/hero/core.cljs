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

(defn entity-ref
  [entity]
  (:entity/id entity))

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
  [world [[chunk-x chunk-y chunk-z :as chunk-p] [rel-x rel-y rel-z :as rel-p] :as world-pos] entity]
  (-> world
      (update-in [:chunks chunk-p] (fn [chunk]
                                     (if chunk
                                       (update-in chunk [:entities] conj (entity-ref entity))
                                       {:chunk-pos chunk-p
                                        :entities #{(entity-ref entity)}})))
      (assoc-in [:entities (entity-ref entity)] (assoc entity :world/position world-pos))))

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
  [world entity-id f & args]
  (-> world
      (update-in [:entities entity-id] #(apply f % args))
      (world-rechunk-entity entity-id)))

(defn game-init
  []
  {:camera (camera-setup [0.0 0.0 50.0 50.0] [0 0 (/ 1920 2) (/ 1080 2)])
   :world (-> (world-create [10 10 10])
              (world-spawn-entity [[2 3 0] [5 5 2]] (entity 41))
              (world-spawn-entity [[1 3 0] [2 2 2]] (entity 42)))
   :thing {:p [25 25]}})

(defonce game-state (atom (game-init)))

(comment
 (-> (world-create [10 10 10])
     (world-spawn-entity [[0 0 0] [2 2 2]] (entity 42))
     )

 (-> (deref game-state) :world :entities (get 42) :world/position (get 1))

 (swap! game-state update-in [:world] world-update-entity 42 update-in [:world/position 1] vec3/add [10 22 30])

 (world-update-entity (-> @game-state :world) 42 update-in [:world/position 1] vec3/add [10 22 30])

 )


(defn game-update
  [world input]
  (let [down (-> input :keys :down)
        mouse-down (-> input :mouse :down)]
    (let [v (* (if (down keys/shift)
                 12.5171
                 1.400) (-> input :Δt))]
      (cond-> world
        (down keys/a)
        (update-in [:thing :p] vec2/add [(- v) 0])
        (down keys/d)
        (update-in [:thing :p] vec2/add [v 0])
        (down keys/s)
        (update-in [:thing :p] vec2/add [0 (- v)])
        (down keys/w)
        (update-in [:thing :p] vec2/add [0 v])
        (mouse-down 0)
        (update-in [:camera] camera-pan (-> input :mouse :Δp))
        ))

    ))

(comment
 (vec2/add [25 25] [0 0.5])

 (-> 102.423
     (/ 10.0)
     #_((.-floor js/Math))
     )

 )

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

    (let [[cw ch] (-> world :world :chunk-dim)]
      ;; draw chunks as rects
      (doseq [[cx cy cz] (-> world :world :chunks keys)]
        (set! (.-strokeStyle ctx) "lightgray")
        (.strokeRect ctx (* cw cx) (* ch cy) cw ch))
      ;; draw the entities as dots
      (doseq [e (-> world :world :entities vals)]
        (let [[[cx cy] [x y z]] (:world/position e)]
          (set! (.-fillStyle ctx) "#00b")
          (.beginPath ctx)
          (.arc ctx (+ (* cw cx) x) (+ (* ch cy) y) 0.5 0 (* 2 (.-PI js/Math)))
          (.fill ctx))))

    (set! (.-fillStyle ctx) "#000")
    (let [[x y] (-> world :thing :p)]
      (.fillRect ctx x y 0.4 1.4))))

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
