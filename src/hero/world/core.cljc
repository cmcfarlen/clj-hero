(ns hero.world.core
  (:require [hero.math.vec3 :as vec3]
            [hero.math.core :as math]))

(defn entity
  [id]
  {:entity/id id})

(defn create
  [[dim-x dim-y dim-z :as dim]]
  {:chunk-dim dim
   :chunks {}
   :entities {}})

(defn- fixup-chunk-offset
  [rel dim]
  (math/floor (/ rel dim)))

(defn- recanonicalize-position
  [[[chunk-x chunk-y chunk-z :as old-chunk] [rel-x rel-y rel-z :as old-rel] :as old-position] [dim-x dim-y dim-z :as dim]]
  (let [offsets [(fixup-chunk-offset rel-x dim-x) (fixup-chunk-offset rel-y dim-y) (fixup-chunk-offset rel-z dim-z)]]
    [(vec3/add old-chunk offsets) (vec3/subtract old-rel (vec3/multiply offsets dim))]))

(defn spawn
  [world [[chunk-x chunk-y chunk-z :as chunk-p] [rel-x rel-y rel-z :as rel-p] :as world-pos] {:keys [entity/id] :as entity}]
  (let [[chunk-p rel-p :as world-pos] (recanonicalize-position world-pos (:chunk-dim world))]
    (-> world
        (update-in [:chunks chunk-p] (fn [chunk]
                                       (if chunk
                                         (update-in chunk [:entities] conj id)
                                         {:chunk-pos chunk-p
                                          :entities #{id}})))
        (assoc-in [:entities id] (assoc entity :world/position world-pos)))))

(comment
 (recanonicalize-position [[5 5 5] [-0.245 0.456 20.432]] [10 10 10])
 (fixup-chunk-offset -0.245 10))

(defn- rechunk-entity
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

(defn update-entity
  "Update an entity in the world.  f is a fn that takes the entity, the world and any arguments"
  [world entity-id f & args]
  (-> world
      (update-in [:entities entity-id] #(apply f % world args))
      (rechunk-entity entity-id)))

(defn get-entity
  [world entity-id]
  (get-in world [:entities entity-id]))

;; TODO: entity update functions... perhaps move these

(defn move-entity-by
  "Move an entity in world position by the given vec3 delta."
  [entity world delta]
  (println entity)
  (update-in entity [:world/position 1] vec3/add delta))

(defn chunk->scene
  [[[cx cy cz] [x y z]] [sx sy sz]]
  ; TODO: vector math here
  [(+ x (* cx sx))
   (+ y (* cy sy))
   (+ z (* cz sz))])

(defn collides?
  [e w dim]
  (let [ep (chunk->scene (:world/position e) dim)
        wp (chunk->scene (:world/position w) dim)
        l (/ (:wall/length w) 2)
        n (:wall/n w)
        d (vec3/point-plane-distance wp n ep)
        c (vec3/intersect-line-plane wp n ep n)
        v (:world/velocity e)
        dw (vec3/distance c wp)]
    (if (and (> 0 (vec3/dot v n)) (< d 0.5) (< dw l))
      c)))

(defn collide-walls
  [entity world]
  (let [dim (:chunk-dim world)]
    (reduce (fn [e w]
              (if-let [c (collides? e w dim)]
                (-> e
                    (update-in [:world/velocity] vec3/reflect (:wall/n w))
                    #_(assoc-in [:world/position 1] (vec3/add c (vec3/scale (:wall/n w) 0.5)))
                    )
                e)) entity (filter :wall/n (-> world :entities vals)))))

(defn accellerate-entity
  "entity acceleration"
  [entity world Δt a]
  (let [v (entity :world/velocity [0 0 0])
        a (vec3/add a (vec3/scale v -8.0))]
    (-> entity
        (update-in [:world/position 1] vec3/add (vec3/add
                                                 (vec3/scale a (* 0.5 Δt Δt))
                                                 (vec3/scale v Δt)))
        (update-in [:world/velocity] (fnil vec3/add [0 0 0]) (vec3/scale a Δt))
        (collide-walls world))))

