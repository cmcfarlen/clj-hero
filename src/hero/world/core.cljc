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

(defn- position->chunk
  [pos dim]
  (mapv math/floor (vec3/divide pos dim)))

(defn spawn
  [world world-pos {:keys [entity/id] :as entity}]
  (let [chunk-p (position->chunk world-pos (:chunk-dim world))]
    (-> world
        (update-in [:chunks chunk-p] (fn [chunk]
                                       (if chunk
                                         (update-in chunk [:entities] conj id)
                                         {:chunk-pos chunk-p
                                          :entities #{id}})))
        (assoc-in [:entities id] (assoc entity :world/position world-pos :world/chunk chunk-p)))))

(defn- rechunk-entity
  [world entity-id]
  (let [e (-> world :entities (get entity-id))
        old-chunk (:world/chunk e)
        new-chunk (position->chunk (:world/position e) (:chunk-dim world))]
    (-> world
        (update-in [:chunks old-chunk :entities] disj entity-id)
        (update-in [:chunks new-chunk] (fn [chunk]
                                         (if chunk
                                           (update-in chunk [:entities] conj entity-id)
                                           {:chunk-pos new-chunk
                                            :entities #{entity-id}})))
        (update-in [:entities entity-id] assoc :world/chunk new-chunk))))

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
  (let [ep (:world/position e)
        wp (:world/position w)
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
                    #_(assoc-in [:world/position] (vec3/add c (vec3/scale (:wall/n w) 0.5)))
                    )
                e)) entity (filter :wall/n (-> world :entities vals)))))

(defn accellerate-entity
  "entity acceleration"
  [entity world Δt a]
  (let [v (entity :world/velocity [0 0 0])
        a (vec3/add a (vec3/scale v -8.0))]
    (-> entity
        (update-in [:world/position] vec3/add (vec3/add
                                                 (vec3/scale a (* 0.5 Δt Δt))
                                                 (vec3/scale v Δt)))
        (update-in [:world/velocity] (fnil vec3/add [0 0 0]) (vec3/scale a Δt))
        (collide-walls world))))

