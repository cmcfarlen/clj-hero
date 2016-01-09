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

(defn spawn
  [world [[chunk-x chunk-y chunk-z :as chunk-p] [rel-x rel-y rel-z :as rel-p] :as world-pos] {:keys [entity/id] :as entity}]
  (-> world
      (update-in [:chunks chunk-p] (fn [chunk]
                                     (if chunk
                                       (update-in chunk [:entities] conj id)
                                       {:chunk-pos chunk-p
                                        :entities #{id}})))
      (assoc-in [:entities id] (assoc entity :world/position world-pos))))

(defn- fixup-chunk-offset
  [rel dim]
  (math/floor (/ rel dim)))

(defn- recanonicalize-position
  [[[chunk-x chunk-y chunk-z :as old-chunk] [rel-x rel-y rel-z :as old-rel] :as old-position] [dim-x dim-y dim-z :as dim]]
  (let [offsets [(fixup-chunk-offset rel-x dim-x) (fixup-chunk-offset rel-y dim-y) (fixup-chunk-offset rel-z dim-z)]]
    [(vec3/add old-chunk offsets) (vec3/subtract old-rel (vec3/multiply offsets dim))]))

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

(defn accellerate-entity
  "entity acceleration"
  [entity world Δt a]
  (let [v (entity :world/velocity [0 0 0])
        a (vec3/add a (vec3/scale v -8.0))]
    (-> entity
        (update-in [:world/position 1] vec3/add (vec3/add
                                                 (vec3/scale a (* 0.5 Δt Δt))
                                                 (vec3/scale v Δt)))
        (update-in [:world/velocity] (fnil vec3/add [0 0 0]) (vec3/scale a Δt)))))

