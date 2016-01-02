(ns hero.core
  (:require [hero.input.core :as input]
            [hero.input.browser :as browser]
            [hero.input.keys :as keys])
  (:import [goog.events KeyCodes]))

(enable-console-print!)

(defn game-init
  []
  {:thing [100 100 10 10]})

(defonce game-state (atom (game-init)))

(defn game-update
  [world input]
  (let [down (-> input :keys :down)]
    (cond-> world
      (down keys/a)
      (update-in [:thing 0] dec)
      (down keys/d)
      (update-in [:thing 0] inc)
      (down keys/s)
      (update-in [:thing 1] inc)
      (down keys/w)
      (update-in [:thing 1] dec))))

(defn game-render
  [ctx world]
  (let [canvas (.-canvas ctx)
        width (.-width canvas)
        height (.-height canvas)]
    (set! (.-fillStyle ctx) "#aaa")
    (.fillRect ctx 0 0 width height)
    (set! (.-fillStyle ctx) "#000")
    (let [[x y w h] (:thing world)]
      (.fillRect ctx x y w h))))

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
