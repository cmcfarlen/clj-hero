(ns hero.input.browser
  (:require [goog.events :as events]
            [hero.input.core :as input]))

(def ignore-keys #{91 9})

(def key-state (atom {:down #{}}))

(defn key-down-handler
  [e]
  (let [key-code (.-keyCode e)
        {:keys [down modifiers]} @key-state]
    (when-not (or (ignore-keys key-code) (down key-code))
      #_(println "key-down" e (.-keyCode e) (char key-code))
      (swap! key-state update-in [:down] conj key-code)))
  true)

(defn key-up-handler
  [e]
  (let [key-code (.-keyCode e)]
      #_(println "key-up" e (.-keyCode e))
    (swap! key-state update-in [:down] disj key-code))
  true)

(def mouse-state (atom {:p [0 0]
                        :w [0 0 0]
                        :buttons #{}}))

(defn mouse-move-handler
  [e]
  (let [client-pt [(.-clientX e) (.-clientY e)]]
    (swap! mouse-state assoc-in [:p] client-pt)))

;; todo - fix wheel
(defn mouse-wheel-handler
  [e]
  (let [be (.getBrowserEvent e)
        Δx (.-deltaX be)
        Δy (.-deltaY be)
        Δz (.-deltaZ be)]
    (swap! mouse-state update-in [:w] (fn [[x y z]]
                                        [(+ Δx x) (+ Δy y) (+ Δz z)]))))

(defn mouse-down-handler
  [e]
  (let [button (.-button e)]
    (swap! mouse-state update-in [:buttons] conj button)))

(defn mouse-up-handler
  [e]
  (let [button (.-button e)]
    (swap! mouse-state update-in [:buttons] disj button)))

(events/removeAll js/window)

(events/listen js/window "keydown" key-down-handler)
(events/listen js/window "keyup" key-up-handler)
(events/listen js/window "mousemove" mouse-move-handler)
(events/listen js/window "wheel" mouse-wheel-handler)
(events/listen js/window "mousedown" mouse-down-handler)
(events/listen js/window "mouseup" mouse-up-handler)

(defrecord BrowserInput [platform-state]
  input/Input
  (next-input [this ts]
    (let [{:keys [last-keystate last-mouse-state last-ts]} @platform-state
          next-keystate @key-state
          is-down (:down next-keystate)
          was-down (:down last-keystate)
          next-mouse-state @mouse-state
          [omx omy] (:p last-mouse-state)
          [nmx nmy] (:p next-mouse-state)
          [owx owy owz] (:w last-mouse-state)
          [nwx nwy nwz] (:w next-mouse-state)
          mouse-delta [(- nmx omx) (- nmy omy)]
          wheel-delta [(- nwx owx) (- nwy owy) (- nwz owz)]
          mouse-down (:buttons next-mouse-state)
          mouse-was-down (:buttons last-mouse-state)
          Δt (- ts last-ts)]
      (swap! platform-state assoc :last-keystate next-keystate :last-mouse-state next-mouse-state :last-ts ts)
      {:keys {:down is-down
              :pressed (clojure.set/difference is-down was-down)
              :released (clojure.set/difference was-down is-down)}
       :mouse {:p (:p next-mouse-state)
               :Δp mouse-delta
               :Δw wheel-delta
               :down mouse-down
               :pressed (clojure.set/difference mouse-down mouse-was-down)
               :released (clojure.set/difference mouse-was-down mouse-down)}
       :t ts
       :Δt Δt})))


(defn browser-input
  []
  (BrowserInput. (atom {:last-keystate @key-state
                        :last-mouse-state @mouse-state
                        :last-ts 0})))

