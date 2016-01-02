(ns hero.input.core
  (:require [hero.input.keys :as keys])
  )

(defprotocol Input
  (next-input [this timestamp]))

(defrecord RecordingInput [other-input world-atom history]
  Input
  (next-input [this ts]
    (let [oin (next-input other-input ts)
          {:keys [down pressed]} (:keys oin)]
      (if-let [hist @history] ;; recording or playback
        (let [{:keys [idx inputs state]} hist]
          (if idx ;; playback
            (if (and (pressed keys/r)
                     (down keys/ctrl))
              (do
               (println "quit recoring")
               (reset! history nil)
               oin)
              (if-let [i (nth inputs idx nil)]
                (do
                 (swap! history update-in [:idx] inc)
                 i)
                (do
                 (println "reset loop")
                 (reset! world-atom state)
                 (swap! history assoc :idx 1)
                 (first inputs))))
            (if (and (pressed keys/r)
                     (down keys/ctrl))
              (do
               (reset! world-atom state)
               (swap! history assoc :idx 1)
               (first inputs))
              (do
               (swap! history update-in [:inputs] conj oin)
               oin))))
        (do
         (when (and (pressed keys/r)
                    (down keys/ctrl))
           (println "Start recording")
           (reset! history {:state @world-atom :idx nil :inputs [oin]}))
         oin)))))

(defn recording-input
  [input game-atom]
  (RecordingInput. input game-atom (atom nil)))
