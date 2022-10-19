(ns clj-sound.core
  (:require [clj-sound.engine :as engine]
            [clj-sound.score :as score])
  (:gen-class))

(defn -main
  [& args]
  (engine/start-audio)
  )

(comment 
  (engine/start-audio)

  (reset! engine/graph [score/out])

  )