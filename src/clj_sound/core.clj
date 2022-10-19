(ns clj-sound.core
  (:require [clj-sound.engine :as engine]
            [clj-sound.score :as score])
  (:gen-class))

(defn -main
  [& args]
  (engine/start-audio)
  )

(comment 
  
  (reset! engine/graph [score/out])
  (engine/start-audio) 

  )