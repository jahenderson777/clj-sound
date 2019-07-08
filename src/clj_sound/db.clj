(ns clj-sound.db)

(def db (atom {:x 0
               :graph nil
               :playing false
               :tempo 130
               :level 0.0
               :master-vol 1.0}))

(def master-buf (volatile! nil))

;(def anim-count (volatile! 0))
