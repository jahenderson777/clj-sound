(ns clj-sound.ui
  (:require [cljfx.api :as fx])
  (:import [javafx.scene.canvas Canvas]
           [javafx.scene.paint Color]))

(def *progress
  (atom 0.3))

(def *context
  (atom
   (fx/create-context {:x 100
                       :num-active-nodes 39})))

(defn canvas-progress-bar [{:keys [level width height]}]
  {:fx/type :canvas
   :width width
   :height height
   :draw (fn [^Canvas canvas]
           (doto (.getGraphicsContext2D canvas)
             (.clearRect 0 0 width height)
             (.setFill Color/LIGHTGREY)
             (.fillRoundRect 0 0 width height height height)
             (.setFill Color/GREEN)
             (.fillRoundRect 0 0 (* width level) height height height)))})

(def renderer
  (fx/create-renderer
    :middleware
    (fx/wrap-map-desc
      (fn [db]
        {:fx/type :stage
         :showing true
         :scene {:fx/type :scene
                 :root {:fx/type :v-box
                        :padding 100
                        :spacing 50
                        :children [{:fx/type canvas-progress-bar
                                    :width 100
                                    :height 10
                                    :level (:level db)}
                                   {:fx/type :slider
                                    :pref-width 100
                                    :min 0
                                    :max 1
                                    :value 0 ;(:master-vol db)
                                    :on-value-changed #(swap! *progress %)}]}}}))))

(defn launch [*db]
  (fx/mount-renderer *db renderer))
