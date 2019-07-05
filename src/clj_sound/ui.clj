(ns clj-sound.ui
  (:require [cljfx.api :as fx]
            [clj-sound.db :as db])
  (:import [javafx.scene.canvas Canvas]
           [javafx.scene.paint Color]
           [javafx.animation AnimationTimer]))

(def *progress
  (atom 0.3))

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

(def timer (volatile! 0))
(def scope-info (volatile! nil))
(def animation-timer (volatile! nil))

(defn draw-buffer [canvas width height]
  (when-let [info @scope-info]
    (let [db @db/db
          x (:x db)
          random (:random db)
          buffer (:master-buf db)]
      (let [
            old-time @timer
            new-time (vreset! timer (System/nanoTime))
            ctx (doto (.getGraphicsContext2D canvas)
                  (.clearRect 0 0 width height)
                  (.setLineWidth 1.0)
                  (.strokeText (str x) 10 10)
                  (.strokeText (str random) 10 40)
                  (.strokeText (str (- new-time old-time)) 150 10)
                  (.beginPath)
                  (.moveTo 0 50)
                  (.setStroke Color/BLACK))]
        (when buffer
          (loop [i 0]
            (when (< i (/ (count buffer) 8))
              (.lineTo ctx (* i 3) (+ 50 (* 10 (aget buffer (* i 8)))))
              (recur (inc i)))))
                                        ;(.closePath ctx)
        (.stroke ctx)))))

(defn canvas-scope [{:keys [x master-buf width height]}]
  {:fx/type :canvas
   :width width
   :height height
   :draw (fn [^Canvas canvas]
           (when-not @animation-timer
             (println "starting animation timer for scope")
             (.start
              (vreset! animation-timer
                       (proxy [AnimationTimer] []
                         (handle [now]
                           (draw-buffer canvas width height))))))
           (vreset! scope-info {:buffer master-buf :x x}))})

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
                        :children [{:fx/type canvas-scope
                                    :width 500
                                    :height 100
                                    :x (:x db)
                                    :master-buf (:master-buf db)
                                    }
                                   {:fx/type canvas-progress-bar
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
