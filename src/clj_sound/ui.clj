(ns clj-sound.ui
  (:require [cljfx.api :as fx]
            [clj-sound.db :as db]
            [clojure.string :as str])
  (:import [javafx.scene.canvas Canvas]
           [javafx.scene.paint Color]
           [javafx.animation AnimationTimer]))

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

;(def timer (volatile! 0))
;(def scope-info (volatile! nil))
;(def animation-timer (volatile! nil))

#_(defn draw-buffer [canvas width height]
  (vswap! db/anim-count inc))

(defn canvas-scope [{:keys [x master-buf width height]}]
  {:fx/type :canvas
   :width width
   :height height
   :draw (fn [^Canvas canvas]
           (let [;old-time @timer
                                        ;new-time (vreset! timer (System/nanoTime))
                 ctx (doto (.getGraphicsContext2D canvas)
                       (.clearRect 0 0 width height)
                       (.setLineWidth 1.0)
                       (.strokeText (str x) 10 10)
                       ;(.strokeText (str random) 10 40)
                       ;(.strokeText (str (- new-time old-time)) 150 10)
                       (.beginPath)
                       (.moveTo 0 50)
                       (.setStroke Color/BLACK))]
             (when master-buf
               (loop [i 0]
                 (when (< i (/ (count master-buf) 8))
                   (.lineTo ctx (* i 8) (+ 50 (* 10 (aget master-buf (* i 8)))))
                   (recur (inc i)))))
                                        ;(.closePath ctx)
             (.stroke ctx))
           #_(when-not @animation-timer
             (println "starting animation timer for scope")
             (.start
              (vreset! animation-timer
                       (proxy [AnimationTimer] []
                         (handle [now]
                           (draw-buffer canvas width height)))))))})

(defn canvas-graph [{:keys [graph-str-lines width height]}]
  {:fx/type :canvas
   :width width
   :height height
   :draw (fn [^Canvas canvas]
                                        ;(println "a")
           (let [ctx (doto (.getGraphicsContext2D canvas)
                       (.clearRect 0 0 width height)
                                        ;(.setLineWidth 1.0)
                                        ;(.strokeText (str x) 10 10)
                                        ;(.beginPath)
                                        ;(.moveTo 0 50)
                       (.setStroke Color/BLACK))]
             (loop [i 0]
               (when (< i (count graph-str-lines))
                                        ;(println (nth lines i))
                 (.strokeText ctx (nth graph-str-lines i) 10 (+ 10 (* i 20)))
                 (recur (inc i))))))})


(defn map-event-handler [e]
  (case (:event/type e)
    ::stop (swap! db/db assoc :playing nil)
    ::play (swap! db/db assoc :playing true)
    ::pause (swap! db/db assoc :playing nil)
    ;::set-volume (swap! *state assoc :volume (:fx/event e))
    (prn e)))

(defn button [{:keys [text event-type]}]
  {:fx/type :button
   :text text
   :pref-width 100
   :on-action {:event/type event-type}})

(defn root-view [{db :db}]
  {:fx/type :stage
   :showing true
   :scene {:fx/type :scene
           :stylesheets #{"style.css"}
           :root {:fx/type :v-box
                  :padding 100
                  :spacing 50
                  :alignment :center
                  :children [#_{:fx/type canvas-graph
                              :width 800
                              :height 500
                                        ;:graph (:graph db)
                              :graph-str-lines (binding [*print-length* 10]
                                                 (take 10 (str/split-lines
                                                           (with-out-str
                                                             (clojure.pprint/pprint (:graph db))))))
                              }
                             {:fx/type canvas-scope
                              :width 800
                              :height 200
                              :x (:x db)
                              :master-buf (:master-buf db)}
                             {:fx/type canvas-progress-bar
                              :width 800
                              :height 20
                              :level (:level db)}
                             #_{:fx/type :slider
                                :pref-width 100
                                :min 0
                                :max 1
                                :value 0 ;(:master-vol db)
                                :on-value-changed #(swap! *progress %)}
                             {:fx/type :h-box
                              :padding 10
                              :spacing 10
                              :alignment :center
                              :children [{:fx/type button
                                          :text "Stop"
                                          :event-type ::stop}
                                         (if (:playing db)
                                           {:fx/type button
                                            :text "Pause"
                                            :event-type ::pause}
                                           {:fx/type button
                                            :text "Play"
                                            :event-type ::play})]}]}}})

(comment

  (renderer)

  )

(def renderer
  (fx/create-renderer
   :opts {:fx.opt/map-event-handler map-event-handler}
   :middleware
   (fx/wrap-map-desc
    (fn [db]
      {:fx/type root-view
       :db db}))))

(defn launch [*db]
  (fx/mount-renderer *db renderer))
