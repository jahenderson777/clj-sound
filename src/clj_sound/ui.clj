(ns clj-sound.ui
  (:require [cljfx.api :as fx])
  (:import [javafx.scene.canvas Canvas]
           [javafx.scene.paint Color]))

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



(comment
  (defui MyCircle
    (render [this {:keys [radius]}]
            (ui/circle
             :center-x 20
             :center-y 20
             :radius (double radius))))

  (defui Stage
    (render [this db]
            (ui/stage
             :title "Hello World!"
             :shown true
             :min-width 300
             :min-height 300
             :scene (ui/scene
                     :root (ui/stack-pane
                            :children [(my-circle db)
                                       (ui/button
                                        :text (if (:playing db)
                                                "Stop"
                                                "Play")
                                        :on-action {:event :play-stop})])))))

  (defmulti handle-event (fn [db event] (:event event)))

  (defmethod handle-event :play-stop [db {:keys []}]
    (update db :playing not))

  (defmethod handle-event :change-color [db {:keys []}]
    (assoc db :radius (rand-int 200)))

  (defn ui [db]
    (let [ui-state (agent (dom/app (stage @db)
                                   #(try
                                      (swap! db handle-event %)
                                      (catch Throwable ex
                                        (println ex)))))]
      (add-watch db :ui (fn [_ _ _ _]
                          (send ui-state
                                #(try
                                   (dom/update-app % (stage @db))
                                   (catch Throwable ex
                                     (println ex))))))))
  )
