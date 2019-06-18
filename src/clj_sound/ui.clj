(ns clj-sound.ui
  (:require [fn-fx.fx-dom :as dom]
            [fn-fx.controls :as ui]
            [fn-fx.diff :refer [component defui render should-update?]]))

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
