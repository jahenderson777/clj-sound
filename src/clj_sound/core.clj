(ns clj-sound.core
  (:require [fn-fx.fx-dom :as dom]
            [fn-fx.controls :as ui]
            [fn-fx.diff :refer [component defui render should-update?]])
  (:import (javax.sound.sampled AudioSystem DataLine$Info SourceDataLine AudioFormat AudioFormat$Encoding))
  (:gen-class))

(def audio-format
  (AudioFormat. AudioFormat$Encoding/PCM_SIGNED
                48000 ; sample rate
                16    ; bits per sample
                2     ; channels
                4     ; frame size 2*16bits [bytes]
                48000 ; frame rate
                false ; little endian
                ))

(def buffer-size 0)
(def player (agent 0))
(def playing (atom true))

(defn unsigned-byte [x]
  (byte (if (> x 127) (- x 256) x)))

(defn little-endian [size x]
  (map #(-> (bit-shift-right x (* 8 %))
            (bit-and 255)
            unsigned-byte)
       (range size)))

(defn a-synth [x f]
  (int (* 602 (Math/sin (* x
                           (+ 0.01 (/ 0.2 f) (* 0.0003
                                              (Math/sin (* x 0.0016)))))))))

(defn signal [x]
  (repeat 2 (+ (reduce + (map (partial a-synth x) (range 1 10 2)))
               (* 0.01231 (signal (- x 2)))
               (* 0.03231 (signal (- x 3))))))

(defn build-buffer [sample-position]
  (->> (range sample-position (+ sample-position buffer-size))
       (mapcat signal)
       (mapcat (partial little-endian 2))
       byte-array))

(defn play-loop [line buffer player is-playing]
  (when buffer
    (send-off player (fn [sample-position]
                       (.write line buffer 0 (* 4 buffer-size))
                       (+ sample-position buffer-size))))
  (let [new-buffer (if is-playing
                     (build-buffer @player)
                     (Thread/sleep 10))]
    (await player)
    (recur line new-buffer player @playing)))

(defn start-audio []
  (def thread (Thread. #(play-loop
                         (doto (AudioSystem/getLine (DataLine$Info. SourceDataLine audio-format))
                           (.open audio-format)
                           (.start))
                         nil player @playing)))
  (.start thread))

(defui MyCircle
  (render [this {:keys [radius]}]
          (ui/circle
           :center-x 20
           :center-y 20
           :radius (double radius))))

(defui Stage
  (render [this args]
          (ui/stage
           :title "Hello World!"
           :shown true
           :min-width 300
           :min-height 300
           :scene (ui/scene
                   :root (ui/stack-pane
                          :children [(my-circle args)
                                     (ui/button
                                      :text "Change radius"
                                      :on-action {:event :change-color}
                                      )
                                     ])))))


(def data-state (atom {:radius 50}))

(defmulti handle-event (fn [state event]
                         (:event event)))

(defmethod handle-event :change-color
  [state {:keys []}]
  (assoc state :radius (rand-int 200)))

#_(doall (for [i (range 1000)] (do (Thread/sleep 5) (swap! data-state assoc :radius (rand-int 2000)))))

(defn ui []
  (let [;; handler-fn handles events from the ui and updates the data state
        handler-fn (fn [event]
                     (try
                       (swap! data-state handle-event event)
                       (catch Throwable ex
                         (println ex))))

        ;; ui-state holds the most recent state of the ui
        ui-state   (agent (dom/app (stage @data-state) handler-fn))]

    ;; Every time the data-state changes, queue up an update of the UI
    (add-watch data-state :ui (fn [_ _ _ _]
                                (send ui-state
                                      (fn [old-ui]
                                        (try
                                          (dom/update-app old-ui (stage @data-state))
                                          (catch Throwable ex
                                            (println ex)))))))))

#_(defn ui []
  (let [u (ui/stage
           :title "Hello World!"
           :shown true
           :min-width 300
           :min-height 300
           :scene (ui/scene
                   :root (ui/stack-pane
                          :children [
                                     (ui/button
                                      :text "Say 'Hello World'"
                                      :on-action {:say "Hello World!"}
                                      )])))
        handler-fn (fn [evt]
                     (println "Received Event: " evt))]
    (dom/app u handler-fn)))

(defn -main
  [& args]
  (start-audio))
