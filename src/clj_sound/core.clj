(ns clj-sound.core
  (:require [fastmath.core :as m]
            [clj-sound.ui :as ui])
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

(def buffer-size 256)
(def player (agent 0))

(def db (atom {:playing false
               :radius 50}))

(defn unsigned-byte [x]
  (byte (if (> x 127) (- x 256) x)))

(defn little-endian [size x]
  (map #(-> (bit-shift-right x (* 8 %))
            (bit-and 255)
            unsigned-byte)
       (range size)))

(defn a-synth [x f]
  (int (* 302 (m/qsin (* x
                           (+ 0.01 (/ 0.2 f) (* 0.00003
                                              (m/qsin (* x 0.00016)))))))))


(def db {

         :tracks {1 {:instrument :sampler
                     :sample "drum.wav"
                     :volume-env-decay 1}}})


(defn resets [k track x]
  (when (= 0 (mod x 50))
    11))

(defn volume-env [track x delta]
  (if (nil? delta)
    [x (double (/ 1 (* x (get-in db [:tracks track :volume-env-decay]))))]
    (let [ret
          (if-let [reset-env-pos (resets :volume-env track (+ x delta))]
            (volume-env track reset-env-pos nil)
            (when-let [[env-pos y] (volume-env track x (dec delta))]
              (volume-env track (inc env-pos) nil)))]
      (if (zero? delta)
        (second ret)
        ret))))

(defn sampler [track x delta]
  (when (> delta -10000)
    (let [getv (fn [sub-track]
                 (or (resets track sub-track (+ x delta))
                     (when-let [y (sub-track track x (dec delta))]
                       (sub-track track (inc y)))))

          vol (volume-env track x delta)

          vol (or (vol-env-resets (+ x delta))
                  (when-let [y (vol-trk x (dec delta))]
                    (source-vol-env (inc y))))

          note (or (note-resets (+ x delta))
                   (note-trk x (dec delta)))

          pitch (or (pitch-resets (+ x delta))
                    (when-let [y (pitch-trk x (dec delta))]
                      (source-pitch-env (inc y))))

          sample-pos (or (sample-pos-resets (+ x delta))
                         (when-let [y (sample-pos-trk (dec delta))]
                           (source-sample (+ y (/ 1 pitch)))))])))

(defn signal [x]
  (repeat 2 (+ (reduce + (map (partial a-synth x) (range 1 50 1)))
               ;(* 0.01231 (signal (- x 2)))
               ;(* 0.03231 (signal (- x 3)))
               )))

(defn build-buffer [sample-position]
  (->> (range sample-position (+ sample-position buffer-size))
       (mapcat signal)
       (mapcat (partial little-endian 2))
       byte-array))

(defn play-loop [line buffer player is-playing]
  (when buffer
    (send player (fn [sample-position]
                   (.write line buffer 0 (* 4 buffer-size))
                   (+ sample-position buffer-size))))
  (when is-playing
    (let [new-buffer (build-buffer @player)]
      (await player)
      (recur line new-buffer player (:playing @db)))))

(defn start-audio []
  (def thread (Thread. #(play-loop
                         (doto (AudioSystem/getLine (DataLine$Info. SourceDataLine audio-format))
                           (.open audio-format)
                           (.start))
                         nil player (:playing @db))))
  (.start thread))

(add-watch db :play-stop
           (fn [k db old-db new-db]
             (when (and (not (:playing old-db))
                      (:playing new-db))
               (start-audio))))



(comment
  (doall (for [i (range 1000)] (do (Thread/sleep 5) (swap! data-state assoc :radius (rand-int 2000)))))
  (ui/ui db)

  )



(defn -main
  [& args]
  (start-audio))
