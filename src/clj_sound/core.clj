(ns clj-sound.core
  (:import (javax.sound.sampled AudioSystem DataLine$Info SourceDataLine
                                AudioFormat AudioFormat$Encoding))
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
    (send player (fn [sample-position]
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

(defn -main
  [& args]
  (start-audio))
