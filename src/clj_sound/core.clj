(ns clj-sound.core
  (:require [fastmath.core :as m]
            [clj-sound.ui :as ui]
            [clj-sound.score :as score]
            [clojure.data.int-map :as i]
            [clojure.data.avl :as avl])
  (:import (javax.sound.sampled AudioSystem DataLine$Info SourceDataLine AudioFormat AudioFormat$Encoding)
           SimpleOsc
           CubicSplineFast
           WavFile)
  (:gen-class))

(set! *unchecked-math* true)

(def audio-format
  (AudioFormat. AudioFormat$Encoding/PCM_SIGNED
                48000 ; sample rate
                16    ; bits per sample
                2     ; channels
                4     ; frame size 2*16bits [bytes]
                48000 ; frame rate
                false ; little endian
                ))

(def buffer-size 1024)
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

(defn signal [x]
  (repeat 2 (+ (reduce + (map (partial a-synth x) (range 1 50 1)))
                                        ;(* 0.01231 (signal (- x 2)))
                                        ;(* 0.03231 (signal (- x 3)))
               )))

(def fa (into-array (repeat 2 (float-array 1024))))

(def o (SimpleOsc.))

(.init o 48000)

(def buf-size 1024)
(def x (atom 0))
(def graph (atom (score/out 0)))


(defn build-buffer [sample-position]
  (reset! score/buffers {})
  (swap! graph (partial score/build-graph buf-size @x @x))
  (let [fa (score/process-node buf-size @x @graph)]
    (swap! x (partial + buf-size))
    (->> (interleave fa fa)
         (map #(int (* 1000 %)))
         (mapcat (partial little-endian 2))
         byte-array)))

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

  (def w (WavFile/openWavFile (java.io.File. "resources/Electro-Tom.wav")))
  (def da (double-array (* (.getNumChannels w) (.getNumFrames w))))
  (.readFrames w da (.getNumFrames w))

  (do (swap! db assoc :playing false)
      (Thread/sleep 1000)
      (reset! score/buffers {})
      (reset! x 0)
      (reset! graph (score/out 0))
      (swap! db assoc :playing true))
  )


(defn -main
  [& args]
  (start-audio))
