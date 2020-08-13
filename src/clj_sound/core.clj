(ns clj-sound.core
  (:require [clj-sound.ui :as ui]
            [clj-sound.score :as score]
            [clj-sound.db :as db]
            [clj-sound.rate :refer [sample-rate]]
            [clj-sound.process-node :as process-node]
            [clj-sound.build-graph :as build-graph]
            [clj-sound.util :as util])
  (:import (javax.sound.sampled Mixer Mixer$Info AudioSystem DataLine$Info SourceDataLine AudioFormat AudioFormat$Encoding)
           SoundUtil)
  (:gen-class))

(set! *unchecked-math* true)

(def audio-format
  (AudioFormat. AudioFormat$Encoding/PCM_SIGNED
                sample-rate ; sample rate
                16    ; bits per sample
                2     ; channels
                4     ; frame size 2*16bits [bytes]
                48000 ; frame rate
                false ; little endian
                ))

(def buffer-size 800)
(def player (agent 0))

(def var-map (atom {}))

(def old-buf-calc-time (volatile! 0))

(defn build-buffer [sample-position]
  (let [now (System/nanoTime)
        diff (/ (- now @old-buf-calc-time) 1000000) ; in ms
        buf-len (/ buffer-size sample-rate 0.001)         ; in ms
        ]
    (when (< diff buf-len)
      (Thread/sleep (int (* 1.7 (- buf-len diff)))))
    (vreset! old-buf-calc-time now))
  (reset! process-node/buffers {})
  (let [{:keys [x graph]} @db/db
        new-graph (util/perf-watch :build-graph #(build-graph/build-graph buffer-size x x graph))
        fa (util/perf-watch :process-node #(process-node/process-node buffer-size x new-graph))]
    (swap! db/db assoc
           :master-buf fa
           :level (if fa
                    (/ (SoundUtil/maxFromBuf fa) 10)
                    0)
           :graph new-graph
           :x (+ x buffer-size))
    (when fa
      (->> (interleave fa fa)
           (map #(int (* 1000 %)))
           (mapcat (partial util/little-endian 2))
           byte-array))))

(defn play-loop [line buffer player is-playing]
  (when buffer
    (send player (fn [sample-position]
                   (.write ^SourceDataLine line buffer 0 (* 4 buffer-size))
                   (+ sample-position buffer-size))))
  (when is-playing
    (when-let [new-buffer (build-buffer @player)]
      (await player)
      (recur line new-buffer player (:playing @db/db)))))

(defn start-audio []
  (def thread (Thread. (fn []
                         (play-loop
                          (doto
                           ;(.getLine (SoundUtil/getLineByName "Soundflower (2ch)") (DataLine$Info. SourceDataLine audio-format))
                           (AudioSystem/getLine (DataLine$Info. SourceDataLine audio-format))
                            .open
                            .start)
                          nil player (:playing @db/db))
                         )))
  (.start thread))

(add-watch db/db :play-stop
           (fn [k db old-db new-db]
             (when (and (not (:playing old-db))
                      (:playing new-db))
               (start-audio))))



(comment
  (do (swap! db/db assoc :playing false)
      (Thread/sleep 1000)
      (swap! db/db assoc
             :playing true
             :x 0
             :graph [score/out]))
  
  


  (ui/launch db/db)


  
  )


(defn -main
  [& args]
  (start-audio))
