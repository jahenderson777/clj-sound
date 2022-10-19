(ns clj-sound.engine
  (:require [clj-sound.rate :refer [sample-rate]]
            [clj-sound.process-node :refer [process-node clear-buffers]]
            [clj-sound.build-graph :refer [build-graph]]
            [clj-sound.util :as util])
  (:import (javax.sound.sampled AudioSystem DataLine$Info SourceDataLine AudioFormat AudioFormat$Encoding)
           SoundUtil))

(set! *unchecked-math* true)

(def audio-format (AudioFormat. AudioFormat$Encoding/PCM_SIGNED
                                sample-rate ; sample rate
                                16    ; bits per sample
                                2     ; channels
                                4     ; frame size 2*16bits [bytes]
                                48000 ; frame rate
                                false ; little endian
                                ))

(def buffer-size 800)
(def paused (atom false))
(def graph (atom nil))

(defn floats->bytes [fa]
  (->> (interleave fa fa)
       (map #(int (* 1000 %)))
       (mapcat (partial util/little-endian 2))
       byte-array))

(defonce line
  (doto (->> (DataLine$Info. SourceDataLine audio-format)
             ;(.getLine (SoundUtil/getLineByName "Soundflower (2ch)"))
             AudioSystem/getLine)
    .open .start))

(defn write-to-line! [floats]
  (.write ^SourceDataLine line (floats->bytes floats) 0 (* 4 buffer-size)))

(defn start-audio []
  (future (loop [sample-pos 0]
            (clear-buffers)
            (if @paused (Thread/sleep 50)
                (->> (swap! graph #(build-graph {:n buffer-size :x-buf sample-pos :x sample-pos :node %}))
                     (process-node buffer-size sample-pos)
                     write-to-line!))
            (recur (+ sample-pos buffer-size)))))
