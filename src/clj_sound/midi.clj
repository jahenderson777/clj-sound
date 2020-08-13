(ns clj-sound.midi
  #_(:require [clj-sound.db :as db]
            ))

(def cc (atom {}))

(defn ^java.util.function.Function as-function [f]
  (reify java.util.function.Function
    (apply [this arg] (f arg))))

(defn midi-receive [ba]
  (let [[msg ch val] (seq ba)]
    (when (= msg -80)
      (swap! cc assoc ch val))))

(def midi-in (MidiHandler. "LPD8" (as-function midi-receive)))