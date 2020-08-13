(ns clj-sound.rate)

(def sample-rate 48000)
(def bpm 140)
(def samples-per-tick (/ sample-rate (/ bpm 60) 256))