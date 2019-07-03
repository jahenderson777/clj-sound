(ns clj-sound.score
  (:import SawTooth SineOsc)
  (:require [clojure.string :as str]))


(defn a-synth [x freq]
  [*
   {0 1 50 0}
   [SawTooth freq]
    ;30000 [SawTooth freq]
    ;60000 [SawTooth freq]
    ])



(defn melody [x freq]
  [0 ['a-synth freq]
   40000 ['a-synth 200]])

(defn noise [x freq]
  [* {0 (rand-int 4)
      (+ 500 (* 1000 (rand-int 100))) 1
      100010 0}
   [SineOsc freq]])

(defn lazy-melody [x n]
  (lazy-seq (cons (+ (rand-int 1000) (* 10000 n)) (cons ['noise (* 100 (inc (rand-int 13)))] (lazy-melody x (inc n))))))

(defn drum [x]
  [SineOsc {0 700
            1000 170
            10000 20}])

(defn out [x]
  #_[0 [SawTooth 2000]
   2 [SawTooth 2000]]
  {:b1 [0 ['lazy-melody 0]
        100000 ['drum]
        120000 ['drum]
        160000 ['drum]
        200000 ['noise 400]
        300000 ['noise 350]
        400000 ['noise 100]
        ]
                                        ;['lazy-melody 0]
                                        ;100000 ['a-synth 200]
                                        ;200000 ['a-synth 301]
                                        ;100000 ['a-synth 553]
                                        ;400000 ['a-synth 551]
   
   ;:b0 ['a-synth 10]
   :<- :b1})






;; so process has to handle:
;; maps, process buffers in order
;; recur process on buffer values
;; if a seq? and int? first key, process all the events until we get a t>x, mix all the results, somehow handle ended ugens
;; if a seq? and UGen object or built-in fn first key, process all the args then (.process obj n input-buffers output-buffer)
;; returns a buffer
;; should we allow static float inputs, or should we always create buffers full of floats?






;; somehow watch for re-compiles of node-fn's? and only re-execute if re-compiled?;; keep a registry of node-fn's mapping symbol to function, check this often, and if different, re-execute node-fn in running db

;; if a sequenced event is in the past in the new one, but doesn't exist in the old one (matching by time & Class), remove it
;; any sequenced events that exist in the old one but not in the new one (matching by time=time & class=object), let them carry on,
;; if a sequenced event is in the near future in the new, but not in the old, mix it in 
;; if exists in both then change any parameters (unless they are nodes, in which case recur)
;; any un-sequenced UGens/Events/Nodes that exist in the old but not the new, fade them out
;; any un-sequenced UGens/Events/Nodes that exist in the new but not the old, fade them in



;; the nice thing about this was is that it is an easy and natural way to express the song
;; the problem is that recompiling top level defn's that don't get run very won't change what's running
;; like if we change the parameters to the reverb above, it will have no effect because 'out' only gets
;; executed once at the beginning, or if we change the melody sequence this will have no effect
;; we kind of need to re-evaluate the whole graph whenever we make a change
;; and somehow match up what is playing with the result of the new evaluation
;; if we swap the reverb for a compressor for example if would be nice if the playing melody sequence
;; was just immediately re-routed through the compressor.

;; the problem is we need to create stateful instances of ugens that change over time, but we want to describe everything in a pure way
;; maybe we need some concept of static effect
;; or maybe if we described all in data instead of functions, in one big data-structure, and we only had one datastructure,
;; that we supplemented with the running state information, then we could swap a reverb for a compressor whilst it is running
;; or what if

(comment
  {:fn ['out]
   :x 1000
   :b1 [0 {:fn ['melody 400]
           :x 1000
           :<- [0 {:fn ['a-synth freq]
                   :x 1000
                   :<- [mul
                        {0 1 50 0}
                        [SawTooth freq]]}
                4 ['a-synth 200]]}
        10 ['melody 300]]
   :b2 [5 ['melody 200]
        15 ['melody 500]]
   :<- [Compressor [Reverb :b1]]}

  {:fn ['out]
   :b1 [0 ['melody 400]
        10 ['melody 300]]
   :b2 [5 ['melody 200]
        15 ['melody 500]]
   :<- [Compressor [Phaser :b1]]})

