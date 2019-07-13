(ns clj-sound.score
  (:import Saw Sine MoogLP MoogHP MoogBP WavFile UGen Dist)
  (:require [clj-sound.util :refer [defn*]]
            [clj-sound.db :as db]
            [clojure.string :as str]))


(defn make-sampler [filename]
  (let [w (WavFile/openWavFile (java.io.File. filename))
        n (.getNumFrames w)
        ya (double-array (* (.getNumChannels w) n))
        xa (double-array n)
        _ (.readFrames w ya n)
        w (.close w)
        _ (doseq [i (range n)]
            (aset-double xa i i))]
    (CubicSplineFast. xa ya)))

(def bd (make-sampler "resources/909/tape1/bd01.wav"))
(def ch (make-sampler "resources/909/tape1/cr01.wav"))
(def oh (make-sampler "resources/909/tape1/oh01.wav"))
(def cp (make-sampler "resources/909/tape1/cp01.wav"))
(def lt (make-sampler "resources/909/tape1/lt01.wav"))
(def vc (make-sampler "resources/the-year-is-1977.wav"))

(defn a-synth [x freq]
  [* {0 1 50 0}
   [Saw freq]])

(defn melody [x freq]
  [0 ['a-synth freq]
   40000 ['a-synth 200]])

(defn drum [x]
  [Sine {0 700
         1000 170
         10000 20}])

(defn* fm-synth [x freq]
  [0 [* {0 1 0x180 0.4 0x200 0}
      [Sine [+ :b01 (/ freq 1.5)]]]
   0x100 [Sine [+ freq
                   [* {0 99
                       0x100 (/ freq 2)
                       0x200 0}
                    [Sine (* freq (+ 2 (rand-int 4)))]]]]
   0x300 [* {0 1 0x80 0.4 0x100 0}
          [Sine (* freq 0.75)]]])

(defn* noise [x freq]
  [* {0 (rand-int 4)
      (+ 0x20 (* 0x10 (rand-int 10))) 1
      0x300 0.8
      0x400 0}
   [+ [fm-synth freq]
    [fm-synth (* 0.5 freq)]
    ;['drum]
    ]])

(defn lazy-melody [x n]
  (lazy-seq
   (concat [(* 30000 n) ['noise (* 100 (inc (rand-int 9)))]]
           (when (= 0 (mod n 4))
             [(+ 14000 (* 30000 n)) ['noise (* 200 (inc (rand-int 7)))]])
           (lazy-melody x (inc n)))))

(defn out [x]
  {:b01 [* 4 [Sine 12.1]]
   :b1 ['lazy-melody 0]
   :<- :b1})

(defn* moog-synth [x freq]
  [* 2 [MoogLP
        [*  0.8
         {0x00 0.8
          0x40 0.8
          0x65 0}
         [+ [Saw freq]
          [Saw (* 2.05 freq)]]]
        [*
         (+ 1.5 (/ (rand-int 3) 7))
                                        ;:cutoff
         {0x00 200
                                        ;1000 6111
          0x10 910
          0x80 122}]
        0.46]])

(defn* foo-melody [x n]
  (lazy-seq
   (let [x1 (* n 40000)]
     (concat [x1 ['moog-synth 10]
              (+ x1 11000) ['moog-synth 12]
              (+ x1 20000) ['moog-synth 9]
              (+ x1 31000) ['moog-synth 18]]
             (foo-melody* x (inc n))))))

(defn* out [x]
  {:cutoff [+ 11 [* 10 [Sine 0.02]]]
   :<- [+ ['techno-loop 0]
          ['foo-melody 0]]})

(defn* techno-loop [x n]
  (lazy-seq
   (let [x1 (* n 0x100)]
     (concat [x1 [bd 1]
              (+ x1 0x40) [ch 0.2]
              (+ x1 0x80) [oh 1]
              (+ x1 0x80) [ch 4]
              (+ x1 0xc0) [ch 1.2]]
             (techno-loop* x (inc n))))))






(defn* out [x]
  ['techno-loop 0])

(defn* debug [x]
  (println @db/db))

(defn* bass-drum [x]
  (if (= 0x0f00 (mod x 0x1000))
    [0x000 [* 4.4 [bd 1]]
     0x080 [* 4.4 [bd 0.8]]
     0x100 [* 2.4 [oh 0.1]]]
    [* 4.4 [bd 1]]))

#_(def bass-drum #'bass-drum*)

(defn* s1 [x f]
  [0x000 [moog-synth f]
   ;0x020 [moog-synth f]
   0x040 [moog-synth (* f 2)]
   ;0x060 [moog-synth (* f 3)]
   0x080 [moog-synth (* f 3)]
   ;0x0a0 [moog-synth (* f 2)]
   0x0c0 [moog-synth (* f 7)] 0x0c0 [moog-synth (* f 12)]
   ])

(defn* bass-drum2 [x]
  [0x000 [bd 1]
   0x450 [ch 1]])

(defn* out [x]
  [0x00 [bass-drum]
   0x00 [s1 15]
   0x40 [* 0.8 [ch 0.4]] 0x45 [ch 6.1]
   0x80 [moog-synth 28]  0x80 [oh 1]
   0xc7 [ch 6]
   0xc7 [* 1.3 [bd 0.8]]
   0xd9 [moog-synth 18]
   0xf0 [* {0 0 0x34 0 0x40 1 0x100 1} [oh 0.2]]
   0x100])


(defn* out [x]
  [MoogLP [0x00 [bass-drum]
               0x40 [* 0.8 [ch 0.2]] 0x45 [ch 6.1]
               0x80 [moog-synth 28]  0x80 [oh 1]
               0xc7 [ch 6]
               0xd9 [moog-synth 18]
               0xf0 [oh 0.2]
               0x100
           ]
   700
   0.7
   ])

(defn* out [x]
  [MoogLP [0x00 [bd 1]
               0x100]
   600
   0.9])

(defn* a [x]
  [0x000 [* {0 1 0x10 1 0x200 0}
          [MoogHP [ch 0.2]
           (+ (rand-int 200) 800) 0.6]]
   0x060 [ch 6]
   0x170 [MoogLP [lt 2.2]
          300 0.4]])

(defn* drums [x]
  [0x000 [bass-drum]
   0x080 [cp 2.5]
   0x100 [bd 0.8]
   0x140 [bass-drum]
   0x180 [cp 2.5]])

(defn* hihats [x]
  [MoogHP [0x000 [oh 2]
           0x044 [oh 2]
           0x080 [oh 2.1]
           0x0c4 [oh 2]
           0x100 [oh 2]
           0x144 [oh 2]
           0x180 [oh 2.2]
           0x1c4 [oh 2]]
   (+ (rand-int 1000) 6000)
   0.4])

(defn* moogs [x]
  [0x080 [* 4 [moog-synth 10]]
   0x0c0 [* 4 [moog-synth 15]]
   0x180 [* 4 [moog-synth 10]]
   0x1c0 [* 4 [moog-synth 45]]])

(defn* out [x]
  [0x000 [noise 400] 0x000 [drums] 0x000 [hihats] 
   0x100 [a] 0x100 [* 4 [cp 1]]
   0x180 [moogs]
   0x200])

(defn* out [x]
  [0x000 [bass-drum] 0x000 [s1 12]
   0x020 [a]
   0x080 [moog-synth 12]
   0x080 [* 3 [Saw 2] [ch 2]]
   0x100 [bass-drum] 0x100 [cp 1] 0x100 [s1 8]
   0x120 [a]
   0x180 [* 3 [Saw 2] [ch 2]]
   0x1c0 [moog-synth 10]
   0x200])

(defn* out [x]
  [;0x000 [foo-loop]
                                        ;0x100 [foo-loop]
   0x000 [bd 1.2]
   0x100 [cp 1]
   0x1c0 [lt 1]
   0x200 [cp 1]
   0x300 [cp 11]
   0x400 [cp 21]
   0x500 [cp 11]
   0x600 [cp 1]
   0x800 [cp 1]])

(defn* out [x]
  [0x000 [bd 1]
   0x100 [oh 1]
   0x200 [cp 1]
   0x300 [oh 1]
   0x400 [cp 1]
   0x500 [oh 1]
   0x600 [cp 1]
   0x700 [oh 1]
   0x800
   ])

(defn* out [x]
  [0x000 [bd 1]
   0x100 [ch 1]
   0x200 [cp 1]
   0x300 [ch 1]
   0x400 [cp 1]
   0x500 [ch 1]
   0x600 [cp 1]
   0x700 [ch 0.1]
   0x800
   ])

(defn* out [x]
  [0x000 [lt 1]
   0x100 [lt 1]
   0x200 [lt 1]
   0x300 [lt 1]
   0x400 [lt 1]
   0x500 [lt 1]
   0x600 [lt 1]
   0x700 [lt 1]
   0x800 [lt 1]
   0x900 [lt 1]
   0xa00 [lt 1]
   0xb00 [lt 1]
   0xc00 [lt 1]
   0xd00 [lt 1]
   0xe00 [lt 1]
   0xf00 [lt 1]])









(defn lfo [low high freq]
  [+ (/ (+ low high) 2)
   [* (/ (- high low) 2)
    [Sine freq]]])


(defn* hhs [x]
  [* 0.3 [MoogHP [0x000 [ch 2]
                  0x044 [ch 2]
                  0x080 [ch 2]
                  0x0c4 [ch 2]]
          (lfo 1000 2000 0.3)
          0.6]])

(defn* sy [x f]
  [* 1.3 [MoogLP
        [* {0 1 0x010 1 0x060 0 0x080 0.5 0x0a0 0}
         [+ [Saw f]
          [* 0.3 [Saw (* f 1.98 (+ 1 (rand-int 2)))]]
          [* 0.3 [Saw (* f 3.99 (+ 1 (rand-int 2)))]]]]
        [+ 200 [* 5 {0 100 0x010 100 0x060 0 0x080 50 0x090 1000}]]
        0.89]])


(defn* clap [x]
  (if (= 0 (mod x 0x200))
    nil
    [0 [cp 0.7]
     0x040 [* 0.3 [cp 0.7]]]))

(defn* toms [x]
  [* 0.7 [MoogLP [0x000 [lt 1]
                  0x040 [lt 1.5]
                  0x080 [lt 0.75]
                  0x0c0 [lt 1.5]]
          [:c 11 200 2000]
          [:c 12 0 0.9999]]])

(defn* l [x]
  [0x000 [* 3 [bd {0 2 4 1 0x100 1}]]
   0x000 [hhs]
   0x000 [toms]
   0x000 [clap]
   0x040 [sy 12]
   0x080 [ch 3] 0x080 [sy 16]
   0x0c0 [sy 12]
   0x0C0 [lt 0.1]
   0x100])

(defn* sw [x f]
  [* 0.2 [Dist [MoogLP [Dist [* {0 0.1 30 1 0x100 0}
                              [Sine f]]
                        1.36]
                [* {0 580
                    (+ (rand-int 10) 0x16) 370
                    0x0c0 60
                    0x100 130} :b1]
                0.9]
          0.5]])

(defn* lts [x f]
  [MoogHP [0x000 [lt (* f 1.25)]
           0x047 [lt (* f 1.5)]
           0x0c8 [lt (* f 1.75)]]
   #_(+ (rand-int
         50) 250)
   [:c 11 90 600]
   0.5])

(defn* hsw [x]
  (cond
    (= 0x7F00 (mod x 0x8000))
    [0x80 [* 4 [bd 0.7]]]

    (= 0 (mod x 0x8000))
    [* 3 [vc 0.4]]

    (= 0 (mod x 0x2000))
    [* 0.7 [ch 0.35]]

    (= 0 (mod x 0x4000))
    [0 [sw 300]
     0x70 [sw 333]
     0x80 [sw 277]
     0x140 [sw 211]
     0x240 [sw (* 211 (+ 1 (rand-int 4)))]]))

(defn* l [x]
  [0x000 [* 3 [bd 0.8]]
   0x000 [hhs] 0x00 [clap]
   0x000 [hsw]
   0x040 [lts 2.3]
   0x060 [sw 100]
   0x080 [lt 1.4] 0x080 [oh 1]
   0x080 [sw 201]
   0x0c0 [* {0 1 10 1 0x170 0} [oh 0.18]]
   0x100])

(defn* out [x]
  {:b1 (lfo 1 13 0.01)
   :<- [* 4 [l]]})























;; 8r025 = 1/3 beat (21.33)
;; 8r053 = 2/3 beat (42.66)
;; 8r100 = 1 beat (64)
;; 8r1000 = 1 bar (8 beats)
;; 8r10000 = 8 bars (64 beats)
;; 8r001 = 1/64 beat, 375 samples at 120bpm
;; 375 = (/ sample-rate (/ bpm 60) 64)

;; 0x53 = 1/3 beat (83.33)
;; 0xab = 2/3 beat (170.6)
;; 0x100 = 1 beat (256)
;; 0x800 = 1 bar (8 beats) (2048)
;; 8r10000 = 8 bars (64 beats)
;; 8r001 = 1/64 beat, 375 samples at 120bpm
;; 93.75 = (/ sample-rate (/ bpm 60) 256)




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
                        [Saw freq]]}
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

