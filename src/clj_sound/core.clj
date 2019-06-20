(ns clj-sound.core
  (:require [fastmath.core :as m]
            [clj-sound.ui :as ui]
            [clojure.data.int-map :as i]
            [clojure.data.avl :as avl])
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







(defn t-pos [db f-sym t x]
  ;(println "t-pos" f-sym t x)
  (when (pos? x)
    (or (when-let [r (get-in db [:tracks t f-sym :resets])]
          (r x))
        (when-let [y (t-pos db f-sym t (dec x))]
          (inc y)))))

(defn << [db t x]
                                        ;(println "<<" f-sym t x)
  (let [f-sym (get-in db [:tracks t :instrument])]
    (when-let [y (t-pos db f-sym t x)]
      ((resolve f-sym) db t y))))

(def db2 {:tracks {:out {:components {10 :foo}}

                  'my-synth {}

                  12 {:instrument 'my-synth
                      'sine-wave {:resets {0 0
                                           7 0
                                          14 0}}
                      'volume-env {:decay 1
                                   :resets {0 0
                                            7 0
                                            14 2}}}

                  13 {:instrument 'track-player
                      'track-player {:track-to-play 12
                                     :resets {4 0
                                              27 0}}}}})

;(def db {'track-1 })

(defn volume-env [x decay]
  (double (/ 1 (inc (* x decay)))))

(defn sine-wave [x freq]
  (Math/sin (* x freq)))

(defn my-synth [x decay freq]
  (* (volume-env x decay)
     (sine-wave x freq)))

(defn track-1 [x]
  (let [resets {0 0
                10 0
                20 2}]
    ))

(defn my-synth [db t x]
  (* (or (<< db 'sine-wave t x) 0)
     (or (<< db 'volume-env t x)
         0)))

;; samples, tracks, instruments, envelopes, effects are all ugens
;; they are all functions of time returning a sample value (f [x & args] y)
;; they all have parameters (signal inputs) that can be automated by other ugens
;; the ugen function can compose other ugens
;; their outputs can all be visualized as a waveform in the ui
;; [note events on a track] is also a ugen function even if it is a map
;; ugens are always 'playing' at a certain rate (default 1). Playing means that their position increases by their rate as each sample progresses.
;; ugen position and rate can be set by events
;; ugen instances? how do they work?
;; the main output ugen is called 'out'.
;; the active ugens at a particular sample point must be calculable from the sample position,
;; ... so how to do this?
;; call (out 123) and you get a graph of ugens, ugens can spawn more ugens
;; so ugen function, in addition to returning the sample value at that point needs to return any new ugens that have been spawned

;; sequence everything in edn
;; each ugen can either be a constant value,
;; a function, of (f [x] y)
;; or an avl/sorted-map in which case, we interpolate any value that is not in the map
;; ugen signal inputs are regular function inputs (f [instance-id x freq amp filter] ...)
;; ugen outputs are either a single value, for single output. or a vector of outputs.

;; how are ugens wired up? by data? or by code? or both somehow?

'{:bass-drum [aux-send
              [drum-synth
               {0 1 50 0.5 100 0}       ; volume env
               {0 2 100 0.2}            ; pitch env
               [sine-wave 0.001]        ; filter lfo
               ]
              :bus/main-reverb
              ]
  :clap [sampler "clap.wav"]
  :bongo [aux-send
          [drum-synth
           {0 1 50 0.5 100 0}           ; volume env
           {0 12 100 10.2}              ; pitch env
           [sine-wave 0.001]            ; filter
           ]
          :bus/main-reverb]
  :hihat [white-noise
          {0 1 40 0}                    ; volume env
          ]
  :filtered-hihat [hp-filter
                   [:hihat]
                   {0 200 40 220}       ; freq
                   :1                   ; q, expose as first parameter
                   ]
  :bass-synth [lp-filter
               [saw-tooth
                {0 1 100 0}
                100]
               {0 :1 100 0.1}
               0.7]
  :short-drum-loop [compressor
                    [reverb
                     [0 [multiplier [:bass-drum] 2]
                      250 [:filtered-hihat 0.7]
                      500 [:hihat]
                      750 [:bongo]
                      1000 [:bass-drum]
                      1000 [:clap]
                      1250 [:bass-synth :1]]]]
  :pulse-env {0 1 100 0}
  :monophonic-bass-line [multiplier
                         [0 0
                          250 [:pulse-env]
                          500 [:pulse-env]
                          750 [:pulse-env]] ; vol
                         [lp-filter
                          [saw-tooth 1
                           [0 33
                            250 66
                            500 33
                            750 99]]
                          :1
                          :midi/cc.10]]
  :bass-line-track [repeat
                    [:monophonic-bass-line [sine-wave 0.0000001]]
                    1000]
  :out [0 [:bass-line-track]
        0 [repeat [:short-drum-loop [sine-wave 0.00001]] 1000]
        0 [reverb :bus/main-reverb]]}

(defn saw-tooth
  ([] -1)
  ([x y1 [freq]]
   (let [y (- y1 (/ 1 freq))]
     (repeat 2 (if (<= y -1)
                 1
                 y)))))

(defn mul
  ([] nil)
  ([x _ [& inputs]]
   (apply * inputs)))

(defn reverb
  ([] nil)
  ([x _ [in]]
   in))

(defn sine-wave
  ([] nil)
  ([x _ [freq]]
   (m/qsin freq)))

(def music
  {:a-synth [mul
             {0 1 50 0}
             [25 [sine-wave 1000]]
             [saw-tooth :1]]
   :melody [0 [:a-synth :1]
            40 [:a-synth 200]]
   :out [reverb
         [0 [:melody 400]
          100 [:melody 300]]]})

(def input-ks
  {:1 1
   :2 2
   :3 3
   :4 4
   :5 5
   :6 6
   :7 7})



(defn process-node [song node x state]
                                        ;(println "process-node" node)
  (cond (number? node)
        node

        (and (vector? node)
             (int? (first node)))
        (->> (partition 2 node)
             (mapv (fn [pair]
                     (if (= (first pair) x)
                       (process-node song
                                     (second pair)
                                     0 state)
                       pair)))
             )        ; instead of nil need to replace-in with appropriate input

        (or (keyword? node)
            (and (vector? node)
                 (keyword? (first node))))
        (if (keyword? node)
          (process-node song (get song node) 0 state)
          (let [next-node (get song (first node))
                inputs node
                replaced-inputs (clojure.walk/postwalk #(if-let [input-num (input-ks %)]
                                                          (nth inputs input-num)
                                                          %)
                                                       next-node)]
            (process-node song replaced-inputs x state)))

        (map? node)           ; TODO interpolate properly the map as an envelope
        0.3731093

        (and (vector? node)
             (fn? (first node)))
        (let [initial-state ((first node))
              processed-args (map #(process-node song % x state)
                                  (rest node))]
          (into [(first node) x initial-state] processed-args)
                                        ;(apply str (first node) " " initial-state " what do we do now we have initial state?" processed-args)
          )

        :else
        node))

(defn next-sample [song x state]
  (process-node song (:out song) x state))

;; (next-sample music 0 0)

(defn lp-filter [x input cutoff]
  [(+ (* (input x))
      (* (input (- x 1)) 0.123)
      (* (input (- x 2)) -0.123))])

#_(defugen track-1 [x]
  (lp-filter x
             (fn [x] (* (saw-wave x 440)
                       (volume-env x)))
             1000)
  [lp-filter ]
  [saw-wave 0 440]
  [volume-env 0 12])

#_(defugen track-1 [x]
  (lp-filter x
             (* (saw-wave x 440)
                (volume-env x))
             1000)
  [lp-filter ]
  [saw-wave 0 440]
  [volume-env 0 12])


(defn bass-drum [x]
  (* (volume-env x 1)
     1))

(defn track-1 [x & [volume & inputs]]
  (println "track1" x volume inputs)
  (into [(* volume
            (apply + inputs))]
        (case (mod x 32)
          0 [['bass-drum 0]]
          nil)))

#_(defn run-ugen [ugen x & args]
  (let [[y & ugens] (apply ugen x args)
        processed-ugens (map (fn [[ugen & args]]
                               (apply ugen args))
                             ugens)]
    (if (s
         eq? ugens)
      (apply ugen x (concat args processed-ugens))
      y)))

;; if a ugen returns a value it is a simple ugen like a sine osc.
(comment
  [[bass-drum 0]
   [hihat 0]]

  [[volume-env 0 10]
   [pitch-env 0]
   [sample 0 "pad.wav" 1.0]]

  (defn another-synth [x & [vol pitch samp]]
    ())


  {:volume {0 1
            10 0.5
            20 0.25
            30 1}
   :reverb 1}


  (defn out [x]
    (when (= 0 x)
      [(* (master-volume x)
          (+ track-1 x)
          (+ track-2 x)
          (+ track-3 x))

       [master-volume]
       [track-1]
       [track-2]
       [track-3]]))


  (def db {:next-id 0
           :static-ugens {0 [out 0]}
           :transient-ugens {}})

  (defn synth-b [db x]
    (when (= 0 x)
      ()))




  (defn track-player [db t x]
    (let [track-to-play (get-in db [:tracks t 'track-player :track-to-play])
          instr (get-in db [:tracks track-to-play :instrument])]
      (when-let [y (t-pos db 'track-player t x)]
        ((resolve instr) db track-to-play y))))


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
                             (source-sample (+ y (/ 1 pitch)))))]))))

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
