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

(def db {:tracks {:out {:components {10 :foo}}

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

(def db {'track-1 })

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

(defn lp-filter [x input cutoff]
  [(+ (* (input x))
      (* (input (- x 1)) 0.123)
      (* (input (- x 2)) -0.123))])

(defugen track-1 [x]
  (lp-filter x
             (fn [x] (* (saw-wave x 440)
                       (volume-env x)))
             1000)
  [lp-filter ]
  [saw-wave 0 440]
  [volume-env 0 12])

(defugen track-1 [x]
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

(defn run-ugen [ugen x & args]
  (let [[y & ugens] (apply ugen x args)
        processed-ugens (map (fn [[ugen & args]]
                               (apply ugen args))
                             ugens)]
    (if (seq? ugens)
      (apply ugen x (concat args processed-ugens))
      y)))

;; if a ugen returns a value it is a simple ugen like a sine osc.

[[bass-drum 0]
 [hihat 0]]

[[volume-env 0 10]
 [pitch-env 0]
 [sample 0 "pad.wav" 1.0]]

(defn another-synth [x & [vol pitch samp]]
  ())

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
