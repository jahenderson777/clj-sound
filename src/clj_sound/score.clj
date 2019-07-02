(ns clj-sound.score
  (:import UGen SawTooth SoundUtil CubicSplineFast Player)
  (:require [clojure.string :as str]
            [clojure.walk :as walk]))


(defn saw-tooth
  ([] -1)
  ([x n y1 [freq]]
   (let [y (- y1 (/ 1 freq))]
     (repeat 2 (if (<= y -1)
                 1
                 y)))))

(defn mul
  ([x] nil)
  ([x _ [& inputs]]
   (apply * inputs)))

(defn mix
  ([] nil)
  ([x _ [& inputs]]
   (apply + inputs)))

(defn reverb
  ([x] nil)
  ([x _ [in]]
   in))

(defn sine-wave
  ([] nil)
  ([x _ [freq]]
   (Math/sin freq)))

(defn sine
  ([initial-x]
   {:inital-x initial-x
    :obj (doto (SimpleOsc.)
           (.init 48000))})
  ([obj buf [freq]]
   (.compute obj (count buf) buf buf freq)))

(def buffers (atom {}))

(defn mul [& bufs]
  (SoundUtil/multiplyBuffers (into-array bufs)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn a-synth [x freq]
  [mul
   {0 1 50 0}
   [0 [SawTooth freq]
    ;30000 [SawTooth freq]
    ;60000 [SawTooth freq]
    ]])

(defn lazy-melody [x n]
  (lazy-seq (cons (* 10 n) (cons ['a-synth (* 100 (inc (rand-int 10)))] (lazy-melody x (inc n))))))

(defn melody [x freq]
  [0 ['a-synth freq]
   40000 ['a-synth 200]])

(defn noise [x freq]
  [0 [SawTooth [0 freq

                0 [mul 1
                   [SawTooth 1]]]]
   0 [SawTooth [0 [mul 2.01 freq]

                0 [mul 1
                   [SawTooth 1.1]] ]]])

(defn out [x]
  #_[0 [SawTooth 2000]
   2 [SawTooth 2000]]
  {:b1 [0 ['noise {0 500
                   50000 400
                   100000 700}]
        0 ['noise 301]]                      ;['lazy-melody 0]
                                        ;100000 ['a-synth 200]
                                        ;200000 ['a-synth 301]
                                        ;100000 ['a-synth 553]
                                        ;400000 ['a-synth 551]
   
   ;:b0 ['a-synth 10]
   :<- :b1})

(defn execute [x {:keys [fn start-x] :as m}]
  (println "execute " *ns* m)
  (let [ret (apply (ns-resolve 'clj-sound.score (first fn)) x (rest fn))]
    ;(println "ret " m)
    (-> (if (map? ret)
          ret
          {:<- ret})
        (assoc :fn fn :start-x start-x))))

#_(defn process-buses [m]
  (let [output (:<- m)
        buses (conj (into [] (into (sorted-map) (dissoc m :<- :fn :x1)))
                    [:<- output])]
    (for [[bus v] buses]
      (println bus v))))

(defn ordered-buses [m]
  (into (sorted-map) (dissoc m :<- :fn :start-x)))

(defn monophonic-pass-line [x x1 cut-off]
  ['lp-filter ['mul [SawTooth [0 100
                               50 125
                               100 150
                               150 75]]
               [0 {0 1 5 0}
                50 {0 1 5 0}
                100 {0 1 5 0}
                150 {0 1 5 0}]]
   ['mix
    cut-off
    [0 {0 1 5 0}
     50 {0 1 5 0}
     100 {0 1 5 0}
     150 {0 1 5 0}]]])


(defn out2 [x x1]
  {:b1 [0 ['a-synth 10]
        10 ['a-synth 20]]
   :b2 [5 ['a-synth 10]

        15 ['a-synth 20]]
   :<- ['mix :b1 :b2]})

#_{:b1 {[0 'a-synth] [10]
      [10 'a-synth] [20]
      [20 'mix] [['a-synth 5] ['a-synth 15]]}}


;; if the node is a vector with first element being an int, then the ugens should be created at the right momement
;; if the node is a map with int keys, it is an envelope
;; if the node is a map with non-int keys then it is a bus router, a set represents a split, e.g. #{:<- :b1}
;; if the node is a vector with the first element being a class, then instatiate that class with appropriate initial-x
;; if the node is a vector with the first element being a symbol, then execute that symbol's function
;; if the node is a vector with the first element being a function, then this is a built in function, leave as is

#_(loop [[a b & tail] (repeatedly #(rand-int 20))
       new-seq []]
  (if (= a 10)
    (concat new-seq tail)
    (recur tail (conj new-seq {:a a :b b}))))

(defn construct [class & args]
  (clojure.lang.Reflector/invokeConstructor class (into-array Object args)))

(defn build-graph [n x-buf x node]
  ;(println "build-graph " n x-buf x "node=*" node "*")
  (cond (or (instance? clojure.lang.LazySeq node) (list? node) (vector? node))
        (cond (int? (first node))
              (build-graph n x-buf x {:seq :polyphonic
                                      :start-x x
                                      :data node})

              (class? (first node))
              (do ;(println "constructing " (first node) (- x-buf x))
                  (concat [(construct (first node) (- x-buf x))] (mapv #(build-graph n x-buf x %)
                                                                       (rest node))))

              (or (fn? (first node))
                  (instance? UGen (first node)))
              (concat [(first node)] (mapv #(build-graph n x-buf x %)
                                           (rest node)))

              (symbol? (first node))
              (build-graph n x-buf x (execute x {:fn node :start-x x}))

              :else
              node)

        (map? node)
        (cond (:seq node)
              (-> node
                  (update :data
                          (fn [s]
                            (let [start-x (:start-x node)
                                  x1 (- x-buf start-x)]
                              (loop [[t sequenced-node & tail] s
                                     new-sequence '()]
                                ;(println sequenced-node "start-x=" start-x "x=" x "x1=" x1 "t=" t "(+ x1 n)=" (+ x1 n))
                                (let [sequenced-node2 (if (< t (+ x1 n))
                                                        ;; TODO can't understand why (+ start-x t) doesn't cause a problem,
                                                        (build-graph n x-buf (+ start-x t) sequenced-node)
                                                        sequenced-node)
                                      new-sequence2 (concat new-sequence [t sequenced-node2])] ; TODO don't concat if nil (ended)
                                  (if (or (not (seq? tail))
                                          (>= t (+ x1 n)))
                                    (concat new-sequence2 tail)
                                    (recur tail new-sequence2))))))))

              (int? (first (first node)))
              (Player. (- x-buf x)
                       (double-array (keys node))
                       (double-array (vals node)))

              :else
              (into {} (map (fn [[k v]]
                              [k (if (not (#{:fn :start-x} k))
                                   (build-graph n x-buf x v)
                                   v)])
                            node)))

        :else
        node))

#_(defn get-or-create-buffer [n bus]
  (if-let [b (bus @buffers)]
    b
    (let [fa (float-array n)]
      (swap! buffers assoc bus fa)
      fa)))

(defn add-bufs [& bufs]
  ;(println "add-bufs" bufs)
  (SoundUtil/sumBuffers (into-array bufs)))

(defn mul-bufs [& bufs]
                                        ;(println "add-bufs" bufs)
  (SoundUtil/multiplyBuffers (into-array bufs)))

(defn process-node [n x node]
  ;(println "process-node" node)
  ;(Thread/sleep 100)
  (cond (and (map? node)
             (:seq node))
        (let [start-x (:start-x node)
              node (:data node)
              x1 (- x start-x)]
          (loop [[t sequenced-node & tail] node
                 bufs-to-sum []]
            ;(println "bufs-to-sum=" bufs-to-sum "t, start-x, n, tail" t start-x n tail)
            (let [bufs-to-sum (if (< t (+ x1 n))
                                (conj bufs-to-sum (process-node n x sequenced-node))
                                bufs-to-sum)]
              (if (or (not (seq? tail))
                      (>= t (+ x1 n)))
                (do ;(println bufs-to-sum)
                    (apply add-bufs bufs-to-sum))
                (recur tail bufs-to-sum)))))

        (map? node)
        (let [x1 (or (:x1 node) x)]
          (do (doseq [[bus v] (ordered-buses node)]
                (swap! buffers update bus (fn [b] (add-bufs (or b (float-array n))
                                                           (process-node n x1 v)))))
              (process-node n x1 (:<- node))))

        (number? node)
        (do ;(println "filling buffer " node)
            ;["foo"]
            (SoundUtil/filledBuf n node)
            )

        (or (instance? clojure.lang.LazySeq node) (list? node) (vector? node))
        (cond ;(int? (first node))
              

          (instance? UGen (first node))
          (do ;(println "process" (first node))
              (.process (first node) n (into-array (map (partial process-node n x) (rest node)))))

              (= (first node) mul)
              (apply mul-bufs (map (partial process-node n x) (rest node))))

        (instance? Player node)
        (.process node n (make-array Float/TYPE 0 0))

        (keyword? node)
        (node @buffers)))

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



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;







(comment
(def fa (float-array 4))
(def st (SawTooth. 2))

(.process st 4 fa (float-array (repeat 4 200)))


  (time (loop [i 0
               machine (process-node (build-graph 0 (out)))]
          (if (> i 48000)
            machine
            (recur (inc i) (process-node (second machine)))))))


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
