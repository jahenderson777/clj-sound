(ns clj-sound.score
  (:import SawTooth))


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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn a-synth [freq]
  [mul
   {0 1 50 0}
   [SawTooth freq]])

(defn melody [freq]
  [0 ['a-synth freq]
   4 ['a-synth 200]])

(defn out [x x1]
  {:b1 [0 ['melody 400]
        10 ['melody 300]]
   :b0 ['a-synth 100]
   :<- ['reverb :b1]})

(defn execute [{:keys [fn x x1] :as m}]
  (let [ret (apply (resolve (first fn)) x x1 (rest fn))]
    (-> (if (map? ret)
          ret
          {:<- ret})
        (assoc :fn fn :x x :x1 x1))))

(defn process-buses [m]
  (let [output (:<- m)
        buses (conj (into [] (into (sorted-map) (dissoc m :<- :fn :x :x1)))
                    [:<- output])]
    (for [[bus v] buses]
      (println bus v))))

(defn ordered-buses [m]
  (let [output (:<- m)]
    (conj (into [] (into (sorted-map) (dissoc m :<- :fn :x :x1)))
          [:<- output])))


(defn out2 [x x1]
  {:b1 [0 ['a-synth 10]
        10 ['a-synth 20]]
   :b2 [5 ['a-synth 10]
        15 ['a-synth 20]]
   :<- ['mix :b1 :b2]})

(defn build-graph [n x old-fn-map]
  "1. re-execute the node function and store this as 'new-node'
   2. "
  (let [fn-map (transient old-fn-map)
        new-fn-map (execute old-fn-map)]
    (for [[bus node] (ordered-buses new-fn-map)]
      [bus (cond (and (vector? node)
                      (int? (first node)))
                 (loop [[t sequenced-node & tail] node
                        new-or-updated-nodes []]
                   (if (or (not (seq? tail))
                           (>= t (+ x n)))
                     new-or-updated-nodes
                     (recur tail
                            (cond (and (>= t x)
                                       (< t (+ x n)))
                                  (conj new-or-updated-nodes [t sequenced-node])

                                  ;; TODO check if past sequenced event exists in old-fn-map, if so maybe modify arguments

                                  :else
                                  new-or-updated-nodes)))
                   ))])
    ))




;; if a sequenced event is in the past in the new one, but doesn't exist in the old one (matching by time & Class), remove it
;; any sequenced events that exist in the old one but not in the new one (matching by time=time & class=object), let them carry on,
;; if a sequenced event is in the near future in the new, but not in the old, mix it in 
;; if exists in both then change any parameters (unless they are nodes, in which case recur)
;; any un-sequenced UGens/Events/Nodes that exist in the old but not the new, fade them out
;; any un-sequenced UGens/Events/Nodes that exist in the new but not the old, fade them in

(defn a-synth [{:keys [freq]}]
  {:s mul
   :c []})

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
   :x1 10
   :b1 [0 {:fn ['melody 400]
           :x 1000
           :x1 1000
           :<- [0 {:fn ['a-synth freq]
                   :x 1000
                   :x1 1000
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

(defn construct [class & args]
  (clojure.lang.Reflector/invokeConstructor class (into-array Object args)))

(defn build-graph [n x node]
  (println "build graph" n x node)
  (cond (number? node)
        node

        (and (vector? node)
             (int? (first node)))
        (if (= 2 (count node))
          (if (<= (first node) (+ x n))
            (build-graph n (- x (first node)) (second node))
            node)
          (->> (partition 2 node)
               (mapv (fn [pair]
                       (if (<= (first pair) (+ x n))
                         (build-graph n (- x (first pair)) (second pair))
                         (vec pair))))
               (conj [mix 0 nil])))

        (or (symbol? node)
            (and (vector? node)
                 (symbol? (first node))))
        (if (symbol? node)
          (build-graph n x ((resolve node)))
          (build-graph n x (apply (resolve (first node))
                                (rest node))))

        (map? node)
        0.234234

        (and (vector? node)
             (fn? (first node)))
        (let [ugen ((first node) x)
              args (mapv #(build-graph n x %)
                         (rest node))]
          [(first node) ugen args])

        :else
        node))

(defn process-node [node]
  (if (number? node)
    (list node node)
    (let [[f x state args] node]
      (if (fn? f)
        (let [processed-args (doall (map #(if (vector? %)
                                            (->> (if (and (int? (first %))
                                                          (= (first %) x))
                                                   (build-graph x (second %))
                                                   %)
                                                 process-node)
                                            %)
                                         args))
              ugen-output (f x state (map #(if (list? %)
                                             (first %)
                                             %)
                                          processed-args))

              [new-samp new-state] (if (seq? ugen-output)
                                     ugen-output
                                     [ugen-output nil])]
          (list new-samp [f (inc x) new-state (map #(if (list? %)
                                                      (second %)
                                                      %)
                                                   processed-args)]))
        (list 0 node)))))



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
