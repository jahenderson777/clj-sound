(ns clj-sound.score)


(defn saw-tooth
  ([] -1)
  ([x n y1 [freq]]
   (let [y (- y1 (/ 1 freq))]
     (repeat 2 (if (<= y -1)
                 1
                 y)))))

(defn mul
  ([] nil)
  ([x _ [& inputs]]
   (apply * inputs)))

(defn mix
  ([] nil)
  ([x _ [& inputs]]
   (apply + inputs)))

(defn reverb
  ([] nil)
  ([x _ [in]]
   in))

(defn sine-wave
  ([] nil)
  ([x _ [freq]]
   (Math/sin freq)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn a-synth [freq]
  [mul
   {0 1 50 0}
   ;[25 [sine-wave 1000]]
   [saw-tooth freq]])

(defn melody [freq]
  [0 ['a-synth freq]
   4 ['a-synth 200]])

(defn out []
  [reverb [0 ['melody 400]
           10 ['melody 300]]])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
        (let [initial-state ((first node))
              args (mapv #(build-graph n x %)
                         (rest node))]
          [(first node) x initial-state args])

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
