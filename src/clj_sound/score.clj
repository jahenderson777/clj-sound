(ns clj-sound.score)


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

(defn build-graph [x node]
  (cond (number? node)
        node

        (and (vector? node)
             (int? (first node)))
        (if (= 2 (count node))
          (if (= (first node) x)
            (build-graph 0 (second node))
            node)
          (->> (partition 2 node)
               (mapv (fn [pair]
                       (if (= (first pair) x)
                         (build-graph 0 (second pair))
                         (vec pair))))
               (conj [mix 0 nil])))

        (or (symbol? node)
            (and (vector? node)
                 (symbol? (first node))))
        (if (symbol? node)
          (build-graph 0 ((resolve node)))
          (build-graph 0 (apply (resolve (first node))
                                (rest node))))

        (map? node)
        0.234234

        (and (vector? node)
             (fn? (first node)))
        (let [initial-state ((first node))
              args (mapv #(build-graph x %)
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
