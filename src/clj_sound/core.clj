(ns clj-sound.core
  (:require [fastmath.core :as m]
            [clj-sound.ui :as ui]
            [clj-sound.score :as score]
            [clj-sound.db :as db]
            [clojure.data.int-map :as i]
            [clojure.data.avl :as avl])
  (:import (javax.sound.sampled Mixer Mixer$Info AudioSystem DataLine$Info SourceDataLine AudioFormat AudioFormat$Encoding)
           SimpleOsc WavFile UGen SoundUtil CubicSplineFast Player EnvPlayer Sampler)
  (:gen-class))

(set! *unchecked-math* true)

(def audio-format
  (AudioFormat. AudioFormat$Encoding/PCM_SIGNED
                48000 ; sample rate
                16    ; bits per sample
                2     ; channels
                4     ; frame size 2*16bits [bytes]
                48000 ; frame rate
                false ; little endian
                ))

(def buffer-size 800)
(def player (agent 0))

(def buffers (atom {}))

(defn unsigned-byte [x]
  (byte (if (> x 127) (- x 256) x)))

(defn little-endian [size x]
  (map #(-> (bit-shift-right x (* 8 %))
            (bit-and 255)
            unsigned-byte)
       (range size)))

(defn execute [x {:keys [fn start-x]}]
  (let [ret (apply (ns-resolve 'clj-sound.score (first fn)) x (rest fn))]
    (-> (if (map? ret)
          ret
          {:<- ret})
        (assoc :fn fn :start-x start-x))))

(defn ordered-buses [m]
  (into (sorted-map) (dissoc m :<- :fn :start-x)))

(defn construct [class & args]
  (clojure.lang.Reflector/invokeConstructor class (into-array Object args)))

(declare build-graph)

(defn build-graph-seq [n x-buf x node]
  (let [updated
        (-> node
            (update
             :data
             (fn [s]
               (let [start-x (:start-x node)
                     x1 (- x-buf start-x)]
                 (loop [[t sequenced-node & tail] s
                        new-sequence '()]
                   (let [new-sequence2
                         (as-> sequenced-node $
                           (if (< t (+ x1 n))
                             (build-graph n x-buf (+ start-x t) $) ;; TODO can't understand why (+ start-x t) doesn't cause a problem,
                             $)
                           (if $
                             (concat new-sequence [t $])
                             $))]
                     (if (or (not (seq? tail))
                             (>= t (+ x1 n)))
                       (concat new-sequence2 tail)
                       (recur tail new-sequence2))))))))]
    (when (seq (:data updated))
      updated)))

(defn build-graph [n x-buf x node]
  (cond (or (instance? clojure.lang.LazySeq node) (list? node) (vector? node))
        (cond (int? (first node))
              (build-graph n x-buf x {:seq :polyphonic
                                      :start-x x
                                      :data node})

              (instance? CubicSplineFast (first node))
              (concat [(Sampler. (- x-buf x) (first node))]
                      (mapv #(build-graph n x-buf x %) (rest node)))

              (class? (first node))
              (concat [(construct (first node) (- x-buf x))]
                      (mapv #(build-graph n x-buf x %) (rest node)))

              (or (fn? (first node))
                  (instance? UGen (first node)))
              (if (and (instance? Sampler (first node))
                       (.-ended (first node)))
                nil
                (let [inputs (mapv #(build-graph n x-buf x %) (rest node))]
                  (when-not (or (and (not (= + (first node))) (some nil? inputs))
                                (and (= + (first node)) (every? nil? inputs)))
                    (concat [(first node)] inputs))))

              (symbol? (first node))
              (build-graph n x-buf x (execute x {:fn node :start-x x}))

              :else node)

        (map? node)
        (cond (:seq node)
              (build-graph-seq n x-buf x node)

              (int? (first (first node)))
              (EnvPlayer. (- x-buf x)
                          (double-array (keys node))
                          (double-array (vals node)))

              :else
              (let [buffers (dissoc node :fn :start-x)
                    others (select-keys node [:fn :start-x])
                    processed-buffers (map (partial build-graph n x-buf x) (vals buffers))]
                (when (some identity processed-buffers)
                  (merge (zipmap (keys buffers) processed-buffers)
                         others))))

        (instance? EnvPlayer node)
        (when-not (.-ended node)
          node)

        :else node))

(defn add-bufs [& bufs]
  (SoundUtil/sumBuffers (into-array (remove nil? bufs))))

(defn mul-bufs [& bufs]
  (SoundUtil/multiplyBuffers (into-array bufs)))

(defn process-node [n x node]
  (cond (and (map? node)
             (:seq node))
        (let [start-x (:start-x node)
              node (:data node)
              x1 (- x start-x)]
          (loop [[t sequenced-node & tail] node
                 bufs-to-sum []]
            (let [bufs-to-sum (if (< t (+ x1 n))
                                (conj bufs-to-sum (process-node n x sequenced-node))
                                bufs-to-sum)]
              (if (or (not (seq? tail))
                      (>= t (+ x1 n)))
                (if (seq bufs-to-sum)
                  (apply add-bufs bufs-to-sum)
                  nil)
                (recur tail bufs-to-sum)))))

        (map? node)
        (let [x1 (or (:x1 node) x)]
          (do (doseq [[bus v] (ordered-buses node)]
                (swap! buffers update bus (fn [b] (add-bufs (or b (float-array n))
                                                           (process-node n x1 v)))))
              (process-node n x1 (:<- node))))

        (number? node)
        (SoundUtil/filledBuf n node)

        (or (instance? clojure.lang.LazySeq node) (list? node) (vector? node))
        (cond (instance? UGen (first node))
              (.process (first node) n (into-array (map (partial process-node n x) (rest node))))

              (#{* +} (first node))
              (let [inputs (remove nil? (map (partial process-node n x) (rest node)))]
                (if (seq inputs)
                  (apply (cond (= + (first node)) add-bufs
                               (= * (first node)) mul-bufs)
                         inputs)
                  nil)))

        (instance? EnvPlayer node)
        (.process node n (make-array Float/TYPE 0 0))

        (keyword? node)
        (node @buffers)))

(def old-buf-calc-time (volatile! 0))

(defn build-buffer [sample-position]
  (let [now (System/nanoTime)
        diff (/ (- now @old-buf-calc-time) 1000000) ; in ms
        buf-len (/ buffer-size 48000 0.001)         ; in ms
        ]
    (when (< diff buf-len)
      (Thread/sleep (int (* 2 (- buf-len diff)))))
    (vreset! old-buf-calc-time now))

  (reset! buffers {})
  (let [{:keys [x graph]} @db/db
        new-graph (build-graph buffer-size x x graph)
        fa (process-node buffer-size x new-graph)]
    (swap! db/db assoc
           :master-buf fa
           :level (if fa
                    (/ (SoundUtil/maxFromBuf fa) 10)
                    0)
           :graph new-graph
           :x (+ x buffer-size))
    (when fa
      (->> (interleave fa fa)
           (map #(int (* 1000 %)))
           (mapcat (partial little-endian 2))
           byte-array))))

(defn play-loop [line buffer player is-playing]
  (when buffer
    (send player (fn [sample-position]
                   (.write ^SourceDataLine line buffer 0 (* 4 buffer-size))
                   (+ sample-position buffer-size))))
  (when is-playing
                                        ;(swap! db/db assoc :random (rand-int 10))
    (when-let [new-buffer (build-buffer @player)]
      (await player)
      (recur line new-buffer player (:playing @db/db)))))

(defn start-audio []
  (def thread (Thread. #(play-loop
                         (doto
                           ;(.getLine (SoundUtil/getLineByName "Soundflower (2ch)") (DataLine$Info. SourceDataLine audio-format))
                           (AudioSystem/getLine (DataLine$Info. SourceDataLine audio-format))
                           .open
                           .start)
                         nil player (:playing @db/db))))
  (.start thread))

(add-watch db/db :play-stop
           (fn [k db old-db new-db]
             (when (and (not (:playing old-db))
                      (:playing new-db))
               (start-audio))))



(comment
 
  (ui/launch db/db)

  (make-sampler "resources/909/tape1/bd01.wav")

  (def w (WavFile/openWavFile (java.io.File. "resources/909/tape1/bd01.wav")))
  (def da (double-array (* (.getNumChannels w) (.getNumFrames w))))
  (.readFrames w da (.getNumFrames w))

  (do (swap! db/db assoc :playing false)
      (Thread/sleep 1000)
      (swap! db/db assoc
             :playing true
             :x 0
             :graph (score/out 0)))
  )


(defn -main
  [& args]
  (start-audio))
