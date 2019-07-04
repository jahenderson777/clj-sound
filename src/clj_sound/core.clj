(ns clj-sound.core
  (:require [fastmath.core :as m]
            [clj-sound.ui :as ui]
            [clj-sound.score :as score]
            [clojure.data.int-map :as i]
            [clojure.data.avl :as avl])
  (:import (javax.sound.sampled Mixer Mixer$Info AudioSystem DataLine$Info SourceDataLine AudioFormat AudioFormat$Encoding)
           SimpleOsc WavFile UGen SoundUtil CubicSplineFast Player EnvPlayer)
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

(def buffer-size 1024)
(def player (agent 0))

(def db (atom {:playing false
               :level 0.0
               :master-vol 1.0}))

(def buffers (atom {}))

(def x (atom 0))
(def graph (atom (score/out 0)))

(defn unsigned-byte [x]
  (byte (if (> x 127) (- x 256) x)))

(defn little-endian [size x]
  (map #(-> (bit-shift-right x (* 8 %))
            (bit-and 255)
            unsigned-byte)
       (range size)))

(defn execute [x {:keys [fn start-x] :as m}]
  (let [ret (apply (ns-resolve 'clj-sound.score (first fn)) x (rest fn))]
    (-> (if (map? ret)
          ret
          {:<- ret})
        (assoc :fn fn :start-x start-x))))

(defn ordered-buses [m]
  (into (sorted-map) (dissoc m :<- :fn :start-x)))

(defn construct [class & args]
  (clojure.lang.Reflector/invokeConstructor class (into-array Object args)))

(defn build-graph [n x-buf x node]
  (cond (or (instance? clojure.lang.LazySeq node) (list? node) (vector? node))
        (cond (int? (first node))
              (build-graph n x-buf x {:seq :polyphonic
                                      :start-x x
                                      :data node})

              (class? (first node))
              (concat [(construct (first node) (- x-buf x))] (mapv #(build-graph n x-buf x %)
                                                                   (rest node)))

              (or (fn? (first node))
                  (instance? UGen (first node)))
              (let [inputs (mapv #(build-graph n x-buf x %)
                                 (rest node))]
                (if (some nil? inputs)
                  nil
                  (concat [(first node)] inputs)))

              (symbol? (first node))
              (build-graph n x-buf x (execute x {:fn node :start-x x}))

              :else
              node)

        (map? node)
        (cond (:seq node)
              (let [updated
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
                                            new-sequence2 (if sequenced-node2
                                                            (concat new-sequence [t sequenced-node2])
                                                            new-sequence)] ; don't concat if nil (ended)
                                        (if (or (not (seq? tail))
                                                (>= t (+ x1 n)))
                                          (concat new-sequence2 tail)
                                          (recur tail new-sequence2))))))))]
                (if (seq (:data updated))
                  updated
                  nil))

              (int? (first (first node)))
              (EnvPlayer. (- x-buf x)
                          (double-array (keys node))
                          (double-array (vals node)))

              :else
              (let [buffers (dissoc node :fn :start-x)
                    others (select-keys node [:fn :start-x])
                    processed-buffers (map (partial build-graph n x-buf x) (vals buffers))]
                (if (some identity processed-buffers)
                  (merge (zipmap (keys buffers) processed-buffers)
                         others)
                  nil)))

        (instance? EnvPlayer node)
        (if (.-ended node)
          nil
          node)

        :else
        node))

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
                (apply add-bufs bufs-to-sum)
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

              (= (first node) *)
              (let [inputs (remove nil? (map (partial process-node n x) (rest node)))]
                (if (seq inputs)
                  (apply mul-bufs inputs)
                  nil)))

        (instance? EnvPlayer node)
        (.process node n (make-array Float/TYPE 0 0))

        (keyword? node)
        (node @buffers)))

(defn build-buffer [sample-position]
  (reset! buffers {})
  (swap! graph (partial build-graph buffer-size @x @x))
  (let [fa (process-node buffer-size @x @graph)]
    (swap! x (partial + buffer-size))
    (swap! db assoc :level (/ (SoundUtil/maxFromBuf fa) 10))
    (->> (interleave fa fa)
         (map #(int (* 1000 %)))
         (mapcat (partial little-endian 2))
         byte-array)))

(defn play-loop [line buffer player is-playing]
  (when buffer
    (send player (fn [sample-position]
                   (.write ^SourceDataLine line buffer 0 (* 4 buffer-size))
                   (+ sample-position buffer-size))))
  (when is-playing
    (let [new-buffer (build-buffer @player)]
      (await player)
      (recur line new-buffer player (:playing @db)))))

(defn start-audio []
  (def thread (Thread. #(play-loop
                         (doto
                           ;(.getLine (SoundUtil/getLineByName "Soundflower (2ch)") (DataLine$Info. SourceDataLine audio-format))
                           (AudioSystem/getLine (DataLine$Info. SourceDataLine audio-format))
                           .open
                           .start)
                         nil player (:playing @db))))
  (.start thread))

(add-watch db :play-stop
           (fn [k db old-db new-db]
             (when (and (not (:playing old-db))
                      (:playing new-db))
               (start-audio))))

(comment
 
  (ui/launch db)

  (def w (WavFile/openWavFile (java.io.File. "resources/Electro-Tom.wav")))
  (def da (double-array (* (.getNumChannels w) (.getNumFrames w))))
  (.readFrames w da (.getNumFrames w))

  (do (swap! db assoc :playing false)
      (Thread/sleep 1000)
      (reset! buffers {})
      (reset! x 0)
      (reset! graph (score/out 0))
      (swap! db assoc :playing true))
  )


(defn -main
  [& args]
  (start-audio))
