(ns clj-sound.core
  (:require [fastmath.core :as m]
            [clj-sound.ui :as ui]
            [clj-sound.score :as score]
            [clj-sound.db :as db]
            [clojure.data.int-map :as i]
            [clojure.data.avl :as avl]
            [clojure.string :as str])
  (:import (javax.sound.sampled Mixer Mixer$Info AudioSystem DataLine$Info SourceDataLine AudioFormat AudioFormat$Encoding)
           SimpleOsc WavFile UGen SoundUtil CubicSplineFast Player EnvPlayer Sampler MidiHandler)
  (:gen-class))

(set! *unchecked-math* true)

(def sample-rate 48000)

(def audio-format
  (AudioFormat. AudioFormat$Encoding/PCM_SIGNED
                sample-rate ; sample rate
                16    ; bits per sample
                2     ; channels
                4     ; frame size 2*16bits [bytes]
                48000 ; frame rate
                false ; little endian
                ))

(def buffer-size 800)
(def player (agent 0))
(def bpm 140)
(def samples-per-tick (/ sample-rate (/ bpm 60) 256))

(def buffers (atom {}))
(def var-map (atom {}))

(def cc (atom {}))

(def perf (atom {}))
(defn perf-watch [k f]
  (let [start-time (System/nanoTime)
        ret (f)
        end-time (System/nanoTime)
        exec-time (long (/ (- end-time start-time) 1000))]
    (swap! perf update-in [k :recent-runs] (fn [v] (take 20 (conj (or v []) exec-time))))
    (swap! perf update-in [k :max-time] (fn [t] (if (> exec-time (or t 0))
                                                 exec-time
                                                 t)))
    ret))

(defn ^java.util.function.Function as-function [f]
  (reify java.util.function.Function
    (apply [this arg] (f arg))))

(defn midi-receive [ba]
  (let [[msg ch val] (seq ba)]
    (when (= msg -80)
      (swap! cc assoc ch val))))

(def midi-in (MidiHandler. "LPD8" (as-function midi-receive)))

(defn unsigned-byte [x]
  (byte (if (> x 127) (- x 256) x)))

(defn little-endian [size x]
  (map #(-> (bit-shift-right x (* 8 %))
            (bit-and 255)
            unsigned-byte)
       (range size)))

#_(defn latest-fn [f]
  (let [fn-s (.toString f)
        [ns fn-name] (str/split fn-s #"[$]")
        symbolize #(symbol (str/replace % #"_" "-"))]
    (ns-resolve (symbolize ns) (symbolize (first (str/split fn-name #"@"))))))

(defn actual-fn [fn-v]
  (let [m (meta fn-v)]
    (var-get (ns-resolve (:ns m) (:name m)))))

(defn execute [x {:keys [fn start-x]}]
  (let [ret (apply (first fn) (long (Math/ceil (double (/ x samples-per-tick)))) (rest fn))]
    (-> (if (map? ret)
          ret
          {:<- ret})
        (assoc :actual-fn (actual-fn (first fn)) :fn fn :start-x start-x))))

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
                 (loop [[t* sequenced-node & tail] s
                        new-sequence '()]
                   (if (nil? t*)
                     new-sequence
                     (let [t (long (* t* samples-per-tick))
                           ;_ (println sequenced-node)
                           new-sequence2
                           (as-> sequenced-node $
                             (cond (< t (+ x1 n))
                                   (build-graph n x-buf (+ start-x t) $) ;; TODO can't understand why (+ start-x t) doesn't cause a problem,

                                   :else
                                   $)
                             (concat new-sequence (when $ [t* $])))]
                       (if (or (not (seq? tail))
                               (>= t (+ x1 n)))
                         (concat new-sequence2 tail)
                         (recur tail new-sequence2)))))))))]
    (when (seq (:data updated))
      updated)))

(defn repeating [s offset n]
  (lazy-seq
   (concat (loop [[t node & tail] s
                  new-sequence []]
             (let [new-sequence2 (conj new-sequence (+ t n offset) node)]
               (if (= 1 (count tail))
                 new-sequence2
                 (recur tail new-sequence2))))
           (repeating s offset (+ n (last s))))))

(defn remove-past-from-sequence [s x]
  (loop [[t* sequenced-node & tail] s
         new-sequence []]
    (let [t (long (* t* samples-per-tick))
          new-sequence2
          (if (< t x)
            new-sequence
            (conj new-sequence t* sequenced-node))]
      (if (or (not (seq? tail))
              (>= t x))
        (concat new-sequence2 tail)
        (recur tail new-sequence2)))))

(defn build-graph
  "x-buf is the sample position of the start of the buffer.
   x is the sample position that this node starts at, because a node might be sequenced to start
   partway through a buffer. It only really comes into play when constructing UGens."
  [n x-buf x node]
  (cond (or (instance? clojure.lang.LazySeq node) (list? node) (vector? node))
        (let [g (first node)
              build-inputs (fn [] (mapv #(build-graph n x-buf x %) (rest node)))]
          (cond (int? g)
                (build-graph n x-buf x {:seq :polyphonic
                                        :start-x x
                                        :data (if (and (not (instance? clojure.lang.LazySeq node))
                                                       (odd? (count node)))
                                                (repeating node (long (* (last node) (int (/ (/ (- x-buf x) samples-per-tick) (last node))))) 0)
                                                (remove-past-from-sequence node (- x-buf x)))})

                (instance? CubicSplineFast g)
                (when (>= x x-buf)
                  [(Sampler. (- x-buf x)
                             g
                             (nth node 2 1.0)
                             (nth node 3 0))
                   (if-let [speed (nth node 1 nil)]
                     (build-graph n x-buf x speed)
                     1.0)])

                (class? g)
                (when (>= x x-buf)
                  (concat [(construct g (- x-buf x))] (build-inputs)))

                (or (#{* +} g)
                    (instance? UGen g))
                (when-not (and (instance? Sampler g)
                               (.-ended g))
                  (let [inputs (build-inputs)]
                    (when-not (or (and (not (= + g)) (some nil? inputs))
                                  (and (= + g) (every? nil? inputs)))
                      (concat [g] inputs))))

                (var? g)
                (build-graph n x-buf x (execute x {:fn node :start-x x}))

                :else node))

        (map? node)
        (cond (:seq node)
              (build-graph-seq n x-buf x node)

              (int? (first (first node)))
              (when (>= x x-buf)
                (EnvPlayer. (- x-buf x)
                            (double-array (map (partial * samples-per-tick) (keys node)))
                            (double-array (vals node))))

              :else
              (let [buffers (dissoc node :fn :start-x :actual-fn)
                    others (select-keys node [:fn :start-x :actual-fn])
                    actual (:actual-fn others)
                    old-fn (:fn others)
                    new-actual (actual-fn (first old-fn))]
                (if (not= actual new-actual)
                  (do (println "recompile detected")
                      (def old-node node)
                      (def new-node (build-graph n x-buf (:start-x node) (execute (:start-x node) {:fn old-fn :start-x (:start-x node)})))
                      ;(println "here" (:start-x node)  new-node (execute (:start-x node) {:fn old-fn :start-x (:start-x node)}))
                      new-node)

                  (let [processed-buffers (map (partial build-graph n x-buf x) (vals buffers))]
                    (when (some identity processed-buffers)
                      (merge (zipmap (keys buffers) processed-buffers)
                             others))))))

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
          (loop [[t* sequenced-node & tail] node
                 bufs-to-sum []]
            (let [t (long (* t* samples-per-tick))
                  bufs-to-sum (if (< t (+ x1 n))
                                (conj bufs-to-sum (process-node n x sequenced-node))
                                bufs-to-sum)]
              (if (or (not (seq? tail))
                      (>= t (+ x1 n)))
                (if (seq bufs-to-sum)
                  (apply add-bufs bufs-to-sum)
                  (float-array n))
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
              (perf-watch :dot-process #(.process (first node) n (into-array (map (partial process-node n x) (rest node)))))

              (#{* +} (first node))
              (let [inputs (remove nil? (map (partial process-node n x) (rest node)))]
                (if (seq inputs)
                  (apply (cond (= + (first node)) add-bufs
                               (= * (first node)) mul-bufs)
                         inputs)
                  nil))

              (= :c (first node))
              (let [[ch min max default] (rest node)
                    m (/ (- max min) 127)
                    v (or (get @cc ch)
                          default
                          0)]
                (SoundUtil/filledBuf n (+ min (* v m))))

              :else
              (println "shouldn't get here" node))

        (instance? EnvPlayer node)
        (.process node n (make-array Float/TYPE 0 0))

        (keyword? node)
        (node @buffers)))

(def old-buf-calc-time (volatile! 0))

(defn build-buffer [sample-position]
  (let [now (System/nanoTime)
        diff (/ (- now @old-buf-calc-time) 1000000) ; in ms
        buf-len (/ buffer-size sample-rate 0.001)         ; in ms
        ]
    (when (< diff buf-len)
      (Thread/sleep (int (* 1.7 (- buf-len diff))))
      )
    (vreset! old-buf-calc-time now))

  (reset! buffers {})
  (let [{:keys [x graph]} @db/db
        new-graph (perf-watch :build-graph #(build-graph buffer-size x x graph))
        ;_ (swap! db/db assoc :graph new-graph)
        fa (perf-watch :process-node #(process-node buffer-size x new-graph))]
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
  (do (swap! db/db assoc :playing false)
      (Thread/sleep 1000)
      (swap! db/db assoc
             :playing true
             :x 0
             :graph [score/out]))


  (ui/launch db/db)

  (make-sampler "resources/909/tape1/bd01.wav")

  (def w (WavFile/openWavFile (java.io.File. "resources/909/tape1/bd01.wav")))
  (def da (double-array (* (.getNumChannels w) (.getNumFrames w))))
  (.readFrames w da (.getNumFrames w))

  
  )


(defn -main
  [& args]
  (start-audio))
