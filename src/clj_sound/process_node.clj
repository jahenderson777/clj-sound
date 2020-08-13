(ns clj-sound.process-node
  (:require [clj-sound.rate :refer [samples-per-tick]]
            [clj-sound.midi :refer [cc]]
            [clj-sound.util :as util])
  (:import UGen SoundUtil EnvPlayer))

(def buffers (atom {}))

(defn ordered-buses [m]
  (into (sorted-map) (dissoc m :<- :fn :start-x)))

(defn add-bufs [& bufs]
  (SoundUtil/sumBuffers (into-array (remove nil? bufs))))

(defn mul-bufs [& bufs]
  (SoundUtil/multiplyBuffers (into-array bufs)))

(declare process-node)

(defn process-node-seq [n x node]
  (cond (instance? UGen (first node))
        (util/perf-watch :dot-process #(.process (first node) n (into-array (map (partial process-node n x) (rest node)))))

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
        (println "shouldn't get here" node)))

(defn process-node-map [n x node]
  (if (:seq node)
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
    
    (let [x1 (or (:x1 node) x)]
      (doseq [[bus v] (ordered-buses node)]
        (swap! buffers update bus (fn [b] (add-bufs (or b (float-array n))
                                                    (process-node n x1 v)))))
      (process-node n x1 (:<- node)))))

(defn process-node [n x node]
  ;(println "process-node = " n x node)
  (cond (map? node)
        (process-node-map n x node)

        (number? node)
        (SoundUtil/filledBuf n node)

        (or (instance? clojure.lang.LazySeq node) (list? node) (vector? node))
        (process-node-seq n x node)

        (instance? EnvPlayer node)
        (.process node n (make-array Float/TYPE 0 0))

        (keyword? node)
        (node @buffers)))