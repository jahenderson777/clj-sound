(ns clj-sound.util)

(defmacro defn* [name & body]
  (let [name* (symbol (str name "*"))]
    `(do (defn ~name* ~@body)
         (def ~name #'~name*))))

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

(defn queue
  ([] (clojure.lang.PersistentQueue/EMPTY))
  ([coll] (reduce conj clojure.lang.PersistentQueue/EMPTY coll)))

(defn unsigned-byte [x]
  (byte (if (> x 127) (- x 256) x)))

(defn little-endian [size x]
  (map #(-> (bit-shift-right x (* 8 %))
            (bit-and 255)
            unsigned-byte)
       (range size)))

(defn clamp [x min max]
  (if (< x min)
    min
    (if (> x max)
      max
      x)))