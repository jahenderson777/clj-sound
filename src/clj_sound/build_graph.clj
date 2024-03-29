(ns clj-sound.build-graph
  (:require [clj-sound.rate :refer [samples-per-tick]]
            [clojure.test :refer [deftest is]])
  (:import UGen CubicSplineFast EnvPlayer Sampler))

(declare build-graph)

(defn actual-fn [fn-v]
  (let [m (meta fn-v)]
    (var-get (ns-resolve (:ns m) (:name m)))))

(defn execute [x {:keys [fn start-x]}]
  (let [ret (apply (first fn) (long (Math/ceil (double (/ x samples-per-tick)))) (rest fn))]
    (-> (if (map? ret)
          ret
          {:<- ret})
        (assoc :actual-fn (actual-fn (first fn)) :fn fn :start-x start-x))))

(defn construct [class & args]
  (clojure.lang.Reflector/invokeConstructor class (into-array Object args)))

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

(defn build-graph-seq [{:keys [n x-buf x node] :as opts}]
  (let [updated
        (-> node
            (update :data
                    (fn [s]
                      (let [start-x (:start-x node)
                            x1 (- x-buf start-x)]
                        (loop [[t* sequenced-node & tail] s
                               new-sequence '()]
                          (if (nil? t*)
                            new-sequence
                            (let [t (long (* t* samples-per-tick))
                                  new-sequence2
                                  (as-> sequenced-node $
                                    (cond (< t (+ x1 n))
                                          (build-graph (assoc opts
                                                              :x (+ start-x t)
                                                              :node $)) ;; TODO can't understand why (+ start-x t) doesn't cause a problem,
                                          :else $)
                                    (concat new-sequence (when $ [t* $])))]
                              (if (or (not (seq? tail))
                                      (>= t (+ x1 n)))
                                (concat new-sequence2 tail)
                                (recur tail new-sequence2)))))))))]
    (when (seq (:data updated)) updated)))

(defn build-graph-map [{:keys [n x-buf x node] :as opts}]
  (cond (:seq node)
        (build-graph-seq opts)

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
                (def new-node (build-graph (assoc opts
                                                  :x (:start-x node)
                                                  :node (execute (:start-x node) {:fn old-fn :start-x (:start-x node)}))))
                      ;(println "here" (:start-x node)  new-node (execute (:start-x node) {:fn old-fn :start-x (:start-x node)}))
                new-node)

            (let [processed-buffers (map #(build-graph (assoc opts :node %)) (vals buffers))]
              (when (some identity processed-buffers)
                (merge (zipmap (keys buffers) processed-buffers)
                       others)))))))


(defn build-graph* [{:keys [x-buf x node] :as opts}]
  (let [g (first node)
        build-inputs (fn [] (mapv #(build-graph (assoc opts :node %)) (rest node)))]
    (cond (int? g)
          (build-graph (assoc opts :node
                              {:seq :polyphonic
                               :start-x x
                               :data (if (and (not (instance? clojure.lang.LazySeq node))
                                              (odd? (count node)))
                                       (repeating node (long (* (last node) (int (/ (/ (- x-buf x) samples-per-tick) (last node))))) 0)
                                       (remove-past-from-sequence node (- x-buf x)))}))
          (instance? CubicSplineFast g)
          (when (>= x x-buf)
            [(Sampler. (- x-buf x)
                       g
                       (nth node 2 1.0)
                       (nth node 3 0))
             (if-let [speed (nth node 1 nil)]
               (build-graph (assoc opts :node speed))
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
          (build-graph (assoc opts :node (execute x {:fn node :start-x x})))

          :else node)))

(defn build-graph
  "`n` size of buffer
   `x-buf` the sample position of the start of the buffer.
   `x` the sample position that this node starts at, because a node might be sequenced to start
   partway through a buffer. It only really comes into play when constructing UGens."
  [{:keys [node] :as opts}]
  (cond (or (instance? clojure.lang.LazySeq node) (list? node) (vector? node))
        (build-graph* opts)

        (map? node)
        (build-graph-map opts)

        (instance? EnvPlayer node)
        (when-not (.-ended node) node)

        :else node))
