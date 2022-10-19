(ns clj-sound.midi
  (:require [overtone.osc :as osc]
            [clj-sound.util :as util]))

(def cc (atom {}))

(defn ^java.util.function.Function as-function [f]
  (reify java.util.function.Function
    (apply [this arg] (f arg))))

(defn midi-receive [ba]
  (let [[msg ch val] (seq ba)]
    (when (= msg -80)
      (swap! cc assoc ch val))))

(def midi-in (MidiHandler. "LPD8" (as-function midi-receive)))

(def osc-server (osc/osc-server 9800))

(osc/osc-handle osc-server "/mrmr/accelerometerZ/0/jedermann" 
                (fn [{[x] :args}] (swap! cc assoc 100 (util/clamp (/ x 4) 0 255))))

(comment

  (def client (osc/osc-client "localhost" 9800))

  (osc/osc-send client "/foo/bar/baz 1 2 three")

  (osc/osc-listen server (fn [msg] (println "Listener: " msg)) :debug)

  (osc/osc-rm-listener server :debug)

  (osc/osc-handle server "/mrmr/accelerometerZ/0/jedermann" (fn [msg] (println "Handler for /foo/bar: " msg)))

  (osc/osc-rm-handler server "/mrmr/accelerometerZ/0/jedermann")

  (osc/osc-rm-all-handlers osc-server)

  )