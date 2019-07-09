(ns clj-sound.util)

(defmacro defn* [name & body]
  (let [name* (symbol (str name "*"))]
    `(do (defn ~name* ~@body)
         (def ~name #'~name*))))
