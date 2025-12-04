(ns util
  (:require [clojure.string :as str]))

(defn read-input [filename]
  (slurp filename))

(defmacro bench [expr]
  `(let [start# (System/nanoTime)
         result# ~expr
         elapsed# (/ (- (System/nanoTime) start#) 1e6)]
     {:result result# :ms elapsed#}))
