(ns day11
  (:require [clojure.string :as str]
            [util]))

(defn parse-line [line]
  " \"ccc: ddd eee fff\" -> {:ccc #{:ddd :eee :fff}} "
  (let [[source conns] (str/split line #":")]
    {(keyword source)
     (into #{} (->> (str/split (str/trim conns) #" ")
                           (map keyword)))}))

(comment
  (parse-line "ccc: ddd eee fff")
  :dimiro1)

(defn parse-input [input]
  "Parses the input in the form \"ccc: ddd eee fff\""
  (->> (str/split-lines input)
       (map parse-line)))

(defn build-graph [parsed-maps]
  "Merges the individual lines parsed from the parse-input function"
  (apply merge parsed-maps))

(comment
  (let [parsed (parse-input (util/read-input "inputs/day11-example.txt"))]
    (build-graph parsed))
  :dimiro1)

(defn find-all-paths [graph start end]
  "Returns all paths from start to end as a vector of paths"
  (letfn [(depth-first-search [current path visited]
            (if (= current end)
              [path]
              (->> (get graph current #{})
                   (remove visited)
                   (mapcat #(depth-first-search % (conj path %) (conj visited %))))))]
    (vec (depth-first-search start [start] #{start}))))

(defn part-1 [input]
  (let [parsed (parse-input input)
        graph (build-graph parsed)]
    (count (find-all-paths graph :you :out))))

(comment
  (part-1 (util/read-input "inputs/day11-example.txt"))
  (part-1 (util/read-input "inputs/day11.txt"))
  :dimiro1)
