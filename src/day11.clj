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
  "Returns all paths from start to end as a vector of paths.
This a naive approach, it only works in simple cases."
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

(defn count-paths-containing [graph start end node-a node-b]
  "Counts paths from start to end containing both node-a and node-b.

I found this article: https://www.geeksforgeeks.org/dsa/number-of-paths-from-source-to-destination-in-a-directed-acyclic-graph/, that is basically the idea. "
  (let [memo (atom {})]
    (letfn [(dp [node has-a has-b]
              (let [has-a (or has-a (= node node-a))
                    has-b (or has-b (= node node-b))
                    key [node has-a has-b]]
                (if (= node end)
                  (if (and has-a has-b) 1 0)
                  (if-let [cached (get @memo key)]
                    cached
                    (let [result (->> (get graph node #{})
                                      (map #(dp % has-a has-b))
                                      (reduce + 0))]
                      (swap! memo assoc key result)
                      result)))))]
      (dp start false false))))

(defn part-2 [input]
  (let [parsed (parse-input input)
        graph (build-graph parsed)]
    (count-paths-containing graph :svr :out :dac :fft)))

(comment
  (part-2 (util/read-input "inputs/day11-example2.txt"))
  (part-2 (util/read-input "inputs/day11.txt"))
  :dimiro1)
