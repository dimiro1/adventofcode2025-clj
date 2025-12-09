(ns day9
  (:require [clojure.string :as str]
            [util]))

(defn parse-input [input]
  (->> (str/split-lines input)
       (mapv #(->> (str/split % #",")
                   (mapv parse-long)))))

(defn calc-area [[x1 y1] [x2 y2]]
  (* (inc (- x2 x1)) (inc (- y2 y1))))

(comment
  (calc-area [2 5] [9 7])
  (calc-area [7 1] [11 7])
  (calc-area [2 5] [11 1])
  :dimiro1)

(defn part-1 [input]
  (let [coords (parse-input input)
        combinations (for [x coords y coords] [x y])]
    (->> combinations
       (remove (fn [[pos1 pos2]] (= pos1 pos2)))
       (map (fn [[pos1 pos2]] (calc-area pos1 pos2)))
       (sort >)
       first)))

(comment
  (part-1 (util/read-input "inputs/day9-example.txt"))
  (part-1 (util/read-input "inputs/day9.txt"))
  :dimiro1)
