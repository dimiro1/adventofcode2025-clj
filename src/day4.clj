(ns day4
  (:require [clojure.string :as str]
            [util]))

(defn get-sibling [shelf [row col] direction]
  (let [[dest-row dest-col]
        (case direction
          :north     [(dec row) col]
          :south     [(inc row) col]
          :east      [row (inc col)]
          :west      [row (dec col)]
          :northeast [(dec row) (inc col)]
          :northwest [(dec row) (dec col)]
          :southeast [(inc row) (inc col)]
          :southwest [(inc row) (dec col)])]
    (get-in shelf [dest-row dest-col] ".")))

(defn get-siblings [shelf pos]
  (loop [directions [:north :south :east :west
                     :northeast :northwest :southeast :southwest]
         siblings []]
    (if (empty? directions)
      siblings
      (recur (rest directions)
             (conj siblings (get-sibling shelf pos (first directions)))))))

(defn can-be-picked? [siblings]
  (< (->> siblings
          (filter #(= % "@"))
          (count)) 4))

(defn parse-input [s]
  (mapv #(mapv str %) (str/split-lines s)))

(comment
  (let [shelf (parse-input (util/read-input "inputs/day4-example.txt"))]
    (can-be-picked? (get-siblings shelf [1 1])))
  :dimiro1)

(defn part-1 [input]
  (let [shelf (parse-input input)]
    (count (filter true?
                   (for [row (range (count shelf))
                         col (range (count (first shelf)))]
                     (if (= "@" (get-in shelf [row col]))
                       (->> (get-siblings shelf [row col])
                            (can-be-picked?))))))))

(comment
  (part-1 (util/read-input "inputs/day4-example.txt"))
  (part-1 (util/read-input "inputs/day4.txt"))
  :dimiro1)
