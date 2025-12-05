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


(defn rolls-to-be-picked [shelf]
  (filter some? (for [row (range (count shelf))
                      col (range (count (first shelf)))]
                  (if (= "@" (get-in shelf [row col]))
                    (if (can-be-picked? (get-siblings shelf [row col]))
                      [row col])))))

(defn part-1 [input]
  (let [shelf (parse-input input)]
    (count (rolls-to-be-picked shelf))))

(comment
  (part-1 (util/read-input "inputs/day4-example.txt"))
  (part-1 (util/read-input "inputs/day4.txt"))
  :dimiro1)

(defn remove-roll [shelf pos]
  (assoc-in shelf pos "."))

(defn remove-rolls [shelf positions]
  (loop [new-shelf shelf
         pos (first positions)
         remaining (rest positions)]
    (if (empty? remaining)
      (remove-roll new-shelf pos)
      (recur (remove-roll new-shelf pos) (first remaining) (rest remaining)))))

(comment
  (let [shelf (parse-input (util/read-input "inputs/day4-example.txt"))]
    (remove-rolls shelf [[1 0] [0 2]]))
  :dimiro1)


(comment
  (let [shelf (parse-input (util/read-input "inputs/day4-example.txt"))]
    (rolls-to-be-picked shelf))
  :dimiro1)

(defn part-2 [input]
  (loop [current-shelf (parse-input input)
         pickable-rolls (rolls-to-be-picked current-shelf)
         removed-count 0]
    (if (empty? pickable-rolls)
      removed-count
      (let [updated-shelf (remove-rolls current-shelf pickable-rolls)]
        (recur updated-shelf
               (rolls-to-be-picked updated-shelf)
               (+ removed-count (count pickable-rolls)))))))

(comment
  (part-2 (util/read-input "inputs/day4-example.txt"))
  (part-2 (util/read-input "inputs/day4.txt"))
  :dimiro1)
