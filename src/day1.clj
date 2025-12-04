(ns day1
  (:require [clojure.string :as str]
            [util]))


(def rotation-pattern #"([LR])(\d+)")

(defn parse-rotation [s]
  (when-let [[_ direction number] (re-matches rotation-pattern s)]
    {:direction direction
     :number (parse-long number)}))

(defn rotate-left [start amount]
  (mod (- start amount) 100))

(defn rotate-right [start amount]
  (mod (+ start amount) 100))

(defn apply-rotation [rotations {:keys [direction number]}]
  (case direction
    "L" (rotate-left rotations number)
    "R" (rotate-right rotations number)))

(defn part-1 [input]
  (->> input
       str/split-lines
       (remove str/blank?)
       (map parse-rotation)
       (reduce (fn [{:keys [rotations zeros]} rotation]
                 (let [new-rotations (apply-rotation rotations rotation)]
                   {:rotations new-rotations
                    :zeros (if (zero? new-rotations)
                             (inc zeros)
                             zeros)}))
               {:rotations 50 :zeros 0})
       :zeros))

(comment
  (part-1 (util/read-input "inputs/day1.txt"))
  (part-1 (util/read-input "inputs/day1-example.txt"))
  :dimiro1)


(defn rotate-left-with-crossings [start amount]
  (loop [position start
         i 0
         crossings 0]
    (if (>= i amount)
      [position crossings]
      (let [new-position (mod (- position 1) 100)
            new-crossings (if (zero? new-position)
                           (inc crossings)
                           crossings)]
        (recur new-position (inc i) new-crossings)))))

(defn rotate-right-with-crossings [start amount]
  (loop [position start
         i 0
         crossings 0]
    (if (>= i amount)
      [position crossings]
      (let [new-position (mod (+ position 1) 100)
            new-crossings (if (zero? new-position)
                           (inc crossings)
                           crossings)]
        (recur new-position (inc i) new-crossings)))))

(defn apply-rotation-with-crossings [rotations {:keys [direction number]}]
  (case direction
    "L" (rotate-left-with-crossings rotations number)
    "R" (rotate-right-with-crossings rotations number)))

(defn part-2 [input]
  (->> input
       str/split-lines
       (remove str/blank?)
       (map parse-rotation)
       (reduce (fn [{:keys [rotations zeros]} rotation]
                 (let [[new-rotations crossings] (apply-rotation-with-crossings rotations rotation)]
                   {:rotations new-rotations
                    :zeros (+ zeros crossings)}))
               {:rotations 50 :zeros 0})
       :zeros))

(comment
  (part-2 (util/read-input "inputs/day1.txt"))
  (part-2 (util/read-input "inputs/day1-example.txt"))
  :dimiro1)
