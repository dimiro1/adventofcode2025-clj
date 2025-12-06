(ns day6
  (:require [clojure.string :as str]
            [util]))

(comment
  (let [input [123 328 51 64 45 64 387 23 6 98 215 314 "*" "+" "*" "+"]]
    (println (take-nth 4 input))
    (println (take-nth 4 (drop 1 input))))
  :dimiro1)

(defn parse-element [x]
  (let [x' (str/trim x)]
    (case x'
      "*" x'
      "+" x'
      (parse-long x'))))

(comment
  (parse-element "123")
  (parse-element "+")
  (parse-element "*\n")
  :dimiro1)

(defn apply-operation
  "Input is [1 2 3 \"+\"]"
  [input]
  (let [reversed (reverse input)
        op (first (take 1 reversed))
        only-numbers (drop 1 reversed)]
    (case op
      "+" (apply + only-numbers)
      "*" (apply * only-numbers))))

(comment
  (take 1 ["+" 1 2 3])
  (apply-operation [6 45 123 "*"])
  :dimiro1)

(defn parse-input [input]
  (->> (str/split (str/replace input #"\n" " ") #" ")
   (remove empty?)
   (map parse-element)))

(comment
  (parse-input (util/read-input "inputs/day6-example.txt"))
  (parse-input (util/read-input "inputs/day6.txt"))
  :dimiro1)

(defn part-1 [input]
  (let [elements (parse-input input)
        operations-count (count (filter #(or (= % "*") (= % "+")) elements))]
    (reduce +
            (for [offset (range operations-count)]
              (apply-operation (take-nth operations-count (drop offset elements)))))))

(comment
  (part-1 (util/read-input "inputs/day6-example.txt"))
  (part-1 (util/read-input "inputs/day6.txt"))
  :dimiro1)
