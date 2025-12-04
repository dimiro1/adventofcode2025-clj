(ns core
  (:require [util]
            [day1]
            [day2]))

(defn day1-part-1 [] (day1/part-1 (util/read-input "inputs/day1.txt")))
(defn day1-part-1-example [] (day1/part-1 (util/read-input "inputs/day1-example.txt")))

(defn day2-part-1 [] (day2/part-1 (util/read-input "inputs/day2.txt")))
(defn day2-part-1-example [] (day2/part-1 (util/read-input "inputs/day2-example.txt")))
(defn day2-part-2 [] (day2/part-2 (util/read-input "inputs/day2.txt")))
(defn day2-part-2-example [] (day2/part-2 (util/read-input "inputs/day2-example.txt")))
