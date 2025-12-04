(ns day3
  (:require [clojure.string :as str]
            [util]))


(defn largest-joltage [bank]
  (let [batteries (->> (str/split bank #"")
                       (map (partial apply str))
                       (map parse-long))
        left (apply max (drop-last 1 batteries))
        right (apply max (drop 1 (drop-while #(not= % left) batteries)))]
    (parse-long (str left right))))

(comment
  (largest-joltage "987654321111111")
  (largest-joltage "811111111111119")
  (largest-joltage "234234234234278")
  (largest-joltage "818181911112111")
  :dimiro1)

(defn part-1 [input]
    (->> input
       str/split-lines
       (remove str/blank?)
       (map largest-joltage)
       (reduce +)))

(comment
  (part-1 (util/read-input "inputs/day3-example.txt"))
  (part-1 (util/read-input "inputs/day3.txt"))
  :dimiro1)


(defn largest-12-joltage [bank]
  (let [batteries (->> (str/split bank #"")
                       (mapv parse-long))
        total (count batteries)
        needed 12]
    (loop [pos 0
           selected []]
      (if (= (count selected) needed)
        (parse-long (apply str selected))
        (let [still-need (- needed (count selected))
              remaining (- total pos)
              lookahead (- remaining still-need)
              window-end (+ pos lookahead 1)
              window (subvec batteries pos window-end)
              max-digit (apply max window)
              max-pos (+ pos (.indexOf window max-digit))]
          (recur (inc max-pos)
                 (conj selected max-digit)))))))


(comment
  (largest-12-joltage "987654321111111")
  (largest-12-joltage "811111111111119")
  (largest-12-joltage "234234234234278")
  (largest-12-joltage "818181911112111")
  :dimiro1)

(defn part-2 [input]
  (->> input
       str/split-lines
       (remove str/blank?)
       (map largest-12-joltage)
       (reduce +)))

(comment
  (part-2 (util/read-input "inputs/day3-example.txt"))
  (part-2 (util/read-input "inputs/day3.txt"))
  :dimiro1)
