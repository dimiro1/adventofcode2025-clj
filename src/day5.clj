(ns day5
  (:require [clojure.string :as str]
            [util]))

(defn parse-range [r]
  (let [[start end] (str/split r #"-")]
    {:start (parse-long start) :end (parse-long end)}))

(comment
  (parse-range "11-22")
  :dimiro1)

(defn expand-range [r]
  (range (:start r) (inc (:end r))))

(comment
  (expand-range {:start 11 :end 22})
  :dimiro1)

(defn is-in-range? [range id]
  (and (>= id (:start range))
       (<= id (:end range))))

(comment
  (is-in-range? {:start 11 :end 22} 20)
  (is-in-range? {:start 11 :end 22} 30)
  :dimiro1)

(defn parse-input [input]
  (let [[ranges ids]
        (->> (str/split-lines input)
             (partition-by #(= % ""))
             (remove #(= (first %) "")))]
    {:ranges (map parse-range ranges)
     :ids (map parse-long ids)}))

(comment
  (parse-input (util/read-input "inputs/day5-example.txt"))
  :dimiro1)

(defn part-1 [input]
  (let [inventory (parse-input input)]
    (->> (for [ingredient (:ids inventory)
               range      (:ranges inventory)]
           (when (is-in-range? range ingredient)
             ingredient))
         (into #{})
         (filter some?)
         count)))

(comment
  (part-1 (util/read-input "inputs/day5-example.txt"))
  (part-1 (util/read-input "inputs/day5.txt"))
  :dimiro1)
