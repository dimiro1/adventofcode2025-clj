(ns day2
  (:require [clojure.string :as str]
            [util]))


(defn split-str [input parts]
  (let [split-n (/ (count input) parts)]
    (->> input
         (partition-all split-n)
         (map (partial apply str)))))

(comment
  (split-str "1188511885" 2)
  (split-str "1188511885" 10)
  (split-str "123123123" 9)
  :dimiro1)

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

(defn is-valid [id]
  (= 2 (-> (str id)
           (split-str 2)
           (as-> x (into #{} x))
           (count))))

(comment
  (is-valid 12)
  (is-valid 1188511885)
  :dimiro1)

(defn part-1 [input]
  (-> input
      (str/replace #"\n" "")
      (str/split #",")
      (->> (map parse-range)
           (map expand-range)
           (mapcat identity)
           (remove is-valid)
           (apply +))))

(comment
  (part-1 (util/read-input "inputs/day2-example.txt"))
  (part-1 (util/read-input "inputs/day2.txt"))
  :dimiro1)

(defn is-valid-part-2 [id]
  (let [s (str id)
        max-size (count s)]
    (loop [split-n 2]
      (if (> split-n max-size)
        true
        (let [parts-count (-> s
                              (split-str split-n)
                              (as-> x (into #{} x))
                              (count))]
          (if (= parts-count 1)
            false
            (recur (inc split-n))))))))

(comment
  (is-valid-part-2 123123123)
  (is-valid-part-2 1231231235)
  (is-valid-part-2 14)
  :dimiro1)

(defn part-2 [input]
  (->> (str/split (str/replace input #"\n" "") #",")
       (map parse-range)
       (mapcat expand-range)
       (remove is-valid-part-2)
       (reduce +)))

(comment
  (part-2 (util/read-input "inputs/day2-example.txt"))
  (part-2 (util/read-input "inputs/day2.txt"))
  :dimiro1)
