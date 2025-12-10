(ns day10
  (:require [clojure.string :as str]
            [util]))

(defn toggle
  "Toggle a single light"
  [index lights]
  (update lights index #(if (zero? %) 1 0)))

(defn toggle-lights [indexes lights]
  "Toggle a vector of lights"
  (reduce (fn [lights index] (toggle index lights)) lights indexes))

(comment
  (toggle-lights [0 0] [0])
  :dimiro1)

(defn format-lights
  "Format the lights accordingly to the problem statement."
  [lights]
  (mapv #(if (zero? %) "." "#") lights))

(comment
  (println (format-lights (toggle 0 [0 0 0 0 0])))
  (println (format-lights (toggle-lights [1 2] [0 0 0 0 0])))
  :dimiro1)

(defn combinations [coll k]
  "This was AI, the alternative would be using https://clojure.github.io/math.combinatorics/"
  (cond
    (zero? k) [[]]
    (empty? coll) []
    :else (concat
           (map #(cons (first coll) %)
                (combinations (rest coll) (dec k)))
           (combinations (rest coll) k))))

(comment
  (combinations [[3] [1 3] [2] [2 3] [0 2] [0 1]] 3)
  :dimiro1)

(defn find-solution
  "Find a combination of switches that transforms initial-lights into target.
 Example: target [0 1 1 0], initial-lights [0 0 0 0], switch-combinations [[[1 3] [2 3]]]"
  [target initial-lights switches-combinations]
  (some (fn [candidate]
          ;; candidate is in the format [[3] [1 3]]
          (let [indexes-to-toggle (flatten candidate)]
            (when (= target (toggle-lights indexes-to-toggle initial-lights))
              ;; found a combination that solves the problem
              candidate)))
        switches-combinations))

(comment
  ;; [. . . .] -> [1 3] -> [. 1 . 1] -> [2 3] -> [. 1 1 .]
  (let [switches [[3] [1 3] [2] [2 3] [0 2] [0 1]]
        comb (combinations switches 2)]
    (find-solution [0 1 1 0] [0 0 0 0] comb))
  :dimiro1)

(defn find-shortest-solution
  "Tries combinations of increasing size until a solution is found."
  [target switches]
  (let [initial (mapv (fn [_] 0) target)]
    (loop [combinations-len 1
           result           nil
           comb             (combinations switches combinations-len)]
      (cond
        ;; Found a solution
        (some? result)                        result
        ;; exhausted candidates
        (= combinations-len (count switches)) nil
        ;; keep trying
        :else                                 (recur (inc combinations-len)
                                                     (find-solution target initial comb)
                                                     (combinations switches (inc combinations-len)))))))

(comment
  (let [switches [[3] [1 3] [2] [2 3] [0 2] [0 1]]]
    (find-shortest-solution [0 1 1 0] switches))
  :dimiro1)

(defn parse-lights [s]
  "Parse .##. into [0 1 1 0]"
  (->> (str/split s #"")
       (mapv #(if (= "#" %) 1 0))))

(comment
  (parse-lights ".##.")
  :dimiro1)

(defn parse-line
  "Parse [.##.] (3) (1,3) (2) ... {3,5,4,7} into
  { :lights = [0 1 1 0] :switches = ([3] [1 3] [2] ...) :joltage = [3 5 4 7] }"
  [line]
  (let [[_ lights switches-str joltage-str] (re-matches #"\[([.#]+)\]\s+(.+)\s+\{([\d,]+)\}" line)
        parse-nums #(mapv parse-long (str/split % #","))
        switches (->> (re-seq #"\((\d+(?:,\d+)*)\)" switches-str)
                      (map (comp parse-nums second)))]
    {:lights (parse-lights lights)
     :switches switches
     :joltage (parse-nums joltage-str)}))

(comment
  (parse-line "[.##.] (3) (1,3) (2) (2,3) (0,2) (0,1) {3,5,4,7}")
  (parse-line "[...#.] (0,2,3,4) (2,3) (0,4) (0,1,2) (1,2,3,4) {7,5,12,7,2}")

  ;; I had an issue with this line, the result is all the combinations altogether
  ;; This allow me to uncover an error in the find-shortest-solution, where I was checking for
  ;; the end of the list before comparing the result.
  ;; Nice debugging session.
  (let [parsed (parse-line "[.##.#.#] (2,4) (0,1,4,5,6) (3,4) (0,1,2,4) (0,1,3) (1,2,3,4,5) {40,208,200,187,208,176,8}")]
    (find-shortest-solution (:lights parsed) (:switches parsed)))
  :dimiro1)

(defn parse-input [input]
  (map parse-line (str/split-lines input)))

(defn part-1 [input]
  (let [parsed (parse-input input)]
    (reduce + (map count (map  #(find-shortest-solution (:lights %) (:switches %)) parsed)))))

(comment
  (part-1 (util/read-input "inputs/day10-example.txt"))
  (part-1 (util/read-input "inputs/day10.txt"))
  :dimiro1)
