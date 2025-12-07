(ns day6
  (:require [clojure.string :as str]
            [clojure.math :as math]
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

(defn print-matrix [matrix]
  (println "----------")
  (doseq [row matrix]
    (println (apply str (interpose "" (map #(format "%2s" %) row))))))

(defn mark-operation-splitter [operation-line]
  (str/replace operation-line #" ([\+\*])" "x$1"))

(defn decompose-number [n max-digits]
  (->> (range (dec max-digits) -1 -1)
       (mapv #(mod (quot n (long (Math/pow 10 %))) 10))))

(comment
  (decompose-number 651)
  (decompose-number 6514)
  :dimiro1)

(defn rotate-numbers [arr]
  "[123 45 6] -> [356 24 1]"
  (let [max-digits (->> arr (map str) (map count) (apply max))]
    (->> arr
         (map #(decompose-number % max-digits))
         (apply map vector)
         (map (fn [col]
                (->> col
                     (remove zero?)
                     (apply str)
                     parse-long)))
         (remove nil?)
         reverse
         vec)))

(comment
  (rotate-numbers [123 45 6])
  :dimiro1)

(defn part-2 [input]
  (let [lines            (str/split-lines input)
        raw-number-lines (->> (butlast lines)
                              (mapv #(str/split % #"")))
        ;; [*   +   *   +] -> [*  x+  x*  x+]
        raw-operations       (str/split (mark-operation-splitter (last lines)) #"")
        ;; [*  x+  x*  x+] -> ["*" "+" "*" "+"]
        operations          (->> raw-operations
                                 (filter #(contains? #{"*" "+"} %))
                                 vec)

        ;; [  6 98  215 314] -> [  6x98 x215x314]
        number-lines-with-divider (reduce (fn [nums [idx elem]]
                                            (if (= "x" elem)
                                              (mapv #(assoc % idx "x") nums)
                                              nums))
                                          raw-number-lines
                                          (map-indexed vector raw-operations))
        ;; [  6x98 x215x314] -> [006x980x215x314]
        number-lines-with-zeros   (mapv (fn [row]
                                          (mapv #(if (= " " %) "0" %) row))
                                        number-lines-with-divider)
        max-len                   (apply max (map count number-lines-with-zeros))
        ;; [123 328  51 64] -> [123x328x051x640]
        number-lines-padded       (mapv (fn [row]
                                          (let [diff (- max-len (count row))]
                                            (into row (repeat diff "0"))))
                                        number-lines-with-zeros)
        ;; ["1" "2" "3" "x" "3" "2" "8" ...] -> [123 328 51 640]
        number-lines              (mapv (fn [row]
                                          (->> (str/join "" row)
                                               (#(str/split % #"x"))
                                               (mapv parse-long)))
                                        number-lines-padded)
        ;; [123
        ;;   45
        ;;    6] -> [123 45 6]
        number-lines-transposed (apply mapv vector number-lines)
        ;; [123 45 6] -> [356 24 1]
        numbers-rotated      (mapv rotate-numbers number-lines-transposed)
        ;; [123 45 6 "*"]
        problems     (mapv (fn [row op] (conj row op)) numbers-rotated operations)
        ;; [1058 3253600 625 8544]
        result      (mapv apply-operation problems)]
    (apply + result))
  )

(comment
  (part-2 (util/read-input "inputs/day6-example.txt"))
  (part-2 (util/read-input "inputs/day6.txt"))
  :dimiro1)
