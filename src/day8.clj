(ns day8
  (:require [clojure.string :as str]
            [util]))

(defn euclidean-distance [box1 box2]
  "box is a triple [x y z]"
  (let [[x1 y1 z1] box1
        [x2 y2 z2] box2
        dx (- x2 x1)
        dy (- y2 y1)
        dz (- z2 z1)]
    (Math/sqrt (+ (* dx dx) (* dy dy) (* dz dz)))))

(comment
  (euclidean-distance [162 817 812] [425 690 689])
  :dimiro1)

(defn boxes-with-distances [boxes]
  "Compute the distance of all the boxes, returns [box1 box2 distance]"
  (let [indexed (vec boxes)]
    (for [[i box1] (map-indexed vector indexed)
          [j box2] (map-indexed vector indexed)
          :when (< i j)]
      [box1 box2 (euclidean-distance box1 box2)])))


(defn parse-input [input]
  (->> (str/split-lines input)
       (mapv #(->> (str/split % #",")
                   (mapv parse-long)))))

(comment
  (parse-input (util/read-input "inputs/day8-example.txt"))
  :dimiro1)

(defn sort-boxes [boxes]
  (sort-by (fn [[_ _ distance]] distance) boxes))

(defn find-circuit [circuits box]
  "Find the circuit in wich the box is in."
  (first (filter #(% box) circuits)))

(defn build-circuits [sorted-pairs num-connections]
  (let [all-boxes (set (mapcat (fn [[box1 box2 _]] [box1 box2]) sorted-pairs))
        ;; each box starts with their own circuit.
        circuits (atom (set (map hash-set all-boxes)))]
    ;; connect together the 10 (example) 1000 (real problem) pairs
    (doseq [[box1 box2 _] (take num-connections sorted-pairs)]
      (let [c1 (find-circuit @circuits box1)
            c2 (find-circuit @circuits box2)]
        (when (not= c1 c2)
          (swap! circuits #(-> %
                               (disj c1 c2)
                               (conj (into c1 c2)))))))
    @circuits))

(defn part-1 [input num-connections]
    (let [boxes (parse-input input)
          circuits (->> (boxes-with-distances boxes)
                        sort-boxes
                        (#(build-circuits % num-connections)))
          sizes (->> circuits
                     (map count)
                     (sort >))]
      ;; Multiplying together the sizes of the three largest circuits
      (apply * (take 3 sizes))))


(comment
  (part-1 (util/read-input "inputs/day8-example.txt") 10)
  (part-1 (util/read-input "inputs/day8.txt") 1000)
  :dimiro1)
