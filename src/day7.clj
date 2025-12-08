(ns day7
  (:require [clojure.string :as str]
            [util]))

(defn parse-input [input]
  (let [lines (mapv #(str/split % #"") (str/split-lines input))
        first-line (first lines)
        start (.indexOf first-line "S")
        ;; each element will become {:row 0 :col 0 :value .}
        ;; this is important because we need these coordinates to check the neighbors
        ;; when we are simulating the puzzle.
        puzzle (->> lines
                    (map-indexed (fn [row line]
                                   (map-indexed (fn [col value]
                                                  {:row row :col col :value value})
                                                line)))
                    (mapv vec))]
    {:puzzle puzzle}))

(comment
  (parse-input (util/read-input "inputs/day7-example.txt"))
  :dimiro1)

(defn get-in-direction [puzzle {:keys [row col]} direction]
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
    {:row dest-row
     :col dest-col
     :value (get-in puzzle [dest-row dest-col :value] ".")}))

(comment
  (let [puzzle (:puzzle (parse-input (util/read-input "inputs/day7-example.txt")))]
    (get-in-direction puzzle {:row 1 :col 7} :south))
  :dimiro1)


(defn print-puzzle [puzzle]
  (doseq [[idx row] (map-indexed vector puzzle)]
    (println (format "%s" (str/join "" (map :value row))))))

(defn simulate-row [puzzle row]
  (let [new-row (mapv (fn [{:keys [row col value] :as cell}]
                        (let [north     (get-in-direction puzzle cell :north)
                              east      (get-in-direction puzzle cell :east)
                              west      (get-in-direction puzzle cell :west)
                              northeast (get-in-direction puzzle cell :northeast)
                              northwest (get-in-direction puzzle cell :northwest)]
                          (cond
                            ;; S -> |
                            (= value "S") {:row row :col col :value "|"}
                            ;; previous |
                            ;; current  | -> if previous is | then current becomes |
                            (and (= (:value north) "|")
                                 (not= value "^")) {:row row :col col :value "|"}
                            ;; .^.
                            ;;   c
                            ;; current becomes | if west and northwest is |
                            (and (= (:value west) "^")
                                 (= (:value northwest) "|")) {:row row :col col :value "|"}
                            ;; .^.
                            ;; c
                            ;; current becomes | if east and northeast is |
                            (and (= (:value east) "^")
                                 (= (:value northeast) "|")) {:row row :col col :value "|"}
                            :else cell)))
                      row)
        ;; Count ^ that have | above them.
        ;;  |
        ;; |^|
        splits (count (filter (fn [{:keys [value] :as cell}]
                                (let [north (get-in-direction puzzle cell :north)]
                                  (and (= value "^")
                                       (= (:value north) "|"))))
                              row))]
    {:row new-row :splits splits}))

(defn simulate [puzzle]
  (reduce (fn [{:keys [puzzle total-splits]} row-idx]
            (let [row (get puzzle row-idx)
                  {:keys [row splits]} (simulate-row puzzle row)]
              {:puzzle (assoc puzzle row-idx row)
               :total-splits (+ total-splits splits)}))
          {:puzzle puzzle :total-splits 0}
          (range (count puzzle))))

(defn part-1 [input]
  (let [puzzle (:puzzle (parse-input input))
        result (simulate puzzle)]
    (:total-splits result)))

(comment
  (part-1 (util/read-input "inputs/day7-example.txt"))
  (part-1 (util/read-input "inputs/day7.txt"))
  :dimiro1)

(defn part-2 [] nil)
