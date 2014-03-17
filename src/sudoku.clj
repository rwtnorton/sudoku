(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  ((comp not zero?) (value-at board coord)))

(defn row-values [board coord]
  (let [[row _] coord]
    (set (get board row))))

(defn col-values [board coord]
  (let [[_ col] coord
        vs (map (fn [line]
                  (get line col)) board)]
    (set vs)))

(defn coord-pairs [coords]
  (for [r coords
        c coords]
    [r c]))

(defn block-coords
  ([block-row block-col]
     (let [row-start (* 3 block-row)
           col-start (* 3 block-col)]
       (for [r (range row-start (+ 3 row-start))
             c (range col-start (+ 3 col-start))]
         [r c])))
  ([]
     (block-coords 0 0)))

(defn block-values [board coord]
  (let [[coord-row coord-col] coord
        block-row (quot coord-row 3)
        block-col (quot coord-col 3)
        f (fn [coord] (value-at board coord))
        coords (block-coords block-row block-col)]
    (set
     (map f coords))))

(def all-values #{1 2 3 4 5 6 7 8 9})

(defn valid-values-for [board coord]
  (if (has-value? board coord)
    #{}
    (set/difference all-values
                    (row-values board coord)
                    (col-values board coord)
                    (block-values board coord))))

(defn all-coords [board]
  ;; Assumes board is square.
  (coord-pairs (range (count board))))

(defn filled? [board]
  (let [f (fn [coord] (value-at board coord))
        vals (set (map f (all-coords board)))]
    (not (vals 0))))

(defn rows [board]
  (mapv set board))

(def winning-value-sets
  (replicate (count all-values) all-values))

(defn valid-rows? [board]
  (= (rows board)
     winning-value-sets))

(defn cols [board]
  (let [first-row (board 0)
        col-count (count first-row)]
  (mapv (fn [c]
          (col-values board [0 c])) (range col-count))))

(defn valid-cols? [board]
  (= (cols board)
     winning-value-sets))

(defn blocks [board]
  (let [coords (coord-pairs [0 3 6])]
    (mapv (fn [coord]
            (block-values board coord)) coords)))

(defn valid-blocks? [board]
  (= (blocks board)
     winning-value-sets))

(defn valid-solution? [board]
  (and (filled? board)
       (valid-rows? board)
       (valid-cols? board)
       (valid-blocks? board)))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn find-empty-point [board]
  (first
   (filter (fn [coord]
             (not (has-value? board coord)))
           (all-coords board))))

(defn solve [board]
  (let [helper (fn [b]
                 (if (valid-solution? b)
                   [b]
                   [b]))]
    (board [[5 3 4 6 7 8 9 1 2]
            [6 7 2 1 9 5 3 4 8]
            [1 9 8 3 4 2 5 6 7]
            [8 5 9 7 6 1 4 2 3]
            [4 2 6 8 5 3 7 9 1]
            [7 1 3 9 2 4 8 5 6]
            [9 6 1 5 3 7 2 8 4]
            [2 8 7 4 1 9 6 3 5]
            [3 4 5 2 8 6 1 7 9]])))
