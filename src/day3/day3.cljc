(ns day3.day3
  (:require
    [clojure.test :refer :all]
    [clojure.string :as str]))

(defn read-input []
  (slurp "src/day3/input.txt"))

(defn parse-wire-part
  {:test (fn []
           (is (= (parse-wire-part "R75") [:r 75]))
           (is (= (parse-wire-part "D75") [:d 75]))
           (is (= (parse-wire-part "U25") [:u 25]))
           )}
  [part-string]
  (let [direction-str (subs part-string 0 1)
        direction (case direction-str
                    "U" :u
                    "R" :r
                    "D" :d
                    "L" :l)
        distance (read-string (subs part-string 1))]
    [direction distance]))

(defn parse-wire
  {:test (fn []
           (is (= (parse-wire "R75,D30,L83,U83") [[:r 75] [:d 30] [:l 83] [:u 83]]))
           )}
  [wire-string]
  (->> (str/split wire-string #",")
       (map parse-wire-part)))


(defn get-wire-part-coords
  {:test (fn []
           (is (= (get-wire-part-coords [0 0] [:u 3]) [[0 1] [0 2] [0 3]]))
           (is (= (get-wire-part-coords [0 0] [:r 3]) [[1 0] [2 0] [3 0]]))
           (is (= (get-wire-part-coords [0 0] [:d 3]) [[0 -1] [0 -2] [0 -3]]))
           (is (= (get-wire-part-coords [0 0] [:l 3]) [[-1 0] [-2 0] [-3 0]]))
           )}
  [[start-x start-y] [direction length]]
  (case direction
    :u (for [y (take length (iterate inc (inc start-y)))] [start-x y])
    :r (for [x (take length (iterate inc (inc start-x)))] [x start-y])
    :d (for [y (take length (iterate dec (dec start-y)))] [start-x y])
    :l (for [x (take length (iterate dec (dec start-x)))] [x start-y])
    )
  )

(defn get-wire-coords
  {:test (fn []
           (is (= (get-wire-coords [[:r 1] [:d 3]]) [[0 0] [1 0] [1 -1] [1 -2] [1 -3]]))
           )}
  [wire-parts]
  (reduce (fn [result part]
            (concat result (get-wire-part-coords (last result) part)))
          [[0 0]]
          wire-parts))

(defn manhattan
  {:test (fn []
           (is (= (manhattan [0 3]) 3))
           (is (= (manhattan [0 -3]) 3))
           (is (= (manhattan [-3 -3]) 6))
           )}
  [[x y]]
  (+  (Math/abs x) (Math/abs y)))

(defn solve-part-one
  {:test (fn []
           (is (= (solve-part-one
                    "R75,D30,R83,U83,L12,D49,R71,U7,L72"
                    "U62,R66,U55,R34,D71,R55,D58,R83") 159))
           (is (= (solve-part-one
                    "R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51"
                    "U98,R91,D20,R16,D67,R40,U7,R15,U6,R7") 135))
           )}
  [wire-1 wire-2]
  (let [wire-1-coords (set (get-wire-coords (parse-wire wire-1)))
        wire-2-coords (set (get-wire-coords (parse-wire wire-2)))
        intersections (clojure.set/intersection wire-1-coords wire-2-coords)]
    (apply min (map manhattan (disj intersections [0 0])))))

(defn run-part-one []
  (let [[wire-1 wire-2] (str/split-lines (read-input))]
    (solve-part-one wire-1 wire-2)))

(comment
  (run-part-one)
  )
