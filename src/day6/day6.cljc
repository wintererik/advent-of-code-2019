(ns day6.day6
  (:require
    [clojure.test :refer :all]
    [clojure.string :as str]
    [clojure.set :as set]))

(defn read-input []
  (slurp "src/day6/input.txt"))

(defn parse-input
  {:test (fn []
           (is (= (parse-input "COM)A\nA)B") [["COM" "A"] ["A" "B"]]))
           )}
  [file-data]
  (->>
    file-data
    str/split-lines
    (map (fn [line] (str/split line #"\)")))
    ))


(defn create-tree
  {:test (fn []
           (is (= (create-tree [["COM" "A"] ["A" "B"]]) {"COM" ["A"],
                                                         "A"   ["B"]
                                                         "B"   []}))
           (is (= (create-tree [["COM" "A"] ["COM" "B"] ["B" "C"]])
                  {"COM" ["A" "B"], "B" ["C"] "A" [] "C" []}))
           )}
  [node-bindings]
  (reduce (fn [result [binding-key binding-value]]
            (as-> result result
                  (if (get result binding-value)
                    result
                    (assoc result binding-value []))
                  (if (get result binding-key)
                    (assoc result binding-key (conj (get result binding-key) binding-value))
                    (assoc result binding-key [binding-value])))
            )
          {}
          node-bindings))

(defn get-depths
  {:test (fn []
           (is (= (get-depths {"COM" ["A"], "A"   ["B"] , "B"   []})
                  {"COM" 0, "A"   1, "B"   2}))
           )}
  [tree]
  (loop [nodes-at-current-depth ["COM"] depth 0 result {}]
    (if (empty? nodes-at-current-depth)
      result
      (recur
        (apply concat (map (fn [node] (get tree node)) nodes-at-current-depth))
        (inc depth)
        (reduce
          (fn [result node]
            (assoc result node depth))
          result
          nodes-at-current-depth)))
    ))

(defn solve-part-one
  {:test (fn []
           (is (= (solve-part-one (parse-input "COM)B\nB)C\nC)D\nD)E\nE)F\nB)G\nG)H\nD)I\nE)J\nJ)K\nK)L"))
                  42))
           )}
  [node-bindings]
  (->> node-bindings
      (create-tree)
      (get-depths)
      (vals)
      (reduce +)))

(defn run-part-one []
  (-> (read-input)
      (parse-input)
      (solve-part-one)))


(defn get-path-to-COM
  {:test (fn []
           (is (= (get-path-to-COM {"C" "B", "D" "B", "B" "A", "A" "COM"} "C")
                  ["B" "A" "COM"]))
           )}
  [child-to-parent-bindings start]
  (loop [node (get child-to-parent-bindings start) result []]
    (if (= node "COM")
      (conj result node)
      (recur (get child-to-parent-bindings node) (conj result node)))))

(defn get-steps
  {:test (fn []
           (is (= (get-steps ["B" "A" "COM"] ["C" "A" "COM"]) 2))
           (is (= (get-steps ["C" "B" "A" "COM"] ["D" "A" "COM"]) 3))
           )}
  [path-a path-b]
  (count (set/difference
           (set/union (set path-a) (set path-b))
           (set/intersection (set path-a) (set path-b)))))

(defn solve-part-two
  {:test (fn []
           (is (= (solve-part-two (parse-input "COM)B\nB)C\nC)D\nD)E\nE)F\nB)G\nG)H\nD)I\nE)J\nJ)K\nK)L\nK)YOU\nI)SAN"))
                  4))
           )}
  [node-bindings]
  (let [child-to-parent (reduce (fn [result [parent child]] (assoc result child parent)) {} node-bindings)
        path-a (get-path-to-COM child-to-parent "YOU")
        path-b (get-path-to-COM child-to-parent "SAN")
        ]
    (get-steps path-a path-b)))

(defn run-part-two []
  (->> (read-input)
      (parse-input)
      (solve-part-two)))

(comment
  (run-part-one)
  (run-part-two)
  )
