(ns day2.day2
  (:require
    [clojure.test :refer :all]
    [clojure.string :as str]))

(defn read-input []
  (slurp "src/day2/input-eget.txt"))

(defn parse-input
  {:test (fn []
           (is (= (parse-input "1,3,7") [1 3 7]))
           )}
  [file-data]
  (->>
    file-data
    str/split-lines
    first
    ((fn [line] (str/split line #",")))
    (map read-string)
    (vec)
    ))




(defn handle-opcode
  {:test (fn []
           (is (= (handle-opcode [1,0,0,0,99] 0) [2,0,0,0,99]))
           (is (= (handle-opcode [1,1,2,4,99] 0) [1,1,2,4,3]))
           (is (= (handle-opcode [2,0,0,0,99] 0) [4,0,0,0,99]))
           (is (= (handle-opcode [2,3,0,3,99] 0) [2,3,0,6,99]))
           )}
  [program opcode-index]
  (let [[opcode a-index b-index result-index] (subvec program opcode-index (+ opcode-index 4))
        a (get program a-index)
        b (get program b-index)]
    (case opcode
      1 (assoc program result-index (+ a b))
      2 (assoc program result-index (* a b)))))

(defn run-program
  {:test (fn []
           (is (= (run-program [1,0,0,0,99]) [2,0,0,0,99]))
           (is (= (run-program [2,3,0,3,99]) [2,3,0,6,99]))
           (is (= (run-program [2,4,4,5,99,0]) [2,4,4,5,99,9801]))
           (is (= (run-program [1,1,1,4,99,5,6,0,99]) [30,1,1,4,2,5,6,0,99]))
           )}
  [program]
  (loop [program program opcode-index 0]
    (let [opcode (get program opcode-index)]
      (if (= opcode 99)
        program
        (recur (handle-opcode program opcode-index) (+ opcode-index 4))))))

(defn prepare-program
  {:test (fn []
           (is (= (prepare-program [0,0,0,0,0] 123 456) [123,456,0,0,0]))
           )}
  [program a b]
  (-> program
      (assoc 1 a)
      (assoc 2 b)
      ))

(defn run-part-one []
  (-> (read-input)
       (parse-input)
       (prepare-program 12 2)
       (run-program)
       (first)
       ))

(defn run-part-two []
  (let [program (parse-input (read-input))]
    (some (fn [[noun verb]]
            (let [result (first (run-program (prepare-program program noun verb)))]
              (if (= 19690720 result) (+ (* 100 noun) verb))))
          (for [noun (range 100) verb (range 100)] [noun verb]))))

(comment
  (run-part-one)
  (run-part-two)
  )
