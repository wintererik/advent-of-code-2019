(ns day7.day7
  (:require
    [clojure.test :refer :all]
    [day5.day5 :refer [parse-input run-program]]
    [clojure.math.combinatorics :refer [permutations]]))

(defn read-input []
  (slurp "src/day7/input.txt"))

(defn run-amplifier
  {:test (fn []
           (let [ex-program [3 0 3 1 1 0 1 2 4 2 99]]       ; adds the 2 inputs and writes the sum to output
             (is (= (run-amplifier ex-program 123 5) 128))
             ))}
  [program phase input-signal]
  (last (:o (run-program program [phase input-signal]))))


(defn run-amplifiers
  {:test (fn []
           (let [ex-program [3 0 3 1 1 0 1 2 4 2 99]]       ; adds the 2 inputs and writes the sum to output
             (is (= (run-amplifiers ex-program [1 2 3 4 5]) 15))
             (is (= (run-amplifiers ex-program [0 0 0 0 0]) 0))
             ))}
  [program phases]
  (loop [phases phases signal-level 0]
    (if (empty? phases)
      signal-level
      (recur (next phases) (run-amplifier program (first phases) signal-level)))
    ))


(defn find-max-signal
  {:test (fn []
           (is (= (find-max-signal [3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0]) 43210))
           (is (= (find-max-signal [3,23,3,24,1002,24,10,24,1002,23,-1,23,101,5,23,23,1,24,23,23,4,23,99,0,0]) 54321))
           (is (= (find-max-signal [3,31,3,32,1002,32,10,32,1001,31,-2,31,1007,31,0,33,
                                    1002,33,7,33,1,33,31,31,1,32,31,31,4,31,99,0,0,0]) 65210))
           )}
  [program]
  (->> (permutations (range 0 5))
       (map (fn [phases] (run-amplifiers program phases)))
       (apply max)
       )
  )

(defn run-part-one []
  (-> (read-input)
      (parse-input)
      (find-max-signal)))



(comment
  (run-part-one)
  )
