(ns day7.day7
  (:require
    [clojure.test :refer :all]
    [day5.day5 :refer [parse-input run-program]]
    [clojure.math.combinatorics :refer [permutations]]))

(defn read-input []
  (slurp "src/day7/input.txt"))

(defn get-program-output
  {:test (fn []
           (let [ex-program [3 0 3 1 1 0 1 2 4 2 99]]       ; adds the 2 inputs and writes the sum to output
             (is (= (get-program-output ex-program [123 5]) 128))
             ))}
  [program inputs]
  (last (:o (run-program program inputs))))


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
      (recur (next phases) (get-program-output program [(first phases) signal-level])))
    ))


(defn find-max-signal
  {:test (fn []
           (is (= (find-max-signal [] (range 5)
                                   (fn [_ phases] (read-string (apply str (map str phases))))) ; return phase-setting as an int
                  43210))
           )}
  [program phase-range run-amplifiers-fn]
  (->> (permutations phase-range)
       (map (fn [phases] (run-amplifiers-fn program phases)))
       (apply max)
       )
  )

(defn solve-part-one
  {:test (fn []
           (is (= (solve-part-one [3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0] (range 5)) 43210))
           (is (= (solve-part-one [3,23,3,24,1002,24,10,24,1002,23,-1,23,101,5,23,23,1,24,23,23,4,23,99,0,0] (range 5)) 54321))
           (is (= (solve-part-one [3,31,3,32,1002,32,10,32,1001,31,-2,31,1007,31,0,33,
                                    1002,33,7,33,1,33,31,31,1,32,31,31,4,31,99,0,0,0] (range 5)) 65210))

           )}
  [program phase-range]
  (find-max-signal program phase-range run-amplifiers)
  )

(defn run-part-one []
  (-> (read-input)
      (parse-input)
      (solve-part-one (range 5))))



(defn run-amplifier
  {:test (fn []
           (let [ex-program [3 0 3 1 1 0 1 2 4 2 99]]       ; adds the 2 inputs and writes the sum to output
             (is (= (:o (run-amplifier {:p ex-program} [123 5])) [128]))
             ))}
  [amp-state inputs]
  ;(println "rap2" inputs)
  (run-program (:p amp-state) inputs (if (:halted amp-state) (:index amp-state) 0)))


(defn run-amplifiers-with-feedback
  {:test (fn []
           (let [ex-program [3 0 3 1 1 0 1 2 4 2 99]]       ; adds the 2 inputs and writes the sum to output
             (is (= (run-amplifiers-with-feedback ex-program [1 2 3 4 5]) 15))
             (is (= (run-amplifiers-with-feedback ex-program [0 0 0 0 0]) 0))
             )
           (let [ex-program [3 0 3 1 1 0 1 2 4 2 3 0 4 0 99]]       ; adds the 2 inputs and writes the sum to output, then expects one more input, which is output directly
             (is (= (run-amplifiers-with-feedback ex-program [1 2 3 4 5]) 15))
             (is (= (run-amplifiers-with-feedback ex-program [0 0 0 0 0]) 0))
             )
           ; examples from problem description
           (is (= (run-amplifiers-with-feedback [3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26,
                                    27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5]
                                                [9,8,7,6,5]) 139629729))
           (is (= (run-amplifiers-with-feedback [3,52,1001,52,-5,52,3,53,1,52,56,54,1007,54,5,55,1005,55,26,1001,54,
                                    -5,54,1105,1,12,1,53,54,53,1008,54,0,55,1001,55,1,55,2,53,55,53,4,
                                    53,1001,56,-1,56,1005,56,6,99,0,0,0,0,10]
                                                [9,7,8,5,6]) 18216))
           )}
  [program phases]
  (loop [amp-states (mapv (fn [phase]
                               {:p program :i [phase] :o []}) phases)
         amp-index 0
         last-output [0]]
    ;(println "ras2" amp-states amp-index last-output)
    (let [amp-state (nth amp-states amp-index)
          amp-state (run-amplifier amp-state (concat (:i amp-state) last-output))]
      ;(println "ras2." amp-state)
      (if (and (= amp-index 4) (not (:halted amp-state)))
        (last (:o amp-state))
        (recur (assoc amp-states amp-index amp-state)
               (mod (inc amp-index) 5)
               (:o amp-state))))
    ))

(defn solve-part-two
  {:test (fn []
           (is (= (solve-part-two [3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26,
                                    27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5] (range 5 10)) 139629729))
           (is (= (solve-part-two [3,52,1001,52,-5,52,3,53,1,52,56,54,1007,54,5,55,1005,55,26,1001,54,
                                    -5,54,1105,1,12,1,53,54,53,1008,54,0,55,1001,55,1,55,2,53,55,53,4,
                                    53,1001,56,-1,56,1005,56,6,99,0,0,0,0,10] (range 5 10)) 18216))

           )}
  [program phase-range]
  (find-max-signal program phase-range run-amplifiers-with-feedback)
  )

(defn run-part-two []
  (-> (read-input)
      (parse-input)
      (solve-part-two (range 5 10))))

(comment
  (run-part-one)
  (run-part-two)
  )
