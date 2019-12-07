(ns day5.day5
  (:require
    [clojure.test :refer :all]
    [clojure.string :as str]))

(defn read-input []
  (slurp "src/day5/input.txt"))

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


(defn prepare-instruction
  {:test (fn []
           (is (= (prepare-instruction [1,0,0,0,99] 0) {:next-op-index 4 :opcode 1 :params [0 0 0] :param-modes [0 0 0]}))
           (is (= (prepare-instruction [1102,0,0,0,99] 0) {:next-op-index 4 :opcode 2 :params [0 0 0] :param-modes [1 1 0]}))
           (is (= (prepare-instruction [4,0,0,0,99] 0) {:next-op-index 2 :opcode 4 :params [0] :param-modes [0]}))
           (is (= (prepare-instruction [4,0,0,0,99] 4) {:next-op-index 5 :opcode 99 :params [] :param-modes []}))
           )}
  [program opcode-index]
  (let [op-header (get program opcode-index)
        op-code (rem op-header 100)
        param-count (case op-code
                      1 3
                      2 3
                      3 1
                      4 1
                      99 0)
        param-modes (loop [params [] counter param-count param-instr (quot op-header 100)]
                      (if (= counter 0)
                        params
                        (recur
                          (conj params (rem param-instr 10))
                          (dec counter)
                          (quot param-instr 10))))
        ]
    {:next-op-index (+ opcode-index param-count 1)
     :opcode op-code
     :param-modes param-modes
     :params (subvec program (inc opcode-index) (+ opcode-index param-count 1))}))

(defn get-param [program params param-modes index]
  (if (= (get param-modes index) 0)
    (get program (get params index))
    (get params index)))

(defn handle-instruction
  {:test (fn []
           (is (= (handle-instruction [1, 0, 0, 0, 99] 1 [0 0 0] [0 0 0]) {:p [2, 0, 0, 0, 99]}))
           (is (= (handle-instruction [1002, 4, 3, 4, 33] 2 [4 3 4] [0 1 0]) {:p [1002, 4, 3, 4, 99]}))
           (is (= (handle-instruction [3, 2, 0, 0, 0] 3 [2] [0]) {:p [3, 2, 1, 0, 0]}))
           (is (= (handle-instruction [4, 2, 3] 4 [2] [0]) {:p [4, 2, 3] :o 3}))
           (is (= (handle-instruction [104, 2, 3] 4 [2] [1]) {:p [104, 2, 3] :o 2}))
           )}
  [program opcode params param-modes]
  ;  (println "hi" program opcode params param-modes)
  (case opcode
    1 (let [a (get-param program params param-modes 0)
            b (get-param program params param-modes 1)
            result-index (get params 2)]
        {:p (assoc program result-index (+ a b))})
    2 (let [a (get-param program params param-modes 0)
            b (get-param program params param-modes 1)
            result-index (get params 2)]
        {:p (assoc program result-index (* a b))})
    3 (let [result-index (get params 0)]
        {:p (assoc program result-index 1)})
    4 (let [a (get-param program params param-modes 0)]
        {:p program :o a})))

(defn run-program
  {:test (fn []
           (is (= (run-program [1, 0, 0, 0, 99]) {:p [2, 0, 0, 0, 99] :o []}))
           (is (= (run-program [2, 3, 0, 3, 99]) {:p [2, 3, 0, 6, 99] :o []}))
           (is (= (run-program [2, 4, 4, 5, 99, 0]) {:p [2, 4, 4, 5, 99, 9801] :o []}))
           (is (= (run-program [1, 1, 1, 4, 99, 5, 6, 0, 99]) {:p [30, 1, 1, 4, 2, 5, 6, 0, 99] :o []}))
           (is (= (run-program [104, 34, 99]) {:p [104, 34, 99] :o [34]}))
           (is (= (run-program [1,1,1,7,4,7,99,0,0]) {:p [1,1,1,7,4,7,99,2,0] :o [2]}))
           )}
  [program]
  (loop [program program opcode-index 0 outputs []]
    ;(println "rp" program opcode-index outputs)
    (let [instruction (prepare-instruction program opcode-index)
          opcode (:opcode instruction)]
      ;(println "rp2" instruction)
      (if (= opcode 99)
        {:p program :o outputs}
        (let [result (handle-instruction program opcode
                                         (:params instruction)
                                         (:param-modes instruction))]
          (recur (:p result)
                 (:next-op-index instruction)
                 (if (:o result)
                   (conj outputs (:o result))
                   outputs)))))))


(defn run-part-one []
  (-> (read-input)
      (parse-input)
      (run-program)
      (:o)
      (last)))


(comment
  (run-part-one)
  )
