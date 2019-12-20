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
                      5 2
                      6 2
                      7 3
                      8 3
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
           (is (= (handle-instruction [1, 0, 0, 0, 99] 1 [0 0 0] [0 0 0] [1]) {:p [2, 0, 0, 0, 99]}))
           (is (= (handle-instruction [1002, 4, 3, 4, 33] 2 [4 3 4] [0 1 0] [1]) {:p [1002, 4, 3, 4, 99]}))
           (is (= (handle-instruction [3, 2, 0, 0, 0] 3 [2] [0] [1]) {:p [3, 2, 1, 0, 0] :i []}))
           (is (= (handle-instruction [4, 2, 3] 4 [2] [0] [1]) {:p [4, 2, 3] :o 3}))
           (is (= (handle-instruction [104, 2, 3] 4 [2] [1] [1]) {:p [104, 2, 3] :o 2}))
           )}
  [program opcode params param-modes input-values]
  ;(println "hi" program opcode params param-modes input-values)
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
        {:p (assoc program result-index (first input-values)) :i (or (next input-values) [])})
    4 (let [a (get-param program params param-modes 0)]
        {:p program :o a})


    5 (let [a (get-param program params param-modes 0)
            b (get-param program params param-modes 1)]
        {:p         program
         :next-op-index (if (not (zero? a))
                      b
                      nil)})

    6 (let [a (get-param program params param-modes 0)
            b (get-param program params param-modes 1)]
        {:p         program
         :next-op-index (if (zero? a)
                      b
                      nil)})

    7 (let [a (get-param program params param-modes 0)
            b (get-param program params param-modes 1)
            result-index (get params 2)]
        {:p (assoc program result-index (if (< a b) 1 0))})
    8 (let [a (get-param program params param-modes 0)
            b (get-param program params param-modes 1)
            result-index (get params 2)]
        {:p (assoc program result-index (if (= a b) 1 0))})

    ))

(defn run-program
  {:test (fn []
           (is (= (run-program [1, 0, 0, 0, 99] [1]) {:p [2, 0, 0, 0, 99] :o []}))
           (is (= (run-program [2, 3, 0, 3, 99] [1]) {:p [2, 3, 0, 6, 99] :o []}))
           (is (= (run-program [2, 4, 4, 5, 99, 0] [1]) {:p [2, 4, 4, 5, 99, 9801] :o []}))
           (is (= (run-program [1, 1, 1, 4, 99, 5, 6, 0, 99] [1]) {:p [30, 1, 1, 4, 2, 5, 6, 0, 99] :o []}))
           (is (= (run-program [104, 34, 99] [1]) {:p [104, 34, 99] :o [34]}))
           (is (= (run-program [1, 1, 1, 7, 4, 7, 99, 0, 0] [1]) {:p [1, 1, 1, 7, 4, 7, 99, 2, 0] :o [2]}))

           (is (= (:o (run-program [3, 9, 8, 9, 10, 9, 4, 9, 99, -1, 8] [123])) [0]))
           (is (= (:o (run-program [3, 9, 8, 9, 10, 9, 4, 9, 99, -1, 8] [8])) [1]))

           (is (= (:o (run-program [3, 9, 7, 9, 10, 9, 4, 9, 99, -1, 8] [7])) [1]))
           (is (= (:o (run-program [3, 9, 7, 9, 10, 9, 4, 9, 99, -1, 8] [8])) [0]))

           (is (= (:o (run-program [3, 3, 1108, -1, 8, 3, 4, 3, 99] [123])) [0]))
           (is (= (:o (run-program [3, 3, 1108, -1, 8, 3, 4, 3, 99] [8])) [1]))

           (is (= (:o (run-program [3, 3, 1107, -1, 8, 3, 4, 3, 99] [7])) [1]))
           (is (= (:o (run-program [3, 3, 1107, -1, 8, 3, 4, 3, 99] [8])) [0]))

           (is (= (:o (run-program [3, 12, 6, 12, 15, 1, 13, 14, 13, 4, 13, 99, -1, 0, 1, 9] [123])) [1]))
           (is (= (:o (run-program [3, 12, 6, 12, 15, 1, 13, 14, 13, 4, 13, 99, -1, 0, 1, 9] [0])) [0]))


           (is (= (:o (run-program [3, 21, 1008, 21, 8, 20, 1005, 20, 22, 107, 8, 21, 20, 1006, 20, 31,
                                    1106, 0, 36, 98, 0, 0, 1002, 21, 125, 20, 4, 20, 1105, 1, 46, 104,
                                    999, 1105, 1, 46, 1101, 1000, 1, 20, 4, 20, 1105, 1, 46, 98, 99] [7])) [999]))
           (is (= (:o (run-program [3, 21, 1008, 21, 8, 20, 1005, 20, 22, 107, 8, 21, 20, 1006, 20, 31,
                                    1106, 0, 36, 98, 0, 0, 1002, 21, 125, 20, 4, 20, 1105, 1, 46, 104,
                                    999, 1105, 1, 46, 1101, 1000, 1, 20, 4, 20, 1105, 1, 46, 98, 99] [8])) [1000]))
           (is (= (:o (run-program [3, 21, 1008, 21, 8, 20, 1005, 20, 22, 107, 8, 21, 20, 1006, 20, 31,
                                    1106, 0, 36, 98, 0, 0, 1002, 21, 125, 20, 4, 20, 1105, 1, 46, 104,
                                    999, 1105, 1, 46, 1101, 1000, 1, 20, 4, 20, 1105, 1, 46, 98, 99] [9])) [1001]))

           ; A program reading more input than given is expected to halt
           (is (= (run-program [3, 4, 3, 4, 99] [123]) {:p [3, 4, 3, 4, 123] :halted true :index 2 :o []}))

           ; A program can be resumed
           (is (= (run-program [3, 0, 3, 0, 4, 0, 99] [123] 2) {:p [123, 0, 3, 0, 4, 0, 99]:o [123]}))

           )}
  (
   [program inputs]
   (run-program program inputs 0))
  ([program inputs current-index]
   (loop [program program opcode-index current-index outputs [] inputs inputs]
     ;(println "rp" program opcode-index outputs)
     (let [instruction (prepare-instruction program opcode-index)
           opcode (:opcode instruction)]
       ;(println "rp2" instruction inputs)
       (cond
         (= opcode 99)
         {:p program :o outputs}

         (and (= opcode 3) (empty? inputs))
         {:p program :o outputs :halted true :index opcode-index}

         :else
         (let [result (handle-instruction program opcode
                                          (:params instruction)
                                          (:param-modes instruction)
                                          inputs)]
           (recur (:p result)
                  (or (:next-op-index result) (:next-op-index instruction))
                  (if (:o result)
                    (conj outputs (:o result))
                    outputs)
                  (or (:i result) inputs))))))))



(defn run-part-one []
  (-> (read-input)
      (parse-input)
      (run-program [1])
      (:o)
      (last)))


(defn run-part-two []
  (-> (read-input)
      (parse-input)
      (run-program [5])
      (:o)
      (last)))

(comment
  (run-part-one)
  (run-part-two)
  )
