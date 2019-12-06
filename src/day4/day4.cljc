(ns day4.day4
  (:require
    [clojure.test :refer :all]))


(defn non-decreasing?
  {:test (fn []
           (is (non-decreasing? 111111))
           (is (not (non-decreasing? 223450)))
           (is (non-decreasing? 123789))
           )}
  [number]
  (let [number-as-char-seq (seq (str number))
        sorted (sort number-as-char-seq)]
    (= number-as-char-seq sorted)))

(defn has-double?
  {:test (fn []
           (is (has-double? 111111))
           (is (has-double? 223450))
           (is (not (has-double? 123789)))
           )}
  [number]
  (let [number-as-string (str number)
        without-doubles (dedupe number-as-string)]
    (not (= (count number-as-string) (count without-doubles)))))

(defn possible-password?
  {:test (fn []
           (is (possible-password? 111111))
           (is (not (possible-password? 223450)))
           (is (not (possible-password? 123789)))
           )}
  [number]
  (and (has-double? number) (non-decreasing? number)))

(defn solve-part-one
  {:test (fn []
           (is (= (solve-part-one 111111 111111) 1))
           (is (= (solve-part-one 111110 111112) 2))
           )}
  [min max]
  (count (filter possible-password?
                 (range min (inc max)))))

(comment
  (solve-part-one 183564 657474)
  )
