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

(defn solve
  {:test (fn []
           (is (= (solve possible-password? 111111 111111) 1))
           (is (= (solve possible-password? 111110 111112) 2))
           )}
  [password-verifier min max]
  (count (filter password-verifier
                 (range min (inc max)))))

(defn has-pure-double?
  {:test (fn []
           (is (has-pure-double? 112233))
           (is (has-pure-double? 111122))
           (is (not (has-pure-double? 123444)))
           )}
  [number]
  (->> number
       (str)
       (partition-by identity)
       (filter (fn [group-with-identicals] (= 2 (count group-with-identicals))))
       ((fn [double-groups] (> (count double-groups) 0)))))

(defn stricter-possible-password?
  {:test (fn []
           (is (stricter-possible-password? 112233))
           (is (stricter-possible-password? 111122))
           (is (not (stricter-possible-password? 111111)))
           (is (not (stricter-possible-password? 223450)))
           (is (not (stricter-possible-password? 123789)))
           )}
  [number]
  (and (has-pure-double? number) (non-decreasing? number)))

(comment
  (solve possible-password? 183564 657474)
  (solve stricter-possible-password? 183564 657474)
  )
