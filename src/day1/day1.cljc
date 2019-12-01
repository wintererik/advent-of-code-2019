(ns day1.day1
  (:require
    [clojure.test :refer :all]
    [clojure.string :as str]))

(defn read-input []
  (slurp "src/day1/input.txt"))

(defn parse-input
  {:test (fn []
           (is (= (parse-input "1\n3") (seq [1 3])))
           )}
  [file-data]
  (->>
    file-data
    str/split-lines
    (map read-string)))


(defn calculate-fuel
  {:test (fn []
           (is (= (calculate-fuel 12) 2))
           (is (= (calculate-fuel 14) 2))
           (is (= (calculate-fuel 1969) 654))
           (is (= (calculate-fuel 100756) 33583))
           )}
  [mass]
  (-> mass
      (/ 3)
      (Math/floor)
      (int)
      (- 2)))

(defn run-part-one []
  (->> (read-input)
       (parse-input)
       (map calculate-fuel)
       (reduce +)))

(defn calculate-fuel-incl-fuel-mass
  {:test (fn []
           (is (= (calculate-fuel-incl-fuel-mass 14) 2))
           (is (= (calculate-fuel-incl-fuel-mass 1969) 966))
           (is (= (calculate-fuel-incl-fuel-mass 100756) 50346))
           )}
  [module-mass]
  (loop [masses []
         next-fuel-mass (calculate-fuel module-mass)]
    (if (> next-fuel-mass 0)
      (recur (conj masses next-fuel-mass) (calculate-fuel next-fuel-mass))
      (reduce + masses))))

(defn run-part-two []
  (->> (read-input)
       (parse-input)
       (map calculate-fuel-incl-fuel-mass)
       (reduce +)))

(comment
  (run-part-one)
  (run-part-two)
  )
