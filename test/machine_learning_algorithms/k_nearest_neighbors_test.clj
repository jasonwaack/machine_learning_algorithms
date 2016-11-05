(ns machine-learning-algorithms.k-nearest-neighbors-test
  (:require [clojure.test :refer :all]
            [machine-learning-algorithms.k-nearest-neighbors :refer :all]))


(defn squared [value]
  (Math/pow value 2))

(defn squared-difference [x a]
  (squared (- x a)))

(defn square-root [value]
  (Math/sqrt value))

(defn sum-of-squared-differences [point-1 point-2]
  (apply +
         (map #(squared-difference % %2) point-1 point-2)))

(defn euclidean-distance [point-1 point-2]
  (square-root
   (sum-of-squared-differences point-1 point-2)))

(defn data-distance-from-entity [entity training-data]
  (mapv #(conj % (euclidean-distance entity (get % 0))) training-data)
  )

(defn sorted-labels-by-increasing-distance [entity training-data]
  (mapv #(get % 1)
        (sort-by #(get % 2)
                 (data-distance-from-entity entity training-data))))

;; (deftest sum-of-squared-differences-test
;;   (testing "sum of squared differences"
;;     (is (= (sum-of-squared-differences [0.5 -0.2 1.7] [0.2 0.9 0.45]) 2.862500))))

(deftest squared-test
  (testing "squared"
    (is (= (squared 3) 9.0))))

(deftest squared-differnce-test
  (testing "squared different"
    (is (= (squared-difference 5 2) 9.0))))

(deftest square-root-test
  (testing "square root"
    (is (= (square-root 9) 3.0))))

(deftest distances-test
  (testing "distances"
    (is (= (euclidean-distance [2 -1] [-2 2]) 5.0))))

(def simple-data
  [[[1 1] "one"]
   [[2 2] "two"]
   [[3 3] "three"]])

(deftest sort-by-increasing-distance-test
  (testing "sort distances in increasing order"
    (is (= (sorted-labels-by-increasing-distance [4 4] simple-data)
           ["three" "two" "one"]))))
