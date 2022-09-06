(ns bortexz.utils.math-test
  (:require [clojure.test :refer [is deftest testing are]]
            [bortexz.utils.math :as umath])
  (:import (java.math RoundingMode)))

(deftest round-step
  (testing "round-step"
    (are [x y] (= x y)
      (umath/round-step 1.25M 0.5M RoundingMode/HALF_DOWN) 1.0M 
      (umath/round-step 1.25M 0.5M RoundingMode/HALF_UP) 1.5M
      (umath/round-step 1.3M 0.5M RoundingMode/DOWN) 1.0M
      (umath/round-step 1.1M 0.5M RoundingMode/UP) 1.5M
      (umath/round-step -1.3M 0.5M RoundingMode/CEILING) -1.0M
      (umath/round-step -1.3M 0.5M RoundingMode/FLOOR) -1.5M)))

(deftest mean
  (testing "mean"
    (is (= 3 (umath/mean [1 3 5])))))

(deftest variance-std-dev
  (let [xs [2 4 4 4 5 5 7 9]]
    (testing "variance"
      (is (= 4 (umath/variance xs))))
    (testing "standard-dev"
      (is (= 2 (umath/standard-deviation xs))))))


