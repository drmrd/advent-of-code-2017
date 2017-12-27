(ns advent-of-code.day6-test
  (:require [advent-of-code.day6 :refer :all]
            [clojure.test :refer :all]))

(deftest test-reallocate-on-sample-memory-banks
  (testing "Memory reallocation of sample banks [0 2 7 0]"
    (testing "step-by-step"
      (is (= (reallocate [0 2 7 0]) [2 4 1 2]))
      (is (= (reallocate [2 4 1 2]) [3 1 2 3]))
      (is (= (reallocate [3 1 2 3]) [0 2 3 4]))
      (is (= (reallocate [0 2 3 4]) [1 3 4 1]))
      (is (= (reallocate [1 3 4 1]) [2 4 1 2])))
    (testing "ensuring cycle length is computed properly"
      (is (= (:cycle-count (reallocation-cycle-data [0 2 7 0])) 5)))))
