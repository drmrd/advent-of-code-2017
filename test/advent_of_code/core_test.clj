(ns advent-of-code.core-test
  (:require [clojure.test :refer :all]
            [advent-of-code.core :refer :all]))

(deftest int-to-digit-list-0-test
  (testing "Test the digit array for 0 is (0)"
    (is (= (int-to-digit-list 0) '(0)))))

(deftest int-to-digit-list-1-test
  (testing "Test the digit array for 1 is (1)"
    (is (= (int-to-digit-list 1) '(1)))))

(deftest int-to-digit-list-9876543210-test
  (testing "Test the correct digit array for 9876543210 is returned"
    (is (= (int-to-digit-list 9876543210) '(9 8 7 6 5 4 3 2 1 0)))))
