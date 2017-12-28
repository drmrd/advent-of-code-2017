(ns advent-of-code.day7-test
  (:require [advent-of-code.day7 :refer :all]
            [clojure.test :refer :all]
            [clojure.string :as str]))

(def sample-graph (->> ["padx (45) -> pbga, havc, qoyq"
                        "tknk (41) -> ugml, padx, fwft"
                        "ugml (68) -> gyxo, ebii, jptl"
                        "fwft (72) -> ktlj, cntj, xhth"
                        "gyxo (61)" "cntj (57)" "jptl (61)" "pbga (66)"
                        "xhth (57)" "ebii (61)" "havc (66)" "ktlj (57)"
                        "qoyq (66)"]
                       (str/join "\n")
                       (program-tree)))

(deftest test-bottom-program
  (testing "The right root program is found in the sample tower"
    (is (= (bottom-program sample-graph) :tknk))))

(deftest test-corrected-weight
  (testing "The unbalanced weight is corrected properly in the sample tower"
    (is (= (corrected-weight sample-graph) 60))))
