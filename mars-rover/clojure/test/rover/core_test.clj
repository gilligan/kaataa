(ns rover.core-test
  (:require [clojure.test :refer :all]
            [rover.core :refer :all]))

(deftest a-test
  (testing "FIXME, I fail."
    (is (= 1 1))))

(deftest rotate-rover
  (testing "that-the-rover-rotates-right" 
    (is (= 
          (mk_rover 1 1 N)
          (rotate_left (mk_rover 1 1 W))
          ))))
