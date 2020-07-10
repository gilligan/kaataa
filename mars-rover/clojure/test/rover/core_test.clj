(ns rover.core-test
  (:require [clojure.test :refer :all]
            [rover.core :refer :all]))

(deftest make-rover-test
  (testing "make-rover can construct a Rover"
    (is (= (make-rover 1 1 :S) (->Rover 1 1 :S)))))

(deftest rotate-left-test
  (testing "can rotate rover left"
    (is (= (rotate-left (make-rover 1 1 :N)) (make-rover 1 1 :W)))))

(deftest left-rotation-test
  (testing "rotating the rover left"
    (are [x y] (= (rotate-left x) y)
         (make-rover 1 1 :N) (make-rover 1 1 :W)
         (make-rover 1 1 :W) (make-rover 1 1 :S)
         (make-rover 1 1 :S) (make-rover 1 1 :E)
         (make-rover 1 1 :E) (make-rover 1 1 :N))))

(deftest right-rotation-test
  (testing "rotating the rover right"
    (are [x y] (= (rotate-right x) y)
         (make-rover 1 1 :N) (make-rover 1 1 :E)
         (make-rover 1 1 :E) (make-rover 1 1 :S)
         (make-rover 1 1 :S) (make-rover 1 1 :W)
         (make-rover 1 1 :W) (make-rover 1 1 :N))))
