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

(deftest move-rover-test
  (testing "move the rover in the direction it is pointed at"
    (are [x y] (= x y)
         (move-rover (make-rover 1 1 :N)) (make-rover 1 2 :N)
         (move-rover (make-rover 1 1 :S)) (make-rover 1 0 :S)
         (move-rover (make-rover 1 1 :E)) (make-rover 2 1 :E)
         (move-rover (make-rover 1 1 :W)) (make-rover 0 1 :W))))

(deftest rover-exec-test
  (testing "executing a rover instruction"
    (are [x y] (= x y)
         (rover-exec-instr (make-rover 1 1 :N) :left) (make-rover 1 1 :W)
         (rover-exec-instr (make-rover 1 1 :N) :right) (make-rover 1 1 :E)
         (rover-exec-instr (make-rover 1 1 :N) :move) (make-rover 1 2 :N))))

(deftest rover-exec-instrs-test
  (testing "executing multiple rover instructions"
    (are [x y] (= x y)
         (rover-exec-instrs (make-rover 1 1 :N) [:move, :move, :move]) (make-rover 1 4 :N))
         (rover-exec-instrs (make-rover 1 1 :N) [:left, :left, :left, :left]) (make-rover 1 1 :N)
         (rover-exec-instrs (make-rover 1 2 :N) [:left, :move, :left, :move, :left, :move, :left, :move, :move]) (make-rover 1 3 :N)))
