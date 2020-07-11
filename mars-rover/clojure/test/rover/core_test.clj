(ns rover.core-test
  (:require [clojure.test :refer :all]
            [rover.core :refer :all]))

(deftest make-rover-test
  (testing "make-rover can construct a Rover"
    (is (= (make-rover 1 1 :S) (->Rover 1 1 :S)))))

(deftest left-rotation-test
  (testing "rotating the rover left"
    (are [x y] (= (rotate x :left) y)
         (make-rover 1 1 :N) (make-rover 1 1 :W)
         (make-rover 1 1 :W) (make-rover 1 1 :S)
         (make-rover 1 1 :S) (make-rover 1 1 :E)
         (make-rover 1 1 :E) (make-rover 1 1 :N))))

(deftest right-rotation-test
  (testing "rotating the rover right"
    (are [x y] (= (rotate x :right) y)
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

(deftest parse-program-test
  (testing "parsing a program text"
    (are [x y] (= x y)
         (parse-program ["5 5", "1 2 N", "LMLMLMLMM"]) (make-program 5 5 (make-rover 1 2 :N) [:left, :move, :left, :move, :left, :move, :left, :move, :move]))))

(deftest run-program-test
  (testing "running a full program"
    (are [x y] (= x y)
         (run-program "5 5\n1 2 N\nLMLMLMLMM") [(make-rover 1 3 :N)]
         (run-program "5 5\n1 2 N\nLMLMLMLMM\n5 5\n1 2 N\nLMLMLMLMM") [(make-rover 1 3 :N) (make-rover 1 3 :N)])))

(deftest parse-dir-test
  (testing "parsing directions"
    (are [x y] (= x y)
         (parse-dir \N) :N
         (parse-dir \S) :S
         (parse-dir \W) :W
         (parse-dir \E) :E)))

(deftest parse-inst-test
  (testing "parsing instructions"
    (are [x y] (= x y)
         (parse-inst "L") :left
         (parse-inst "R") :right
         (parse-inst "M") :move)))

(deftest parse-rover-test
  (testing "parse rover"
    (are [x y] (= x y)
         (parse-rover "1 2 N") (make-rover 1 2 :N))))
