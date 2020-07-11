(ns rover.core
  (:require [clojure.string :as str ])
  (:gen-class))

(defrecord Program [w h rover inst])

(defn make-program [w h rover inst]
  (->Program w h rover inst))

(defrecord Rover [x y dir])

(defn make-rover
  "Create a rover from x, y, direction"
  [x y dir]
  (->Rover x y dir))

(defn rotate
  "Rotate rover to the left or right"
  [r to]
  (let [left {:N :W
              :W :S
              :S :E
              :E :N}
        right {:N :E
               :E :S
               :S :W
               :W :N}
        d (:dir r)]
    (if (= to :left)
      (assoc r :dir (get left d))
      (assoc r :dir (get right d)))))

(defn move-rover
  "Move a rover one unit towards its current orientation"
  [r]
  (let [d (:dir r)]
    (cond
      (= d :N) (update r :y inc)
      (= d :S) (update r :y dec)
      (= d :E) (update r :x inc)
      (= d :W) (update r :x dec))))

(defn rover-exec-instr
  "Execute a move or rotation instruction"
  [r i]
  (cond
    (= i :move) (move-rover r)
    (= i :left) (rotate r :left)
    (= i :right) (rotate r :right)))

(defn rover-exec-instrs
  "Execute a sequence of instructions"
  [r is]
  (reduce rover-exec-instr r is))

(defn parse-dir
  "Parse a direction for a string/character"
  [c]
  (cond
    (or (= c \N) (= c "N")) :N
    (or (= c \S) (= c "S")) :S
    (or (= c \E) (= c "E")) :E
    (or (= c \W) (= c "W")) :W))

(defn parse-inst
  "Parse an instruction from a string/character"
  [i]
  (cond
    (or (= i \L) (= i "L")) :left
    (or (= i \R) (= i "R")) :right
    (or (= i \M) (= i "M")) :move))

(defn parse-rover
  "Parse a rover description from a string"
  [s]
  (let [xs (str/split s #" ")
        x (read-string (nth xs 0))
        y (read-string (nth xs 1))
        d (parse-dir (nth xs 2))
        ]
    (make-rover x y d)))

(defn parse-program
  "Parse a single rover program from a list of lines"
  [lines]
  (let [size (str/split (nth lines 0) #" ")
        r (parse-rover (nth lines 1))
        is (map parse-inst (seq (nth lines 2)))
        w (read-string (nth size 0))
        h (read-string (nth size 1))]
    (make-program w h r is)))

(defn parse-programs
  "Parse one or more rover programs"
  [s]
  (let [lines (str/split-lines s)
        ps (partition 3 lines)]
    (map parse-program ps)))

(defn run-program
  "Run a program returning a list of rovers"
  [text]
  (let [ps (parse-programs text)
        exec-rover-instrs (fn [r is] (reduce rover-exec-instr r is))
        exec (fn [p] (exec-rover-instrs  (:rover p) (:inst p)))]
    (map exec ps)))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
