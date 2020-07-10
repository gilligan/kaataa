(ns rover.core
  (:require [clojure.string :as str ])
  (:gen-class))

(defrecord Program [w h rover inst])

(defn make-program [w h rover inst]
  (->Program w h rover inst))

(defrecord Rover [x y dir])

(defn make-rover [x y dir]
  (->Rover x y dir))

(defn rotate-left [r]
  (let [d (:dir r)]
    (cond
      (= d :N) (assoc r :dir :W)
      (= d :W) (assoc r :dir :S)
      (= d :S) (assoc r :dir :E)
      (= d :E) (assoc r :dir :N))))

(defn rotate-right [r]
  (let [d (:dir r)]
    (cond
      (= d :N) (assoc r :dir :E)
      (= d :E) (assoc r :dir :S)
      (= d :S) (assoc r :dir :W)
      (= d :W) (assoc r :dir :N))))

(defn move-rover [r]
  (let [d (:dir r)]
    (cond
      (= d :N) (update r :y inc)
      (= d :S) (update r :y dec)
      (= d :E) (update r :x inc)
      (= d :W) (update r :x dec))))

(defn rover-exec-instr [r i]
  (cond
    (= i :move) (move-rover r)
    (= i :left) (rotate-left r)
    (= i :right) (rotate-right r)))

(defn rover-exec-instrs [r is]
  (reduce rover-exec-instr r is))

(defn parse-dir [c]
  (cond
    (or (= c \N) (= c "N")) :N
    (or (= c \S) (= c "S")) :S
    (or (= c \E) (= c "E")) :E
    (or (= c \W) (= c "W")) :W))

(defn parse-inst [i]
  (cond
    (or (= i \L) (= i "L")) :left
    (or (= i \R) (= i "R")) :right
    (or (= i \M) (= i "M")) :move))

(defn parse-rover [s]
  (let [xs (str/split s #" ")]
    (make-rover (read-string (nth xs 0)) (read-string (nth xs 1)) (parse-dir (nth xs 2)))))

(defn parse-program [lines]
  (let [size (str/split (nth lines 0) #" ")
        r (parse-rover (nth lines 1))
        is (map parse-inst (seq (nth lines 2)))
        w (read-string (nth size 0))
        h (read-string (nth size 1))]
    (make-program w h r is)))

(defn parse-programs [s]
  (let [lines (str/split-lines s)
        ps (partition 3 lines)]
    (map parse-program ps)))

(defn run-program [text]
  (let [ps (parse-programs text)]
    (map (fn [p] (rover-exec-instrs (:rover p) (:inst p))) ps)))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
