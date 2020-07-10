(ns rover.core
  (:gen-class))

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

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
