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

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
