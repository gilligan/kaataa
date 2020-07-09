(ns rover.core
  (:gen-class))


(def N :N)
(def E :E)
(def S :S)
(def W :W)

(defstruct Rover (:x :y :orientation))
(defn mk_rover [x y o] (struct-map Rover
                                   :x x
                                   :y y
                                   :orientation o
                                   ))

(defn rotate_left [rover] (assoc-in rover [:orientation] W))


(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
