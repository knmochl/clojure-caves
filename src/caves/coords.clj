(ns caves.coords
  (:use [caves.utils :only [abs]]))

(def directions
  {:w  [-1 0 0]
   :e  [1 0 0]
   :n  [0 -1 0]
   :s  [0 1 0]
   :nw [-1 -1 0]
   :ne [1 -1 0]
   :sw [-1 1 0]
   :se [1 1 0]})

(defn offset-coords
  "Offset the starting coordinate by the given amount, return the resulting coord"
  [[x y z] [dx dy dz]]
  [(+ x dx) (+ y dy) (+ z dz)])

(defn dir-to-offset
  "Convert a direction to the offset for moving 1 in that direction"
  [dir]
  (directions dir))

(defn destination-coords
  "Take an origin's coords and a direction and return the destination's coords"
  [origin dir]
  (offset-coords origin (dir-to-offset dir)))

(defn neighbors
  "Return the coords of all neighboring squares of the given coord"
  [origin]
  (shuffle (map offset-coords (vals directions) (repeat origin))))

(defn radial-distance
  "Return the radial distance between two points"
  [[x1 y1 z1] [x2 y2 z2]]
  (max (abs (- x1 x2))
       (abs (- y1 y2))))
