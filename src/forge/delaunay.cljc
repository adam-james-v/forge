(ns forge.delaunay
  (:require [clojure.set]))

;; move these to geom ns?
(def abs #?(:clj #(Math/abs %)
            :cljs #(js/Math.abs %)))

(def pow #?(:clj #(Math/pow %1 %2)
            :cljs #(js/Math/pow %1 %2)))

(defn rand-int-pt
  ([n]
   (rand-int-pt n n))

  ([nx ny]
   [(rand-int nx) (rand-int ny)]))

(defn random-points
  ([n]
   (random-points n n))

  ([nx ny]
   (repeatedly #(rand-int-pt nx ny))))

(defn grid-points
  [cell-x cell-y n-xcells n-ycells]
  (for [nx (range n-xcells)
        ny (range n-ycells)]
    [(* cell-x nx) (* cell-y ny)]))

;; https://gist.github.com/mutoo/5617691
(defn circumscribe-triangle [[[ax ay] [bx by] [cx cy]]]
  (let [A (- bx ax)
        B (- by ay)
        C (- cx ax)
        D (- cy ay)
        E (+ (* A (+ ax bx)) (* B (+ ay by)))
        F (+ (* C (+ ax cx)) (* D (+ ay cy)))
        G (* 2 (- (* A (- cy by)) (* B (- cx bx))))]
    (when (> (abs G) 0.000001)
      (let [cx (/ (- (* D E) (* B F)) G)
            cy (/ (- (* A F) (* C E)) G)
            dx (- cx ax)
            dy (- cy ay)
            r  (+ (pow dx 2) (pow dy 2))]
        {:x cx :y cy :radius-squared r}))))

(defn edges [[p1 p2 p3]] [[p1 p2] [p2 p3] [p3 p1]])

(defn contains-point? [{:keys [x y radius-squared]} [px py]]
  (let [distance-squared (+ (pow (- x px) 2) (pow (- y py) 2))]
    (< distance-squared radius-squared)))

(defn outer-edges [triangles]
  (let [all-edges    (mapcat edges triangles)
        matches      (fn [edge] (filter #{edge (reverse edge)} all-edges))
        appears-once (fn [edge] (= (count (matches edge)) 1))]
    (filter appears-once all-edges)))

(defn make-new-triangles [containers point]
  (->> containers
       outer-edges
       (map (fn [[p1 p2]] [p1 p2 point]))
       set))

(defn add-point-to-triangles [triangles point]
  (let [containers    (filter #(contains-point? (circumscribe-triangle %) point) triangles)
        new-triangles (make-new-triangles containers point)]
    (clojure.set/union (clojure.set/difference triangles containers) new-triangles)))

(defn bounds [points]
  (let [minx (->> points (map first) (apply min) (+ -1000))
        maxx (->> points (map first) (apply max) (+ 1000))
        miny (->> points (map second) (apply min) (+ -1000))
        maxy (->> points (map second) (apply max) (+ 1000))]
    [[minx maxy] [maxx maxy] [minx miny] [maxx miny]]))

;; http://paulbourke.net/papers/triangulate/
(defn triangulate [points]
  (let [points (map (fn [[x y]] [(float x) (float y)]) points)
        [tl tr bl br] (bounds points)
        initial #{[tl tr bl] [bl tr br]}
        with-bounds (reduce add-point-to-triangles initial points)
        triangles (remove #(some #{tl tr bl br} %) with-bounds)]
    {:points points
     :triangles triangles
     :edges (distinct (mapcat edges triangles))}))
