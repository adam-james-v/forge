(ns forge.delaunay
  (:require [clojure.set :as set]
            [forge.utils :as utils]))

;; https://gist.github.com/mutoo/5617691
(defn- circumscribe-tri
  [[[ax ay] [bx by] [cx cy]]]
  (let [A (- bx ax)
        B (- by ay)
        C (- cx ax)
        D (- cy ay)
        E (+ (* A (+ ax bx)) (* B (+ ay by)))
        F (+ (* C (+ ax cx)) (* D (+ ay cy)))
        G (* 2 (- (* A (- cy by)) (* B (- cx bx))))]
    (when (> (utils/abs G) 0.000001)
      (let [cx (/ (- (* D E) (* B F)) G)
            cy (/ (- (* A F) (* C E)) G)
            dx (- cx ax)
            dy (- cy ay)
            r  (+ (utils/pow dx 2) (utils/pow dy 2))]
        {:x cx :y cy :radius-squared r}))))

(defn- edges
  [pts]
  (partition 2 1 (conj (vec pts) (first pts))))

(defn- contains-pt?
  [{:keys [x y radius-squared]} [px py]]
  (let [distance-squared (utils/distance-squared [x y] [px py])]
    (< distance-squared radius-squared)))

(defn- outer-edges
  [tris]
  (let [all-edges (mapcat edges tris)
        matches (fn [edge] (filter #{edge (reverse edge)} all-edges))
        appears-once (fn [edge] (= (count (matches edge)) 1))]
    (filter appears-once all-edges)))

(defn- make-new-tris
  [containers pt]
  (->> containers
       outer-edges
       (map (fn [[p1 p2]] [p1 p2 pt]))
       set))

(defn- add-pt-to-tris
  [tris pt]
  (let [containers (filter #(contains-pt? (circumscribe-tri %) pt) tris)
        new-tris (make-new-tris containers pt)]
    (set/union (set/difference tris containers) new-tris)))

;; http://paulbourke.net/papers/triangulate/
(defn triangulate
  [pts]
  (let [pts (map (fn [[x y]] [(float x) (float y)]) pts)
        pt-indices (zipmap pts (range 0 (count pts)))
        [bl br tr tl] (map #(utils/v* % [2 2]) (utils/bounds-of-pts pts))
        initial #{[tl tr bl] [bl tr br]}
        with-bounds (reduce add-pt-to-tris initial pts)
        tris (remove #(some #{tl tr bl br} %) with-bounds)
        tri-indices (fn [tri] (mapv #(get pt-indices %) tri))]
    {:pts pts
     :tris tris
     :tri-indices (map tri-indices tris)
     :edges (distinct (mapcat edges tris))}))
