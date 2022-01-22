(ns forge.geom
  (:require [forge.utils :as utils]
            [forge.clip-ears :as clip-ears]
            [clojure.string :as str]
            [same :refer [ish? zeroish?]]))

(defn- triangle-area
  "compute the area of a triangle defined by three points"
  [[a b c]]
  ;; use Heron's formula
  (let [la (utils/distance b c)
        lb (utils/distance a c)
        lc (utils/distance a b)
        s (/ (+ la lb lc) 2)]
    (Math/sqrt ^double (* s (- s la) (- s lb) (- s lc)))))

(defn area
  [pts]
  (let [tris (:triangles (clip-ears/triangulate pts))]
    (->> pts
         clip-ears/triangulate
         :tris
         (map triangle-area)
         (reduce +))))

(defn line-intersection
  [[a b] [c d]]
  (let [[ax ay] a
        [bx by] b
        [cx cy] c
        [dx dy] d
        xdiff [(- ax bx) (- cx dx)]
        ydiff [(- ay by) (- cy dy)]
        div (utils/determinant xdiff ydiff)]
    (when-not (zeroish? (utils/abs div))
      (let [d [(utils/determinant a b) (utils/determinant c d)]
            x (/ (utils/determinant d xdiff) div)
            y (/ (utils/determinant d ydiff) div)]
        (mapv double [x y])))))

(defn line-segment-intersection
  [[a b] [c d]]
  (let [pt (line-intersection [a b] [c d])]
    (when
        (and pt
             (utils/on-line? (utils/add-z pt) (mapv utils/add-z [a b]))
             (utils/on-line? (utils/add-z pt) (mapv utils/add-z [c d])))
      pt)))

(defn- identical-polygon?
  [pga pgb]
  (= (into #{} pga)
     (into #{} pgb)))

(defn polygon-intersection
  [pga pgb]
  (when-not (identical-polygon? pga pgb)
    (let [lines-a (utils/edges pga)
          lines-b (utils/edges pgb)
          s (for [la lines-a
                  lb lines-b]
              (line-segment-intersection la lb))]
      (->> s
           (remove nil?)
           distinct
           #_vec))))

#_(let [pga (utils/regular-polygon-pts 120 200)
      #_[[0 0] [200 0] [0 200]]
      pgb (map #(utils/v+ % [50.0 100.0]) (utils/regular-polygon-pts 120 200)
               #_[[-100.0 100.0] [150.0 100.0] [150.0 -100.0]])
      xs (polygon-intersection pga pgb)]
  (svg-clj.tools/cider-show
   (svg-clj.elements/g
    (when (< 0 (count xs))
      (map #(-> (svg-clj.elements/circle 3)
                (svg-clj.transforms/translate %)
                (svg-clj.transforms/style {:fill "limegreen"})) xs))
    (-> (svg-clj.elements/polygon pga)
        (svg-clj.transforms/style {:fill "none" :stroke "blue"}))
    (-> (svg-clj.elements/polygon pgb)
        (svg-clj.transforms/style {:fill "none" :stroke "blue"})))))

(defn trim
  "Trim line a using line b."
  [la lb]
  (let [x (line-segment-intersection la lb)]
    (when x
      [[(first la) x]
       [x (second la)]])))

(defn trim-at-pt
  [[a b] pt]
  (when (and
         (not= a pt)
         (not= b pt)
         (utils/on-line? (utils/add-z (mapv float pt)) (mapv utils/add-z [a b])))
    [[a pt]
     [pt b]]))

(defn trim-at-pts
  [[a b] pts]
  (let [pts (filter #(utils/on-line? % (mapv utils/add-z [a b])) (mapv utils/add-z pts))]
    (when (first pts)
      (->> pts
           (sort-by (partial utils/distance a))
           (mapv #(into [] (drop-last %)))
           (concat [a])
           (apply vector)
           (#(conj % b))
           (partition 2 1)
           (mapv vec)))))

#_(let [la [[0.0 0.0] [200.0 300.0]]
      lb [[0.0 120.0] [250.0 80.0]]
      [lc ld] (trim la lb)
      xs [(line-intersection la lb)]]
  (svg-clj.tools/cider-show
   (svg-clj.elements/g
    (when (< 0 (count xs))
      (map #(-> (svg-clj.elements/circle 3)
                (svg-clj.transforms/translate %)
                (svg-clj.transforms/style {:fill "limegreen"})) xs))
    (-> (svg-clj.elements/polyline la)
        (svg-clj.transforms/style {:fill "none" :stroke "blue"}))
    (-> (svg-clj.elements/polyline lb)
        (svg-clj.transforms/style {:fill "none" :stroke "blue"}))
    (-> (svg-clj.elements/polyline lc)
        (svg-clj.transforms/style {:fill "none" :stroke "red"}))
    (-> (svg-clj.elements/polyline ld)
        (svg-clj.transforms/style {:fill "none" :stroke "green"})))))

(defn endpoint?
  [l pt]
  (or (= (first l) pt)
      (= (second l) pt)))

(defn on-perimeter?
  [pg pt]
  (let [lines (utils/edges pg)]
    (> (count (filter (partial utils/on-line? pt) lines)) 0)))

(defn order-lines
  ([lines]
   (let [start (first (sort-by (comp second first) lines))]
     (order-lines lines start [start])))

  ([lines [_ b] sorted]
   (let [next (first (filter #(= b (first %)) lines))]
     (if (= (count lines) (count sorted))
       (mapv first sorted)
       (recur lines next (conj sorted next))))))

(defn pt-inside?
  [pg pt]
  (let [tris (:tris (clip-ears/triangulate pg))]
    (some true? (concat
                 (map #(utils/pt-on-perimeter? % pt) tris)
                 (map #(utils/pt-inside? % pt) tris)))))

(defn overlap?
  [pga pgb]
  (some true? (map #(pt-inside? pga %) pgb)))

(defn polygon-difference
  [pga pgb]
  (when (overlap? pga pgb)
    (let [in-b (filter #(pt-inside? pgb %) pga)
          in-a (filter #(pt-inside? pga %) pgb)
          rem-a (->> pga
                     (partition-by (set in-b))
                     (remove #((set in-b) (first %)))
                     reverse
                     (apply concat))
          rem-b (->> pgb
                     (partition-by (set in-a))
                     (remove #((set in-a) (first %)))
                     reverse
                     (apply concat))
          crossing-1 [[(last rem-a) (first in-b)]
                      [(last in-a) (first rem-b)]]
          crossing-2 [[(first rem-a) (last in-b)]
                      [(first in-a) (last rem-b)]]
          [i1 i2] (map #(apply line-segment-intersection %) [crossing-1 crossing-2])]
      (concat rem-a [i1] (reverse in-a) [i2]))))

(defn polygon-union
  [pga pgb]
  (when (overlap? pga pgb)
    (let [in-b (filter #(pt-inside? pgb %) pga)
          in-a (filter #(pt-inside? pga %) pgb)
          rem-a (->> pga
                     (partition-by (set in-b))
                     (remove #((set in-b) (first %)))
                     reverse
                     (apply concat))
          rem-b (->> pgb
                     (partition-by (set in-a))
                     (remove #((set in-a) (first %)))
                     reverse
                     (apply concat))
          crossing-1 [[(last rem-a) (first in-b)]
                      [(last in-a) (first rem-b)]]
          crossing-2 [[(first rem-a) (last in-b)]
                      [(first in-a) (last rem-b)]]
          [i1 i2] (map #(apply line-segment-intersection %) [crossing-1 crossing-2])]
      (concat rem-a [i1] rem-b [i2]))))

(defn offset-edge
  [[a b] d]
  (let [p (utils/perpendicular (utils/v- b a))
        pd (utils/v* (utils/normalize p) (repeat (- d)))
        xa (utils/v+ a pd)
        xb (utils/v+ b pd)]
    [xa xb]))

(defn- cycle-pairs
  [pts]
  (let [n (count pts)]
    (vec (take n (partition 2 1 (cycle pts))))))

(defn every-other
  [v]
  (let [n (count v)]
    (map #(get v %) (filter even? (range n)))))

(defn wrap-list-once
  [s]
  (conj (drop-last s) (last s)))

(defn offset-pts
  [pts d]
  (let [edges (cycle-pairs pts)
        opts (mapcat #(offset-edge % d) edges)
        oedges (every-other (cycle-pairs opts))
        edge-pairs (cycle-pairs oedges)]
    (wrap-list-once (map #(apply line-intersection %) edge-pairs))))

(defn hull
  ([pts] (hull [{:pt (first (sort-by first pts))}] pts))
  ([acc pts]
   (if (or
        ;; stop the process if acc grows larger than the pts count
        (> (count acc) (count pts))
        ;; *should* always end where the last added point closes the poly
        (and (< 1 (count acc))
             (= (:pt (first acc)) (:pt (last acc)))))
     (map :pt (drop-last acc))
     (let [prev (:pt (last acc))
           dir (if (= 1 (count acc))
                 (utils/v+ [0 1] prev)
                 (:pt (last (drop-last acc))))
           f (fn [pt]
               (let [a (when (= 3 (count (into #{} [dir prev pt])))
                         (utils/angle-from-pts dir prev pt))]
                 {:pt pt :angle a}))
           sorted (->> (map f pts)
                       (remove #(nil? (:angle %)))
                       (sort-by #(utils/abs (- (:angle %) 180))))]
       (recur (conj acc (first sorted)) pts)))))

(defn nested-hull
  ([pts] (nested-hull [] pts))
  ([acc pts]
   (if (> 3 (count pts))
     acc
     (let [hull (hull pts)
           npts (remove (set hull) pts)]
       (recur (conj acc hull) npts)))))
