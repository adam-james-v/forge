(ns forge.geom
  (:require [forge.utils :as utils]
            [forge.delaunay :as delaunay]
            [clojure.string :as str]
            [same :refer [ish? zeroish?]]))

(defn triangle-area
  "compute the area of a triangle defined by three points"
  [a b c]
  ;; use Heron's formula
  (let [la (utils/distance b c)
        lb (utils/distance a c)
        lc (utils/distance a b)
        s (/ (+ la lb lc) 2)]
    (Math/sqrt (* s (- s la) (- s lb) (- s lc)))))

(defn polygon-area
  [pts]
  (let [tris (:triangles (delaunay/triangulate pts))]
    (->> pts
         (delaunay/triangulate)
         (:triangles)
         (map #(apply triangle-area %))
         (reduce +))))

(defn bb-corners-2d
  [pts]
  (let [xs (map first pts)
        ys (map last pts)
        xmax (apply max xs)
        ymax (apply max ys)
        xmin (apply min xs)
        ymin (apply min ys)]
    [[xmin ymin]
     [xmax ymax]]))

(defn bb-center-2d
  [pts]
  (let [xs (map first pts)
        ys (map last pts)
        xmax (apply max xs)
        ymax (apply max ys)
        xmin (apply min xs)
        ymin (apply min ys)]
    [(+ (/ (- xmax xmin) 2.0) xmin)
     (+ (/ (- ymax ymin) 2.0) ymin)]))

(defn midpoint
  [pts]
  (let [axes (count (first pts))
        splits (for [axis (range 0 axes)]
                 (map #(nth % axis) pts))]
    (mapv #(apply utils/average %) splits)))

(defn bounding-box-corners
  [shape]
  (let [xs (map #(nth % 0)
                (sort-by #(nth % 0) (:vertices shape)))
        ys (map #(nth % 1)
                (sort-by #(nth % 1) (:vertices shape)))
        zs (map #(nth % 2)
                (sort-by #(nth % 2) (:vertices shape)))]
    [[(first xs) (first ys) (first zs)]
     [(last xs) (last ys) (last zs)]]))

(declare polygon)
(declare extrude)
(defn bounding-volume-proto
  [shape]
  (let [[a z] (bounding-box-corners shape)
        b [(first z) (second a) (last a)]
        c [(first z) (second z) (last a)]
        d [(first a) (second z) (last a)]
        s1 (polygon [a b c d])
        h (Math/abs (- (last z) (last a)))]
    (extrude s1 h)))

(defn estimate-path-length
  [path [t1 t2] step]
  (let [samples (range t1 (+ t2 step) step)
        pts (partition 2 1 (map path samples))]
    (reduce + (map (partial apply utils/distance) pts))))

;; this breaks somewhat often. Consider a case where the match is
;; a very tiny difference in parameter space. It's easy to pass over it
;; there's surely a more refined approach, perhaps a combination of several algorithms?
(defn close?
  [p1 p2]
  (let [[x1 y1 z1] p1
        [x2 y2 z2] p2]
    (and (utils/nearly? (+ 1 (Math/abs (- x1 x2))) 1.0)
         (utils/nearly? (+ 1 (Math/abs (- y1 y2))) 1.0)
         (utils/nearly? (+ 1 (Math/abs (- z1 z2))) 1.0))))

(defn estimate-parameter
  [f pt step]
  (let [[x y z] pt
        samples (into [] (range 0 (+ 1 step) step))
        pts (mapv f samples)
        close? (partial close? pt)]
    (get samples
         (count (take-while #(not (= (first (filter close? pts)) %)) pts)))))

(defn line-intersection
  [[a b] [c d]]
  (let [[ax ay] a
        [bx by] b
        [cx cy] c
        [dx dy] d
        xdiff [(- ax bx) (- cx dx)]
        ydiff [(- ay by) (- cy dy)]
        div (utils/determinant-2d xdiff ydiff)]
    (when (not (zeroish? (Math/abs div))) 
      (let [d [(utils/determinant-2d a b) (utils/determinant-2d c d)]
            x (/ (utils/determinant-2d d xdiff) div)
            y (/ (utils/determinant-2d d ydiff) div)]
        [x y]))))

(defn line-segment-intersection
  [[a b] [c d]]
  (let [pt (line-intersection [a b] [c d])]
    (when (and pt
               (utils/on-line? (utils/add-z pt) (mapv utils/add-z [a b]))
               (utils/on-line? (utils/add-z pt) (mapv utils/add-z [c d])))
      pt)))

(defn identical-polygons?
  [pga pgb]
  (= (into #{} pga)
     (into #{} pgb)))

(defn- close-pts
  [pts]
  (vec (take (inc (count pts)) (cycle pts))))

(defn polygon-intersection
  [pga pgb]
  (when (not (identical-polygons? pga pgb))
    (let [lines-a (partition 2 1 (close-pts pga))
          lines-b (partition 2 1 (close-pts pgb))
          s (for [la lines-a
                  lb lines-b]
              (line-segment-intersection la lb))]
      (->> s
           (filter (complement nil?))
           (into #{})
           (vec)))))

(defn pt-inside-convex?
  [pts pt]
  (let [m (mapv float (midpoint pts))
        xs (polygon-intersection pts [m pt])]
    ;; pt inside when count intersection = 0
    ;; pt inside when intersection = pt
    (or (= 0 (count xs))
        (utils/all-nearly? (first xs) pt))))

(defn pt-inside-convex-strict?
  [pts pt]
  (let [m (mapv float (midpoint pts))
        xs (polygon-intersection pts [m pt])]
    (utils/all-nearly? (first xs) pt)))

(defn quadrant
  [x y]
  (let [mask [(pos? x) (pos? y)]]
    (cond (= mask [ true  true]) 1
          (= mask [false  true]) 2
          (= mask [false false]) 3
          (= mask [ true false]) 4)))

(defn angle-from-pts2 
  "Angle following RHR from p3p2 to p1p2"
  [p1 p2 p3]
  (let [[ax ay] (utils/v- p1 p2)
        [bx by] (utils/v- p3 p2)
        qa (quadrant ax ay)
        qb (quadrant bx by)
        theta-a (utils/to-deg (Math/atan2 ay ax))
        theta-b (utils/to-deg (Math/atan2 by bx))]
    (cond 
      ;; la and lb both in same quadrant, always < 90deg
      (= qa qb) (- theta-b theta-a)
      ;; qa greater than qb and 2 away [4 2] or [3 1]
      (= 2 (- qa qb)) (- 360 theta-b theta-a)
      ;; qb greater than qa and 2 away [2 4] or [1 3]
      (= 2 (- qb qa)) (- theta-b theta-a)
      (or
       (= [qa qb] [2 1])
       (= [qa qb] [4 3])) (- 180 (Math/abs theta-b) (Math/abs theta-a))
      (or
       (= [qa qb] [1 2])
       (= [qa qb] [3 4])) (+ 180 (Math/abs theta-b) (Math/abs theta-a))
      (or
       (= [qa qb] [3 2])
       (= [qa qb] [1 4])) (+ (Math/abs theta-b) (Math/abs theta-a))
      (or
       (= [qa qb] [2 3])
       (= [qa qb] [4 1])) (+ 180 (Math/abs theta-b) (Math/abs theta-a)))))

(defn acute?
  [p1 p2 p3]
  (< (angle-from-pts2 p1 p2 p3) 180.0))      

(defn line?
  [[a b c]]
  (utils/on-line-inf? (utils/add-z a) (mapv utils/add-z [b c])))

(defn contains-value?
  [coll val]
  (when (some #{val} coll) true))

(defn vecdiff
  [va vb]
  (into [] (filter (complement (partial contains-value? vb)) va))) 

(defn simplify-segments
  [pts]
  (let [triples (partition 3 1 (take (+ 2 (count pts)) (cycle pts)))
        removable (map second (filter line? triples))]
    (vecdiff pts removable)))

;; can I use reduce instead?
;; other recursion scheme?

(defn clip-ears
  ([pts]
   (clip-ears pts []))
  
  ([pts acc]
   (let [spts (simplify-segments pts)
         tris (->> (cycle spts)
                   (partition 3 1)
                   (take (count spts))
                   (filter #(apply acute? %))
                   (mapv #(into [] %)))
         tri (first (for [tri tris]
                      (let [xpts (remove (into #{} tri) (into #{} spts))
                            clear (= 0 (count (filter #(pt-inside-convex? tri %) xpts)))]
                        (when clear tri))))
         npts (if tri 
                (->> spts
                     (filter (complement #{(second tri)}))
                     (into []))
                (->> spts
                     (cycle)
                     (drop 1)
                     (take (count spts))
                     (into [])))
         acc (if tri (conj acc tri) acc)]
     (if (> (count npts) 2)
       (recur npts acc)
       acc))))

(defn pt-inside?
  [pts pt]
  (let [tris (clip-ears pts)]
    (->> tris
         (map #(pt-inside-convex? % pt))
         (filter true?)
         (empty?)
         (not))))

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

(defn polygon->lines 
  [pg]
  (->> pg
       (cycle)
       (take (inc (count pg)))
       (partition 2 1)))

(defn endpoint?
  [l pt]
  (or (= (first l) pt)
      (= (second l) pt)))

(defn on-perimeter?
  [pg pt]
  (let [pt (utils/add-z pt)
        pg (mapv utils/add-z pg)
        lines (polygon->lines pg)]
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

(defn polygon-union
  [pga pgb]
  (let [xs (polygon-intersection pga pgb)
        ;; trim lines at intersection points
        ls (apply concat 
                  (for [l (mapv vec (mapcat polygon->lines [pga pgb]))]
                    (let [trims (->> #_(map #(trim-at-pts l [%]) xs)
                                     [(trim-at-pts l xs)]
                                     (filter #(not (nil? %))))]
                      (if (> (count trims) 0)
                        (apply concat trims)
                        [l]))))
        ;; remove degenerate lines (= pta ptb)
        ls (filter #(not (= (first %) (second %))) ls)
        ;; get lines that are not in polygon a
        a (filter #(not (pt-inside? pga (midpoint %))) ls)
        ;; get lines that are not in polygon b
        b (filter #(not (pt-inside? pgb (midpoint %))) ls)
        ;; get lines with midpoints on both perimeters
        c (filter #(and (on-perimeter? pga (midpoint %))
                        (on-perimeter? pgb (midpoint %))) ls)]
    (->> (concat a b c)
         (filter (complement nil?))
         (into #{})
         (order-lines))))

(defn polygon-difference
  "Polygon B Cuts Polygon A"
  [pga pgb]
  (let [xs (polygon-intersection pga pgb)
        ;; trim lines at intersection points
        ls (apply concat 
                  (for [l (mapv vec (mapcat polygon->lines [pga pgb]))]
                    (let [trims (->> #_(map #(trim-at-pts l [%]) xs)
                                     [(trim-at-pts l xs)]
                                     (filter #(not (nil? %))))]
                      (if (> (count trims) 0)
                        (apply concat trims)
                        [l]))))
        ;; remove degenerate lines (= pta ptb)
        ls (filter #(not (= (first %) (second %))) ls)
        ;; get lines that are in polygon a
        a (filter #(pt-inside? pga (midpoint %)) ls)
        ;; get lines that are in polygon b
        b (filter #(pt-inside? pgb (midpoint %)) ls)
        ;; get lines with midpoints on both perimeters
        c (filter #(and (on-perimeter? pga (midpoint %))
                        (on-perimeter? pgb (midpoint %))) ls)]
    (->> (concat a b c)
         (filter (complement nil?))
         (into #{})
         (order-lines))))

(defn offset-edge
  [[a b] d]
  (let [p (utils/perpendicular-2d (utils/v- b a))
        pd (utils/v* (utils/normalize p) (repeat (- d)))
        xa (utils/v+ a pd)
        xb (utils/v+ b pd)]
    [xa xb]))

(defn cycle-pairs
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

(defn offset
  [pts d]
  (let [edges (cycle-pairs pts)
        opts (mapcat #(offset-edge % d) edges)
        oedges (every-other (cycle-pairs opts))
        edge-pairs (cycle-pairs oedges)]
    (wrap-list-once (map #(apply line-intersection %) edge-pairs))))
