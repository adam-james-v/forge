(ns forge.brep.curves
  (:require [clojure.string :as str]
            [forge.utils :as utils]
            [forge.geom :as geom]
            [forge.delaunay :as delaunay]
            [forge.clip-ears :as clip-ears]
            [same :refer [ish? zeroish?]]))

(defn check-parametric
  [f]
  (let [fdata (try (f) (catch #?(:cljs :default :clj Exception) e))
        [f0 f05 f1] (map f [0 0.5 1])
        t0 (if (seqable? f0) f0 [f0])
        t05 (if (seqable? f05) f05 [f05])
        t1 (if (seqable? f1) f1 [f1])
        dim (count t05)
        required [:fn :input :vertex-params :length :origin]
        keys-pred (every? #(contains? fdata %) required)
        t0-pred (and t0 (= (count t0) dim) (every? number? t0))
        t1-pred (and t1 (= (count t1) dim) (every? number? t1))
        missing (when-not keys-pred (remove (set (keys fdata)) (set required)))
        result {:dimension dim
                :data fdata
                :valid-data keys-pred
                :valid-t0 t0-pred
                :valid-t1 t1-pred}]
    (cond-> result
      missing       (assoc-in [:error :missing] missing)
      (not fdata)   (assoc-in [:error :invalid-0-arity] fdata)
      (not t0-pred) (assoc-in [:error :invalid-t0] t0)
      (not t1-pred) (assoc-in [:error :invalid-t1] t1))))

(defn valid-parametric?
  [f]
  (nil? (:error (check-parametric f))))

(defn arc-length
  ([curve] (arc-length curve 0 1))
  ([curve t] (arc-length curve 0 t))
  ([curve ta tb]
   (let [seg 13500
         start (/ (* ta seg) seg)
         end   (/ (inc (* tb seg)) seg)]
     (->> (range start end (/ 1 seg))
          (map curve)
          (partition 2 1)
          (map #(apply utils/distance %))
          (reduce +)
          (#(utils/round % 5))))))

(defn- remap-within
  [f [start end] x]
  (when (and (>= x start) (< x end))
    (let [step (- end start)
          t (/ (- x start) step)]
      (f t))))

(defn line
  [a b]
  (fn
    ([] {:fn `line
         :input [a b]
         :origin (utils/centroid-of-pts [a b])
         :vertex-params [0 1]
         :length (utils/distance a b)})
    ([t]
     (cond
       (= (float t) 0.0) a
       (= (float t) 1.0) b
       :else
       (utils/v+ a (utils/v* (utils/v- b a) (repeat t)))))))

(defn fastline
  [[ax ay :as a] [bx by :as b]]
  (let [[vx vy] (utils/v- b a)]
    (fn [t]
      [(+ ax (* vx t))
       (+ ay (* vy t))])))

(defn polyline
  [pts]
  (let [step (/ 1.0 (dec (count pts)))
        lines (map (partial apply line) (partition 2 1 pts))
        length (reduce + (map #(:length (%)) lines))
        intervals (->> lines
                       (map #(:length (%)))
                       (reductions +)
                       (concat [0])
                       (map #(/ % length))
                       (partition 2 1))]
    (fn
      ([] {:fn `polyline
           :input [pts]
           :origin (utils/centroid-of-pts pts)
           :vertex-params (concat [0] (mapv second intervals))
           :length length})
      ([t]
       (cond
         (= (float t) 0.0) (first pts)
         (= (float t) 1.0) (last pts)
         :else
         (first
          (filter some?
                  (map #(remap-within %1 %2 t) lines intervals))))))))

(defn polygon
  [pts]
  (let [pts (concat (vec pts) [(first pts)])
        step (/ 1.0 (dec (count pts)))
        lines (map (partial apply line) (partition 2 1 pts))
        length (reduce + (map #(:length (%)) lines))
        intervals (->> lines
                       (map #(:length (%)))
                       (reductions +)
                       (concat [0])
                       (map #(/ % length))
                       (partition 2 1))]
    (fn
      ([] {:fn `polygon
           :input [pts]
           :origin (utils/centroid-of-pts pts)
           :vertex-params (concat [0] (mapv second intervals))
           :length (reduce + (map #(:length (%)) lines))})
      ([t]
       (cond
         (= (float t) 0.0) (first pts)
         (= (float t) 1.0) (last pts)
         :else
         (first
          (filter some?
                  (map #(remap-within %1 %2 t) lines intervals))))))))

(defn circle
  ([r]
   (fn
     ([] {:fn `circle
          :input [r]
          :origin [0 0]
          :vertex-params [0]
          :length (* Math/PI 2 r)})
     ([t]
      (let [t (* 2 Math/PI t)
            x (* r (Math/cos t))
            y (* r (Math/sin t))]
        [x y]))))

  ([a b c]
   (let [[a b c] (map #(conj % 0) [a b c])
         n (utils/normalize (utils/normal a b c))
         r (utils/radius-from-pts a b c)
         cp (utils/arc-center-from-pts a b c)
         u (utils/normalize (utils/v- a cp))
         v (utils/cross* n u)]
     (fn
       ([] {:fn `circle
            :input [a b c]
            :origin cp
            :vertex-params [0]
            :length (* Math/PI 2 r)
            :radius r})
       ([t]
       (cond
         (or (< t 0.0) (> t 1.0)) nil
         (= (float t) 0.0) (vec (drop-last a))
         (= (float t) 1.0) (vec (drop-last a))
         :else
         (let [t (* 2 Math/PI t)]
           (mapv 
            #(utils/round % 5)
            (drop-last 
             (utils/v+ cp
                       (utils/v* (repeat (* r (Math/cos t))) u)
                       (utils/v* (repeat (* r (Math/sin t))) v)))))))))))

(defn arc
  [a b c]
  (let [[a b c] (map #(conj % 0) [a b c])
        f (circle a b c)
        cp (utils/arc-center-from-pts a b c)
        angle (utils/angle-from-pts a cp c)
        r (utils/radius-from-pts a b c)]
    (fn
      ([] {:fn `arc
           :input [a b c]
           :origin cp
           :vertex-params [0 1]
           :length (* Math/PI 2 r (/ angle 360))
           :radius r
           :center cp})
      ([t]
       (let [t (* t (/ angle 360.0))]
         (f t))))))

;; https://www.mathsisfun.com/geometry/ellipse-perimeter.html
;; uses 'Infinite Series 2' exact calc. using 4 terms.
(defn- ellipse-perimeter
  [rx ry]
  (let [h (/ (Math/pow (- rx ry) 2)
             (Math/pow (+ rx ry) 2))]
    (* Math/PI (+ rx ry)
       (+ 1
          (* h (/ 1 4))
          (* h h (/ 1 64))
          (* h h h (/ 1 256))))))

(defn ellipse
  [rx ry]
  (fn 
    ([] {:fn `ellipse
         :input [rx ry]
         :origin [0 0]
         :vertex-params [0]
         :length (ellipse-perimeter rx ry)})
    ([t]
     (let [t (* 2 Math/PI t)
           x (* rx (Math/cos t))
           y (* ry (Math/sin t))]
       [x y]))))

(defn- quadratic-bezier
  [a b c]
  (fn [t]
    (let [l1 (fastline a b)
          l2 (fastline b c)
          l3 (fastline (l1 t) (l2 t))]
      (l3 t))))

(defn- bezier*
  [pts]
  (if (= 3 (count pts))
    (apply quadratic-bezier pts)
    (let [lines (map #(apply fastline %) (partition 2 1 pts))] 
      (fn
        [t]
        (let [npts (map #(% t) lines)]
          ((bezier* npts) t))))))

(defn bezier
  [pts]
  (let [curve (bezier* pts)
        length (arc-length curve)]
    (fn
      ([] {:fn `bezier
           :input [pts]
           :origin (utils/centroid-of-pts pts)
           :vertex-params [0 1]
           :length length})
      ([t] (curve t)))))

(defn- next-pascal
  [row]
  (vec (concat [(first row)]
          (mapv #(apply + %) (partition 2 1 row))
          [(last row)])))

(defn- binomial
  [n i]
  (let [pascal-tri-row (last (take (inc n) (iterate next-pascal [1])))]
  (get pascal-tri-row i)))

(defn- polynomial
  [n i t]
  (* (Math/pow (- 1 t) (- n i)) (Math/pow t i)))

(defn- half-bezier
  [ws t]
  (let [n (dec (count ws))
        poly (partial polynomial n)
        bi (partial binomial n)]
    (reduce + (map-indexed 
               (fn [i w]
                 (* (bi i) (poly i t) w))
               ws))))

(defn rational-bezier*
  [pts wts]
  (let [xs (map #(* (first %1) %2) pts wts)
        ys (map #(* (second %1) %2) pts wts)
        dn (partial half-bezier wts)]
    (fn [t]
      [(/ (half-bezier xs t) (dn t)) 
       (/ (half-bezier ys t) (dn t))])))

(defn rational-bezier
  [pts wts]
  (let [curve (rational-bezier* pts wts)
        length (arc-length curve)]
    (fn
      ([] {:fn `rational-bezier
           :input [pts wts]
           :origin (utils/centroid-of-pts pts)
           :vertex-params [0 1]
           :length length})
      ([t] (curve t)))))

(defn translate
  [f [x y z]]
  (let [data (f)]
    (fn
      ([] (merge data
                 {:fn `translate
                  :origin (utils/v+ (:origin data) [x y z])
                  :input [f [x y z]]}))
      ([t]
       (utils/v+ (utils/add-z (f t)) [x y z])))))

(defn rotate
  [f [ax ay az]]
  (let [data (f)
        ctr (:origin data)]
    (fn
      ([] (merge data
                 {:fn `rotate
                  :input [f [ax ay az]]}))
      ([t]
       (-> (f t)
           (utils/v+ (map - ctr))
           (utils/rotate-point [ax ay az])
           (utils/v+ ctr))))))

(defn scale
  [f [sx sy sz]]
  (let [data (f)
        ctr (:origin data)]
    (fn
      ([] (merge data
                 {:fn `scale
                  :input [f [sx sy sz]]}))
      ([t]
       (utils/scale-pt-from-center (f t) [sx sy sz] ctr)))))
