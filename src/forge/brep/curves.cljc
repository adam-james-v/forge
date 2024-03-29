(ns forge.brep.curves
  (:require [clojure.string :as str]
            [forge.utils :as utils]
            [forge.geom :as geom]
            [forge.delaunay :as delaunay]
            [forge.clip-ears :as clip-ears]))

(defn check-parametric
  [f]
  (let [fdata (try (f) (catch #?(:cljs :default :clj Exception) e))
        [f0 f05 f1] (map f [0 0.5 1])
        t0 (if (seqable? f0) f0 [f0])
        t05 (if (seqable? f05) f05 [f05])
        t1 (if (seqable? f1) f1 [f1])
        dim (count t05)
        required [:fn :input :vertex-params :dimension :length :origin]
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
  ([curve t]
   (let [eps 0.00001]
     (if (< eps t)
       (arc-length curve 0 t)
       0)))
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

(defn tangent
  [curve t]
  (let [eps 0.00001
        a (curve (- t eps))
        b (curve (+ t eps))
        a (if a a (curve 0))
        b (if b b (curve 1))]
    (-> (utils/v- b a)
        (utils/normalize))))

(defn normal
  [curve t up]
  (let [tangent (tangent curve t)
        binormal (utils/normalize (utils/cross* up tangent))]
    (utils/cross* tangent binormal)))

(defn- remap-within
  ([[start end] x] (remap-within identity [start end] x))
  ([f [start end] x]
   (when (and (>= x start) (< x end))
     (let [step (- end start)
           t (/ (- x start) step)]
       (f t)))))

(defn line
  [a b]
  (fn
    ([] {:fn `line
         :input [a b]
         :origin (utils/centroid-of-pts [a b])
         :vertex-params [0 1]
         :dimension (count a)
         :length (utils/distance a b)})
    ([t]
     (cond
       (= (float t) 0.0) a
       (= (float t) 1.0) b
       :else
       (utils/v+ a (utils/v* (utils/v- b a) (repeat t)))))))

(defn fastline
  [a b]
  (let [[vx vy] (utils/v- b a)]
    (fn [t]
      (utils/v+ a (utils/v* (utils/v- b a) (repeat t))))))

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
           :origin [0 0 0] #_(utils/centroid-of-pts pts)
           :vertex-params (concat [0] (mapv second intervals))
           :dimension (count (first pts))
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
           :origin [0 0 0] #_(utils/centroid-of-pts pts)
           :vertex-params (concat [0] (mapv second intervals))
           :dimension (count (first pts))
           :length (reduce + (map #(:length (%)) lines))})
      ([t]
       (cond
         (= (float t) 0.0) (first pts)
         (= (float t) 1.0) (last pts)
         :else
         (first
          (filter some?
                  (map #(remap-within %1 %2 t) lines intervals))))))))

(defn to-3D
  [curve]
  (let [data (curve)
        origin (utils/add-z (:origin data))]
    (if (< (:dimension data) 3)
      (fn
        ([] (merge data {:fn `to-3D
                         :input [curve]
                         :origin origin
                         :dimension 3}))
        ([t]
         (when (curve t)
           (vec (concat (curve t) [0])))))
      curve)))

(defn circle
  ([r]
   (fn
     ([] {:fn `circle
          :input [r]
          :origin [0 0]
          :vertex-params [0]
          :dimension 2
          :length (* Math/PI 2 r)})
     ([t]
      (let [t (* 2 Math/PI t)
            x (* r (Math/cos t))
            y (* r (Math/sin t))]
        [x y]))))

  ([a b c]
   (let [[a b c] (map utils/add-z [a b c])
         n (utils/normalize (utils/normal a b c))
         r (utils/radius-from-pts a b c)
         cp (utils/arc-center-from-pts a b c)
         u (utils/normalize (utils/v- a cp))
         v (utils/cross* n u)
         dropfn (if (= (count a) 2) drop-last identity)]
     (fn
       ([] {:fn `circle
            :input [a b c]
            :origin cp
            :vertex-params [0]
            :dimension (count a)
            :length (* Math/PI 2 r)
            :radius r})
       ([t]
        (cond
          (or (< t 0.0) (> t 1.0)) nil
          (= (float t) 0.0) (vec (dropfn a))
          (= (float t) 1.0) (vec (dropfn a))
          :else
          (let [t (* 2 Math/PI t)]
            (mapv
             #(utils/round % 5)
             (dropfn
              (utils/v+ cp
                        (utils/v* (repeat (* r (Math/cos t))) u)
                        (utils/v* (repeat (* r (Math/sin t))) v)))))))))))

(defn arc
  [a b c]
  (let [[a b c] (map utils/add-z [a b c])
        f (to-3D (circle a b c))
        cp (utils/arc-center-from-pts a b c)
        angle (utils/angle-from-pts a cp c)
        r (utils/radius-from-pts a b c)]
    (fn
      ([] {:fn `arc
           :input [a b c]
           :origin cp
           :vertex-params [0 1]
           :dimension (count a)
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
         :dimension 2
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
           :dimension (count (first pts))
           :vertex-params [0 1]
           :length length})
      ([t] (curve t)))))

(defn piecewise-curve
  [curves]
  (let [step (/ 1.0 (count curves))
        intervals (partition 2 1 (range 0 (+ 1 step) step))
        remapf (fn [curve [start end]]
                 (let [vertex-params (:vertex-params (curve))
                       sc (- end start)]
                   (map #(+ start (* sc %)) vertex-params)))
        vertex-params (vec (distinct (mapcat remapf curves intervals)))
        origin (utils/centroid-of-pts (map #(:origin (%)) curves))
        length (reduce + (map #(:length (%)) curves))
        sample-curve (first curves)]
    (fn
      ([] {:fn `piecewise-curve
           :input [curves]
           :origin origin
           :dimension (count (sample-curve 0.5))
           :vertex-params vertex-params
           :length length})
      ([t]
       (cond
         (= (float t) 0.0) ((first curves) 0)
         (= (float t) 1.0) ((last curves) 1)
         :else
         (first
          (filter some?
                  (map #(remap-within %1 %2 t) curves intervals))))))))

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
           :dimension (count (first pts))
           :vertex-params [0 1]
           :length length})
      ([t] (curve t)))))

(defn translate
  [f [x y z]]
  (let [data (f)
        f (to-3D f)]
    (fn
      ([] (merge data
                 {:fn `translate
                  :origin (utils/v+ (utils/add-z (:origin data)) [x y z])
                  :dimension 3
                  :input [f [x y z]]}))
      ([t]
       (utils/v+ (utils/add-z (f t)) [x y z])))))

(defn rotate
  [f [ax ay az]]
  (let [data (f)
        f (to-3D f)
        ctr (utils/add-z (:origin data))
        dim 3]
    (fn
      ([] (merge data
                 {:fn `rotate
                  :input [f [ax ay az]]
                  :dimension dim}))
      ([t]
       (-> (f t)
           (utils/v+ (map - ctr))
           (utils/rotate-pt [ax ay az])
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

(defn cut-at-t
  "Cut curve at the parameter `t`, returning a curve from `t-start` (0 if unspecified) to `t-end`."
  ([curve t-end] (cut-at-t curve 0 t-end))
  ([curve t-start t-end]
   (let [data (curve)
         sc (- t-end t-start)
         length (arc-length t-start t-end)]
     (fn
       ([] (merge data {:fn `cut-at-t
                        :input [curve t-start t-end]
                        :length length}))
       ([t] (curve (+ (* t sc) t-start)))))))

(defn- get-t
  "Estimate curve parameter `t` that corresponds to length-percentage `target-lp`."
  [curve target-lp]
  (let [eps 0.00001
        length (:length (curve))
        target-l (* length target-lp)]
    (loop [t target-lp
           n 0]
        (let [next-t (+ t (/ (- target-l (arc-length curve t)) target-l))]
          (if (or
               (= (utils/round t 4) (utils/round next-t 4))
               (< (utils/abs (- target-l (arc-length curve t))) eps)
               (< 300 n))
            next-t
            (recur next-t (inc n)))))))

(defn cut-at-lp
  "Cut curve at length-percentage `lp`."
  ([curve l-end] (cut-at-lp curve 0 l-end))
  ([curve l-start l-end]
   (let [data (curve)
         t-start (get-t curve l-start)
         t-end (get-t curve l-end)
         sc (- t-end t-start)
         length (arc-length t-start t-end)]
     (fn
       ([] (merge data {:fn `cut-at-lp :input [curve l-start l-end]}))
       ([t] (curve (+ (* t sc) t-start)))))))

#_(defn look-at
  [f [px py pz] [ux uy uz]]
  (let [data (f)
        ctr (utils/add-z (:origin data))
        dim 3]
    (fn
      ([] (merge data
                 {:fn `look-at
                  :input [f [px py pz] [ux uy uz]]
                  :dimension dim}))
      ([t]
       (-> (f t)
           utils/add-z
           (transform-pt-quaternion (look-at-quaternion ctr [px py pz] [ux uy uz])))))))

(defn- aligned?
  [[ax ay az] [bx by bz]]
  (and (utils/my-zeroish? (Math/abs (- ax bx)))
       (utils/my-zeroish? (Math/abs (- ay by)))
       (utils/my-zeroish? (Math/abs (- az bz)))))

(defn find-up
  [c]
  (let [data (c)
        [a b c :as pts] (map c [0 0.333 0.667])
        is-line (utils/on-line-inf? a [b c])
        v1 (utils/normalize (utils/v- b a))]
    (cond
      (= (:dimension data) 2)
      [0 0 1]

      (not is-line)
      (-> (apply utils/normal pts)
          utils/normalize)

      (and is-line (aligned? v1 [0 0 1]))
      [0 1 0]

      :else
      [0 0 1])))

(defn look-at
  ([c target] (look-at c target (find-up c)))
  ([c target up]
   (let [data (c)
        ctr (utils/add-z (:origin data))
        dim 3]
    (fn
      ([] (merge data
                 {:fn `look-at
                  :input [c target up]
                  :origin ctr
                  :dimension dim}))
      ([t]
       (if (utils/colinear? (utils/normalize up) ctr target)
         (c t)
         (let [m (utils/look-at-matrix [0 0 0] (utils/v- target ctr) up)]
         (-> (c t)
             utils/add-z
             (utils/v- ctr)
             (utils/transform-pt-matrix m)
             (utils/v+ ctr)))))))))

(ns forge.scene
  (:require [clojure.string :as str]
            [forge.utils :as utils]
            [forge.geom :as geom]
            [forge.model :as mdl]))

(def iso-euler-angles [35.264 45 0])
(def origin-angle-adjust-a [90 0 0])
(def origin-angle-adjust-b [0 -90 0])

(defn- sin-cos-pair [theta]
  [(Math/sin ^double (utils/to-rad theta))
   (Math/cos ^double (utils/to-rad theta))])

(defn- rot-pt-2d
  [[x y] theta]
  (let [[s-t c-t] (sin-cos-pair theta)]
    [(- (* x c-t) (* y s-t))
     (+ (* y c-t) (* x s-t))]))

;; this rotates a point around [0,0,0]
(defn- rot-pt
  [[x y z] axis theta]
  (cond
    (= axis :x) (into [x] (rot-pt-2d [y z] theta))
    (= axis :y) (apply #(into [] [%2 y %1]) (rot-pt-2d [z x] theta))
    (= axis :z) (into (rot-pt-2d [x y] theta) [z])))

(defn- rotate-point
  [pt [ax ay az]]
  (let [pt (if (< (count pt) 3)
             (conj pt 0)
             pt)]
    (-> pt
        (rot-pt :z az)
        (rot-pt :y ay)
        (rot-pt :x ax))))

(defn- rotate-points
  [pts [ax ay az]]
  (mapv #(rotate-point % [ax ay az]) pts))

(defn- rotate-points2
  [pts [ax ay az]]
  (transduce (comp #(rot-pt % :z az)
                   #(rot-pt % :y ay)
                   #(rot-pt % :x ax)) conj [] pts))

;; todo: do this with 4x4 tfrm matrix instead.
(defn isometric-xf-pts
  [pts]
  (-> pts
      (rotate-points origin-angle-adjust-a)
      (rotate-points origin-angle-adjust-b)
      (rotate-points iso-euler-angles)))
