(ns forge.brep.old
  (:require [forge.utils :as utils]
            [forge.geom :as geom]
            [forge.delaunay :as delaunay]
            [clojure.string :as str]
            [same :refer [ish? zeroish?]]))

(defn remap-within
  [f [start end] x]
  (when (and (>= x start) (< x end))
    (let [step (- end start)
          t (/ (- x start) step)]
      (f t))))

(defn line
  [a b]
  (fn [t]
    (cond
      (= t :tag) :line
      (= (float t) 0.0) a
      (= (float t) 1.0) b
      :else
      (utils/v+ a (utils/v* (utils/v- b a) (repeat t))))))

(defn polyline
  [pts]
  (let [step (/ 1.0 (dec (count pts)))
        intervals (partition 2 1 (range 0 (+ 1 step) step))
        lines (map (partial apply line) (partition 2 1 pts))]
    (fn [t]
      (cond
        (= t :tag) :polyline
        (= (float t) 0.0) (first pts)
        (= (float t) 1.0) (last pts)
        :else
        (first (filter some?
                       (map #(remap-within %1 %2 t) lines intervals)))))))

;;https://mathforum.org/library/drmath/view/63755.html
(defn curve-circle
  [a b c]
  (let [n (utils/normalize (utils/normal a b c))
        r (utils/radius-from-pts a b c)
        cp (utils/center-from-pts a b c)
        u (utils/normalize (mapv - a cp))
        v (utils/cross* n u)]
    (fn [t]
      (cond
        (= t :tag) :circle
        (or (< t 0.0) (> t 1.0)) nil
        (= (float t) 0.0) a
        (= (float t) 1.0) a
        :else
        (let [t (* 2 Math/PI t)]
          (utils/v+ cp
              (utils/v* (repeat (* r (Math/cos t))) u)
              (utils/v* (repeat (* r (Math/sin t))) v)))))))

(defn surface-circle
  [a b c]
  (let [cp (utils/center-from-pts a b c)
        c1 (curve-circle a b c)]
    (fn [u v]
      (let [c2 (line cp (c1 u))]
        (c2 v)))))

(defn curve-ellipse
  [rx ry]
  (fn [t]
    (let [t (* 2 Math/PI t)
          x (* rx (Math/cos t))
          y (* ry (Math/sin t))]
      [x y])))

(defn curve-arc
  [a b c]
  (let [f (curve-circle a b c)
        cp (utils/center-from-pts a b c)
        angle (utils/angle-from-pts a cp c)]
    (fn [t]
      (let [t (* t (/ angle 360.0))]
        (f t)))))

(defn curve-polygon
  [pts]
  (polyline (conj (vec pts) (first pts))))

(defn- quadratic-bezier
  [a b c]
  (fn [t]
    (let [l1 (line a b)
          l2 (line b c)
          l3 (line (l1 t) (l2 t))]
      (l3 t))))

(defn bezier
  [pts]
  (if (= 3 (count pts))
    (apply quadratic-bezier pts)
    (let [lines (map #(apply line %) (partition 2 1 pts))]
      (fn [t]
        (let [npts (map #(% t) lines)]
          ((bezier npts) t))))))

(defn surface-triangle
  [a b c]
  (let [l1 (line b a)
        l2 (line c a)]
    (fn [u v]
      (let [l3 (line (l1 v) (l2 v))]
        (l3 u)))))

(defn sphere
  [r]
  (fn [u v]
    (let [[u v] (map #(* 2 Math/PI %) [u v])
          x (* r (Math/sin u) (Math/cos v))
          y (* r (Math/sin u) (Math/sin v))
          z (* r (Math/cos u))]
      [x y z])))

(defn surface-torus
  [R r]
  (fn [u v]
    (let [[u v] (map #(* 2 Math/PI %) [u v])
          x (* (+ R (* r (Math/cos u))) (Math/cos v))
          y (* (+ R (* r (Math/cos u))) (Math/sin v))
          z (* r (Math/sin u))]
      [x y z])))

(defn surface-cylinder
  [r h]
  (fn [u v]
    (let [u (* 2 Math/PI u)
          v (* h v)
          x (* r (Math/cos u))
          y (* r (Math/sin u))
          z v]
      [x y z])))

(defn translate
  [f pos]
  (comp #(utils/v+ pos %) f))

(defn brep-rotate
  [f angles]
  (comp #(utils/rotate-pt % angles) f))

(defn scale
  [f scales]
  (comp #(utils/v* scales %) f))

(defn curve-extrude
  [c h]
  (fn [u v]
    (let [c2 (line (c u) (utils/v+ (c u) [0 0 h]))]
      (c2 v))))

(defn surface-extrude
  [s h]
  (fn [u v w]
    (let [c1 (line (s u v) (utils/v+ (s u v) [0 0 h]))]
      c1 v)))
