(ns forge.frep
  (:require [forge.utils :as utils]
            [forge.geom :as geom]
            [forge.delaunay :as delaunay]
            [clojure.string :as str]
            [same :refer [ish? zeroish?]]))

(defn union [f g]
  (fn [pt]
    (let [a (f pt)
          b (g pt)]
      (min a b))))

(defn difference [f g]
  (fn [pt]
    (let [a (f pt)
          b (* -1 (g pt))]
      (max a b))))

(defn intersection [f g]
  (fn [pt]
    (let [a (f pt)
          b (g pt)]
      (max a b))))

(defn line
  [a b]
  (fn [pt]
    (let [pa (map - pt a)
          ba (map - b a)
          h (utils/clamp (/ (utils/dot* pa ba) (utils/dot* ba ba)) 0 1)]
      (utils/distance (map - pa (map * ba (repeat h))) [0 0 0]))))

(defn circle
  [r]
  (fn [pt]
    (- (utils/distance pt [0 0 0]) r)))

(defn triangle
  [a b c]
  (fn [pt]
    (let [[e0 e1 e2] (map #(apply utils/v- %) [[b a] [c b] [a c]])
          [v0 v1 v2] (map (partial utils/v- pt) [a b c])
          xf (fn [v e] 
               (utils/v- v (map * e (repeat (utils/clamp (/ (utils/dot* v e) (utils/dot* e e)) 0 1)))))
          [pq0 pq1 pq2] (map #(apply xf %) [[v0 e0] [v1 e1] [v2 e2]])
          s (utils/sign (- (* (first e0) (second e2)) (* (second e0) (first e2))))
          d1 (min (utils/dot* pq0 pq0)
                  (utils/dot* pq1 pq1)
                  (utils/dot* pq2 pq2))
          d2 (min (* s (- (* (first v0) (second e0)) (* (second v0) (first e0))))
                  (* s (- (* (first v1) (second e1)) (* (second v1) (first e1))))
                  (* s (- (* (first v2) (second e2)) (* (second v2) (first e2)))))]
      (* -1 (Math/sqrt d1) (utils/sign d2)))))

(defn polygon
  [paths]
  (let [pts (apply concat paths) ;; temporary avoidance of path merge
        tris (map
              #(apply triangle %)
              (geom/clip-ears pts)
              (:triangles (geom/clip-ears pts)))]
    (reduce union tris)))

(defn sphere [r]
  (fn [pt]
    (let [[x y z] pt]
      (+ (utils/sq x) (utils/sq y) (utils/sq z) (- (utils/sq r))))))

(defn cylinder [r h]
  (fn [pt]
    (let [[x y z] pt]
      (max (- (Math/sqrt (+ (utils/sq x) (utils/sq y))) r)
           (- z h) (- (- h) z)))))

(defn box [l w h]
  (fn [pt]
    (let [[x y z] pt]
      (max (- x l) (- (- l) x)
           (- y w) (- (- w) y)
           (- z h) (- (- h) z)))))

(defn translate
  [f pos]
  (fn [pt]
    (f (utils/v+ pt pos))))

(defn rotate
  [f angles]
  (fn [pt]
    (f (utils/rotate-point pt angles))))

(defn scale
  [f scales]
  (fn [pt]
    (f (utils/v* pt scales))))

(defn extrude
  [f h]
  (fn [pt]
    (let [d (f (drop-last pt))
          w [d (- (Math/abs (last pt)) h)]]
      (+ (min (apply max w) 0)
         (utils/distance [0 0]
                   [(max (first w) 0) (max (second w) 0)])))))

(defn revolve
  [f r]
  (fn [pt]
    (let [q [(- (utils/distance [0 0] [(first pt) (last pt)]) r) 
             (second pt)]]
      (f q))))
