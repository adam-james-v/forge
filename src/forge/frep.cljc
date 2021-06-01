(ns forge.frep
  (:require [forge.utils :as utils]
            [forge.geom :as geom]
            [forge.delaunay :as delaunay]
            [svg-clj.elements :as svg]
            [svg-clj.transforms :as tf]
            [svg-clj.tools :as tools]
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

(defn pt
  ([x y]
   (fn [pt]
     (utils/distance [x y] pt)))
  ([x y z]
   (fn [pt]
     (utils/distance [x y z] pt))))

(defn line
  [a b]
  (fn [pt]
    (let [pa (utils/v- pt a)
          ba (utils/v- b a)
          h (utils/clamp (/ (utils/dot* pa ba) (utils/dot* ba ba)) 0 1)]
      (utils/distance (utils/v- pa (utils/v* ba (repeat h))) [0 0 0]))))

(defn polyline
  [pts]
  (let [lines (->> (partition 2 1 pts)
                   (map #(apply line %)))]
    (reduce union lines)))

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

(defn rect
  [l w]
  (let [b [(/ l 2.0) (/ w 2.0)]]
    (fn [pt]
      (let [abs-pt (mapv #(Math/abs %) pt)  
            d (utils/v- abs-pt b)]
        (+ (utils/distance (mapv #(max % 0) d) (repeat 0))
           (min (apply max d) 0))))))

(defn circle
  [r]
  (fn [pt]
    (- (utils/distance pt (repeat 0)) r)))

(defn polygon
  [pts]
  (let [tris (map #(apply triangle %) (geom/clip-ears pts))] 
    (reduce union tris)))

(defn sphere [r]
  (fn [pt]
    (let [[x y z] pt]
      (+ (utils/sq x) (utils/sq y) (utils/sq z) (- (utils/sq r))))))

(defn cylinder [r h]
  (fn [pt]
    (let [[x y z] pt]
      (max (- (Math/sqrt (+ (utils/sq x) (utils/sq y))) r)
           (- z (/ h 2)) (- (/ h -2) z)))))

(defn box [l w h]
  (fn [pt]
    (let [[x y z] pt
          [lh wh hh] (map #(/ % 2) [l w h])]
      (max (- x lh) (- (- lh) x)
           (- y wh) (- (- wh) y)
           (- z hh) (- (- hh) z)))))

(defn extrude
  [frep h]
  (fn [pt]
    (let [d (frep (drop-last pt))
          w (- (Math/abs (- (last pt) (/ h 2))) (/ h 2))]
      (+ (min (max d w) 0)
         (utils/distance [0 0] [(max d 0) (max w 0)])))))

(defn revolve
  [f]
  (fn [pt]
    (let [q [(utils/distance [0 0] [(first pt) (second pt)])
             (last pt)]]
      (f q))))

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

(defn cider-show
  [frep]
  (let [render
        (filter 
         some?
         (for [x (range -200 201)
               y (range -200 201)]
           (let [r 5.0
                 pt [x y]
                 dist (frep pt)
                 a (utils/round 
                    (- 1 (/ (utils/clamp dist 0 r) r)) 3)]
             (when (< (- r) dist r)
               (-> (svg/rect 1 1)
                   (tf/translate [0.5 0.5])
                   (tf/translate pt)
                   (tf/style {:fill (if (> dist 0)
                                      "hotpink"
                                      "skyblue")
                              :opacity (if (< dist 0) 1 a)}))))))]
    (when (not (empty? render))
      (svg-clj.tools/show render))))

(def iso-euler-angles (map utils/to-rad [35.264 45 0]))
(def origin-angle-adjust-a (map utils/to-rad [90 0 0]))
(def origin-angle-adjust-b (map utils/to-rad [0 -90 0]))

(defn isometric-xf
  [frep]
  (-> frep
      (rotate origin-angle-adjust-a)
      (rotate origin-angle-adjust-b)
      (rotate iso-euler-angles)))

(defn cider-show-3d
  [frep]
  (let [frep (isometric-xf frep)
        render
        (filter 
         some?
         (for [x (range -200 201 1)
               y (range -200 201 1)
               z (range -200 201 1)]
             (let [r 0.5
                   pt [x y z]
                   dist (frep pt)
                   a (utils/round 
                      (- 1 (/ (utils/clamp dist 0 r) r)) 3)]
               (when (< (- r) dist r)
                 (-> (svg/rect 1 1)
                     (tf/translate [0.5 0.5])
                     (tf/translate (drop-last pt))
                     (tf/style {:fill (if (> dist 0)
                                        "hotpink"
                                        "skyblue")
                                :opacity (if (< dist 0) 1 a)}))))))]
        (when (not (empty? render))
          (svg-clj.tools/cider-show render))))

(defn slice
  [frep z]
  (fn [pt]
    (frep (conj pt z))))

(def a (-> (utils/regular-polygon-pts 50 8)
           (polygon)
           (translate [200 0 0])
           (revolve)))

(def b (-> (utils/regular-polygon-pts 50 8)
           polygon
           (extrude 100)))
