(ns forge.frep
  (:require [clojure.core.matrix :as m]
            [clojure.core.matrix.operators :as mo]
            [com.climate.claypoole :as cp]
            [forge.utils :as utils]
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
      (let [abs-pt (mapv #(Math/abs ^long %) pt)  
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












(defn polygon
  [pts]
  (fn [[px py :as pt]]
    (let [pts (vec pts)
          n (count pts)
          d1 (utils/dot* (utils/v- pt (first pts))
                           (utils/v- pt (first pts)))]
      (loop [i 0
             j (dec n)
             d d1
             s 1]
        (if (< i n)
          (let [[vix viy :as vi] (get pts i)
                [vjx vjy :as vj] (get pts j)
                [ex ey :as e] (utils/v- vj vi)
                [wx wy :as w] (utils/v- pt vi)
                [bx by :as b] (utils/v-
                               w
                               (mapv #(* % (utils/clamp 
                                            (/ (utils/dot* w e)
                                               (utils/dot* e e)) 0.0 1.0)) e))
                d (min d (utils/dot* b b))
                c [(>= py viy)
                   ( < py vjy)
                   ( > (* ex wy) (* ey wx))]
                s (if (= (count (into #{} c)) 1) (* s -1) (* s 1))]
            (recur (inc i) i d s))
          (* s (Math/sqrt d)))))))

(defn sphere
  [r]
  (fn [pt]
    (- (utils/distance pt [0 0 0]) r)))

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
          w (- (Math/abs ^long (- (last pt) (/ h 2))) (/ h 2))]
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

(defn prep-fn-grid
  [x1 y1 x2 y2 f]
  (let [pts (for [x (range x1 (inc x2))
                  y (range y1 (inc y2))] [x y])]
    (cp/pmap 10 #(vector % (f %)) pts)))

(defn render-pt
  [[pt dist]]
  (let [dist (utils/round dist 4)
        r 1
        a (utils/round 
           (- 1 (Math/abs ^double (/ (utils/clamp dist (- r) r) r))) 3)]
    (when (< -1000000 dist r)
      (svg/g 
       (-> (svg/rect 1 1)
           (tf/translate [0.5 0.5])
           (tf/translate pt)
           (tf/style {:fill (if (< dist 0)
                              "slategray"
                              "none")
                      :opacity 1 #_(if (< dist (- r)) 1 a)}))
       (-> (svg/rect 1 1)
           (tf/translate [0.5 0.5])
           (tf/translate pt)
           (tf/style {:fill (if (> dist (- r))
                              "#ffeeff"
                              "none")
                      :opacity a}))))))

(defn prep-fn-grid6
  [x1 y1 x2 y2 f]
  (cp/pfor 10 [x (range x1 (inc x2))
               y (range y1 (inc y2))]
           (render-pt [[x y] (f [x y])])))

(defn cider-show6
  [frep]
  (let [render (remove nil? (prep-fn-grid6 -200 -200 200 200 frep))]
    (when (not (empty? render))
      (svg-clj.tools/cider-show render))))

(defn cider-show
  [frep]
  (let [grid (prep-fn-grid -200 -200 200 200 frep)
        render (remove nil? (cp/pmap 10 render-pt grid))]
    (when (not (empty? render))
      (svg-clj.tools/cider-show render))))

(defn idx-grid
  [x1 y1 x2 y2]
  (m/array :ndarray
   (for [x (range x1 x2)]
     (for [y (range y1 y2)]
       {:x x :y y}))))

(defn array-frep
  [frep]
  (fn [pt _]
    (frep pt)))

(defn render-pt2
  [pt dist]
  (let [dist (utils/round dist 4)
        r 10
        a (utils/round 
           (- 1 (Math/abs ^double (/ (utils/clamp dist (- r) r) r))) 3)]
    (when (< -1000000 dist r)
      {:result
      (svg/g 
       (-> (svg/rect 1 1)
           (tf/translate [0.5 0.5])
           (tf/translate pt)
           (tf/style {:fill (if (< dist 0)
                              "slategray"
                              "none")
                      :opacity 1 #_(if (< dist (- r)) 1 a)}))
       (-> (svg/rect 1 1)
           (tf/translate [0.5 0.5])
           (tf/translate pt)
           (tf/style {:fill (if (> dist (- r))
                              "#ffeeff"
                              "none")
                      :opacity a})))})))

(defn render-pt3
  [frep pt dist]
  (let [dist (utils/round (frep pt) 4)
        r 10
        a (utils/round 
           (- 1 (Math/abs ^double (/ (utils/clamp dist (- r) r) r))) 3)]
    (when (< -1000000 dist r)
      {:result
      (svg/g 
       (-> (svg/rect 1 1)
           (tf/translate [0.5 0.5])
           (tf/translate pt)
           (tf/style {:fill (if (< dist 0)
                              "slategray"
                              "none")
                      :opacity 1 #_(if (< dist (- r)) 1 a)}))
       (-> (svg/rect 1 1)
           (tf/translate [0.5 0.5])
           (tf/translate pt)
           (tf/style {:fill (if (> dist (- r))
                              "#ffeeff"
                              "none")
                      :opacity a})))})))

(defn cider-show2
  [frep]
  (let [pixels (m/new-array :ndarray [400 400])]
    (do (m/emap-indexed! (array-frep (translate frep [-200 -200])) pixels)
        (m/emap-indexed! render-pt2 pixels)
        (let [spixels (pmap :result (filter some? (apply concat pixels)))]
          (when (not (empty? spixels))
            (svg-clj.tools/cider-show spixels))))))

(defn cider-show3
  [frep]
  (let [pixels (m/new-array :ndarray [400 400])
        f (translate frep [-200 -200])
        spixels (->> pixels
                     (m/emap-indexed (array-frep f))
                     (m/emap-indexed render-pt2)
                     (apply concat)
                     (filter some?)
                     (pmap :result))]
    (when (not (empty? spixels))
      (svg-clj.tools/cider-show spixels))))

(defn cider-show4
  [frep]
  (let [frep (translate frep [-200 -200])
        pixels (m/new-array :ndarray [400 400])]
    (do (m/emap-indexed! (partial render-pt3 frep) pixels)
        (let [spixels (pmap :result (filter some? (apply concat pixels)))]
          (when (not (empty? spixels))
            (svg-clj.tools/cider-show spixels))))))

(defn render-pt5
  [f pt]
  (let [dist (utils/round (f pt) 4)
        r 10
        a (utils/round 
           (- 1 (Math/abs ^double (/ (utils/clamp dist (- r) r) r))) 3)]
    (when (< -1000000 dist r)
      (svg/g 
       (-> (svg/rect 1 1)
           (tf/translate [0.5 0.5])
           (tf/translate pt)
           (tf/style {:fill (if (< dist 0)
                              "slategray"
                              "none")
                      :opacity 1 #_(if (< dist (- r)) 1 a)}))
       (-> (svg/rect 1 1)
           (tf/translate [0.5 0.5])
           (tf/translate pt)
           (tf/style {:fill (if (> dist (- r))
                              "#ffeeff"
                              "none")
                      :opacity a}))))))

(defn render5
  [w h frep render-fn]
  (let [xf (partial render-fn (translate frep [(/ w -2.0) (/ h -2.0)]))
        pts (for [x (range w)
                  y (range h)] [x y])]
    (doall (pmap xf pts))))

(defn cider-show5
  [frep]
  (let [render (filter some? (render5 400 400 frep render-pt5))]
    (when (not (empty? render))
      (svg-clj.tools/cider-show render))))

(def iso-euler-angles [35.264 45 0])
(def origin-angle-adjust-a [90 0 0])
(def origin-angle-adjust-b [0 -90 0])

(defn isometric-xf
  [frep]
  (-> frep
      #_(rotate origin-angle-adjust-b)
      #_(rotate origin-angle-adjust-a)
      (rotate iso-euler-angles)
      (rotate [0 0 45])))

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

(def a (-> (utils/regular-polygon-pts 80 8)
           (polygon)))

(def b (-> a
           (extrude 100)))

(defn march
  [f [x y]]
  (let [frep (isometric-xf f)
        from (conj [x y] -200)
        dir [0 0 1]
        max-steps 50
        min-dist 0.0001]
    (loop [n-steps 0
           total-dist 0]
      (let [pt (utils/v+ from [0 0 total-dist])
            d (utils/round (frep pt) 4)]
        (if (or (> n-steps max-steps)
                (< d min-dist))
          (- 1.0 (/ n-steps max-steps))
          (recur (inc n-steps) (+ total-dist d)))))))

(defn render-pixel
  [[pt a]]
  (when (> a 0.0001)
    (-> (svg/rect 1 1)
        (tf/translate [0.5 0.5])
        (tf/translate pt)
        (tf/style {:fill "slategray"
                   :opacity a}))))

(defn render-frep
  [x1 y1 x2 y2 f]
  (cp/pfor 10 [x (range x1 (inc x2))
               y (range y1 (inc y2))]
           (render-pixel [[x y] (f [x y])])))

(defn cider-show-3d
  [frep]
  (let [f (partial march frep)
        render (remove nil? (render-frep -200 -200 200 200 f))]
    (when (not (empty? render))
      (svg-clj.tools/cider-show render))))

(def asdf
  (let [base (box 200 200 200)]
    (-> (difference
         (box 80 80 80)
         (sphere 50))
        (rotate [0 0 85]))))
