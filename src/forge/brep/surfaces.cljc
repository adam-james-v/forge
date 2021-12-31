(ns forge.brep.surfaces
  (:require [clojure.string :as str]
            [forge.utils :as utils]
            [forge.geom :as geom]
            [forge.delaunay :as delaunay]
            [forge.clip-ears :as clip-ears]
            [forge.brep.curves :as c]))

(defn check-parametric
  [f]
  :not-implemented #_(let [fdata (try (f) (catch #?(:cljs :default :clj Exception) e))
        [f00 f01 f10 f11 f0505] (map f [[0 0] [0 1] [1 0] [1 1] [0.5 0.5]])
        t0 (if (seqable? f00) f0 [f0])
        t05 (if (seqable? f0505) f05 [f05])
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
  :not-implemented #_(nil? (:error (check-parametric f))))

(defn coincident?
  [a b]
  (let [eps 0.00001
        diffs (map #(utils/abs (- %1 %2)) a b)]
    (when (= (count a) (count b))
      (= #{true} (set (map #(< % eps) diffs))))))

(defn- closed?
  [curve]
  (coincident? (curve 0) (curve 1)))

(defn- coincidence-table
  [pts]
  (let [pt-index (zipmap (range (count pts)) pts)
        fh (fn [pta ptb]
             (let [result (coincident? pta ptb)]
               (list (vector [pta ptb] result)
                     (vector [ptb pta] result))))
        f (fn [idx pt]
            (let [ipts (vals (dissoc pt-index idx))]
              (into {} (mapcat #(fh pt %) ipts))))]
    (apply merge (map-indexed f pts))))

(defn- flip-second-curve?
  [a0 b0 b1]
  (and
   ;; don't flip closed curves as it might break user design intent
   (not (coincident? b0 b1))
   (< (utils/distance a0 b1) (utils/distance a0 b0))))

(defn- curve3D?
  [curve]
  (= 3 (count (curve 0))))

(defn reverse-curve
  [curve]
  (fn [t]
    (curve (- 1 t))))

(defn- surface-along*
  [curve-a curve-b]
  (fn [u v]
    (utils/v+ (curve-a u) (curve-b v))))

(defn- surface-between*
  [curve-a curve-b]
  (fn [u v]
    (let [l (c/line (curve-a u) (curve-b u))]
      (l v))))

(defn surface
  [curve-a curve-b]
  (let [curve-a (c/to-3D curve-a)
        curve-b (c/to-3D curve-b)
        [a0 a1] (map curve-a [0 1])
        [b0 b1] (map curve-b [0 1])
        flip (flip-second-curve? a0 b0 b1)
        curve-b (if flip (reverse-curve curve-b) curve-b)
        [b0 b1] (if flip [b1 b0] [b0 b1])
        ctable (coincidence-table [a0 a1 b0 b1])
        surf (cond
               ;; all vals false, no coincident points
               (not (seq (filter true? (vals ctable))))
               (surface-between* curve-a curve-b)
               
               ;; curves may be closed, but don't share start/end pts
               (and (not (ctable [a0 b0])) (not (ctable [a1 b1])))
               (surface-between* curve-a curve-b)
               
               ;; start and end points for each curve are coincident
               (and (ctable [a0 b0]) (ctable [a1 b1]))
               (surface-between* curve-a curve-b)
               
               ;; only start pts coincide
               (ctable [a0 b0])
               (surface-along* curve-a curve-b)
               
               ;; only end pts coincide
               (ctable [a1 b1])
               (surface-along* curve-a curve-b))]
    (fn
      ([] {:fn `surface
           :input [curve-a curve-b]
           :origin (surf 0.5 0.5)})
      ([u v] (surf u v)))))

(defn- bezier-patch*
  ([apts bpts cpts] (bezier-patch* apts bpts cpts nil))
  ([apts bpts cpts dpts]
   (let [cpt-sets (remove nil? [apts bpts cpts dpts])
         curves (map c/bezier cpts)]
       (fn [u v]
         (let [curve (c/bezier (map #(% u) curves))]
           (curve v))))))

(defn bezier-patch
  ([apts bpts cpts] (bezier-patch apts bpts cpts nil))
  ([apts bpts cpts dpts]
   (let [cpt-sets (remove nil? [apts bpts cpts dpts])
         curves (map c/bezier cpts)
         surf (bezier-patch* apts bpts cpts dpts)
         origin (surf 0.5 0.5)]
     (fn
       ([] {:fn `bezier-patch
            :input [apts bpts cpts dpts]
            :origin origin})
       ([u v] (surf u v))))))

(defn extrude-curve
  ([curve h] (extrude-curve curve [0 0 1] h))
  ([curve direction d]
   (let [curve (c/to-3D curve)
         v (utils/v* (utils/normalize direction) (repeat 3 d))]
     (fn
       ([] {:fn `extrude-curve
            :input [curve direction d]
            :origin (:origin (curve))})
       ([u v]
        (utils/v+ (curve u) (utils/v* v (repeat 3 (* d v)))))))))

(defn rect
  [w h]
  (let [[wh hh] (map #(/ % 2.0) [w h])
        uline (c/line [(- wh) (- hh)] [(+ wh) (- hh)])
        vline (c/line [(- wh) (- hh)] [(- wh) (+ hh)])]
    (fn
      ([] {:fn `rect
           :input [w h]
           :origin [(uline 0.5) (vline 0.5) 0]})
      ([u v]
       [(first (uline u))
        (second (vline v))
        0]))))

(defn circle
  [r]
  (let [perim (c/circle r)]
    (fn
      ([] {:fn `circle
           :input [r]
           :origin [0 0 0]})
      ([u v]
       (let [rad (c/line [0 0] (perim u))]
         (rad v))))))

(defn triangle
  [a b c]
  (let [ctr (utils/centroid-of-pts a b c)
        perim (c/polygon [a b c])]
    (fn
      ([] {:fn `triangle
           :input [a b c]
           :origin ctr})
      ([u v]
       (let [rad (c/line ctr (perim u))]
         (rad v))))))

(defn translate
  [f [x y z]]
  (let [data (f)]
    (fn
      ([] (merge data
                 {:fn `translate
                  :origin (utils/v+ (:origin data) [x y z])
                  :dimension 3
                  :input [f [x y z]]}))
      ([u v]
       (utils/v+ (utils/add-z (f u v)) [x y z])))))

(defn rotate
  [f [ax ay az]]
  (let [data (f)
        ctr (:origin data)
        dim (if (and (= 0 ax) (= 0 ay)) (:dimension data) 3)]
    (fn
      ([] (merge data
                 {:fn `rotate
                  :input [f [ax ay az]]
                  :dimension dim}))
      ([u v]
       (-> (f u v)
           (utils/v+ (map - ctr))
           (utils/rotate-pt [ax ay az])
           (utils/v+ ctr))))))

;; https://stackoverflow.com/a/52551983
(defn- look-at-quaternion
  [from to world-up]
  (let [world-up (utils/normalize world-up)
        [fx fy fz :as forward] (utils/normalize (utils/v- from to))
        [rx ry rz :as right] (utils/normalize (utils/cross* world-up forward))
        [ux uy uz :as up] (utils/cross* forward right)
        trace (+ rx uy fz)]
    (cond
      (> trace 0)
      (let [s (/ 0.5 (Math/sqrt (+ trace 1.0)))
            x (* (- uz fy) s)
            y (* (- fx rz) s)
            z (* (- ry ux) s)
            w (/ 0.25 s)]
        [x y z w])

      (and (> rx uy) (> rx fz))
      (let [s (* 2 (Math/sqrt (+ 1 rx (- uy) (- fz))))
            x (* 0.25 s)
            y (/ (+ ux ry) s)
            z (/ (+ fx rz) s)
            w (/ (- uz fy) s)]
        [x y z w])

      (> uy fz)
      (let [s (* 2 (Math/sqrt (+ 1 uy (- rx) (- fz))))
            x (/ (+ ux ry) s)
            y (* 0.25 s)
            z (/ (+ fy uz) s)
            w (/ (- fx rz) s)]
        [x y z w])

      :else
      (let [s (* 2 (Math/sqrt (+ 1 fz (- rx) (- uy))))
            x (/ (+ fx rz) s)
            y (/ (+ fy uz) s)
            z (* 0.25 s)
            w (/ (- ry ux) s)]
        [x y z w]))))

(defn transform-pt-quaternion
  [[px py pz] [x y z w]]
  [(+ (* w w px) (* 2 y w pz) (* -2 z w py) (* x x px)
      (* 2 y x py) (* 2 z x pz) (* -1 z z px) (* -1 y y px))

   (+ (* 2 x y px) (* y y py) (* 2 z y pz) (* 2 w z px)
      (* -1 z z py) (* w w py) (* -2 x w pz) (* -1 x x py))

   (+ (* 2 x z px) (* 2 y z py) (* z z pz) (* -2 w y px)
      (* -1 y y pz) (* 2 w x py) (* x x pz) (* w w pz))])


(defn look-at
  [f [px py pz] [ux uy uz]]
  (let [data (f)
        ctr (utils/add-z (:origin data))
        dim 3]
    (fn
      ([] (merge data
                 {:fn `look-at
                  :input [f [px py pz] [ux uy uz]]
                  :dimension dim}))
      ([u v]
       (-> (f u v)
           utils/add-z
           (transform-pt-quaternion (look-at-quaternion ctr [px py pz] [ux uy uz])))))))

(defn axis-angle
  [va vb]
  (let [angle (-> (utils/dot* (utils/normalize va) (utils/normalize vb))
                  Math/acos
                  utils/to-deg)
        axis (utils/normalize (utils/cross* va vb))]
    [axis angle]))

(defn axis-angle->euler
  [axis angle]
  (let [eps 0.00001
        angle (utils/to-rad angle)
        ah (/ angle 2.0)
        [x y z] (utils/normalize axis)]
    (cond
      (and (pos? z) (< (utils/abs (- 1 z)) eps)) ;; points up
      (mapv utils/to-deg
            [0
             (/ Math/PI 2)
             (* 2 (Math/atan2 (* x (Math/sin ah)) (Math/cos ah)))])
      
      (and (neg? z) (< (utils/abs (- 1 z)) eps)) ;; points down
      (mapv utils/to-deg
            [0
             (/ Math/PI -2)
             (* -2 (Math/atan2 (* x (Math/sin ah)) (Math/cos ah)))])
      
      :else
      (mapv utils/to-deg
            [(Math/atan2
              (- (* x (Math/sin angle)) (* y z (- 1 (Math/cos angle))))
              (- 1 (* (+ (* x x) (* z z)) (- 1 (Math/cos angle)))))
             (Math/asin (+ (* x y (- 1 (Math/cos angle))) (* z (Math/sin angle))))
             (Math/atan2
              (- (* y (Math/sin angle)) (* x z (- 1 (Math/cos angle))))
              (- 1 (* (+ (* y y) (* z z)) (- 1 (Math/cos angle)))))]))))

(defn axis-angle->quaternion
  [axis angle]
  (let [[ax ay az] (utils/normalize axis)
        ha (/ (utils/to-rad angle) 2.0)
        qx (* ax (Math/sin ha))
        qy (* ay (Math/sin ha))
        qz (* az (Math/sin ha))
        qw (Math/cos ha)]
    (utils/normalize [qx qy qz qw])))

;; heading = Z
;; attitude = Y
;; bank = X
;; my own convention, probably should change and/or document it better
;; ZYX is application order, but vector returns [ax ay az] which is fed into (rotate ...)

(defn quaternion->euler
  [[qx qy qz qw]]
  (let [eps 0.00001
        pole (+ (* qx qy) (* qz qw))]
    (cond
      (< (utils/abs (- pole 0.5)) eps) ;; pole close to 0.5 is N
      (mapv utils/to-deg
            [0
             (/ Math/PI 2)
             (* 2 (Math/atan2 qx qw))])

      (< (utils/abs (+ pole 0.5)) eps) ;; pole close to -0.5 is S
      (mapv utils/to-deg
            [0
             (/ Math/PI -2)
             (* -2 (Math/atan2 qx qw))])

      :else
      (mapv utils/to-deg
            [(Math/atan2 (- (* 2 qx qw) (* 2 qy qz)) (- 1 (* 2 qx qx) (* 2 qz qz)))
             (Math/asin (+ (* 2 qx qy) (* 2 qz qw)))
             (Math/atan2 (- (* 2 qy qw) (* 2 qx qz)) (- 1 (* 2 qy qy) (* 2 qz qz)))]))))

(defn euler
  [va vb]
  (->> (axis-angle va vb)
       (apply axis-angle->quaternion)
       quaternion->euler))

;; http://www.euclideanspace.com/maths/geometry/rotations/conversions/quaternionToAngle/index.htm
(defn quaternion->axis-angle
  [q]
  (let [eps 0.00001
        [qx qy qz qw] (utils/normalize q)
        angle (utils/to-deg (* 2 (Math/acos qw)))
        s (Math/sqrt (- 1 (* qw qw)))]
    (if (< s eps)
      #_[[qx qy qz] angle] [[1 0 0] angle]
      [[(/ qx s) (/ qy s) (/ qz s)] angle])))

(defn rotate-pt-aa
  [pt axis angle]
  (let [angle (utils/to-rad angle)
        axis (utils/normalize axis)
        d (utils/v* (repeat 3 (utils/dot* axis pt)) axis)
        r (utils/v- pt d)
        rp (utils/v+ (utils/v* r (repeat 3 (Math/cos angle)))
                     (utils/v* (utils/cross* axis r) (repeat 3 (Math/sin angle))))]
    (utils/v+ d rp)))

(defn extrude-along
  ([xs path] (extrude-along xs path [0 0 1]))
  ([xs path up]
   (let [xs (c/to-3D xs)
         path (c/to-3D path)
         data (path)
         origin (:origin data)]
     (fn
       ([] {:fn `extrude-along
            :input [xs path up]
            :origin origin})
       ([u v]
        (let [pt (utils/add-z (xs v))
              path-pt (utils/add-z (path u))
              path-tangent (c/tangent path u)
              path-normal (c/normal path u up)
              xs-normal [0 0 1]
              q (look-at xs-normal path-tangent path-normal)
              xpt (-> pt
                      (transform-pt-quaternion q)
                      (utils/v+ path-pt))]
          xpt))))))

(defn scale
  [f [sx sy sz]]
  (let [data (f)
        ctr (:origin data)]
    (fn
      ([] (merge data
                 {:fn `scale
                  :input [f [sx sy sz]]}))
      ([u v]
       (utils/scale-pt-from-center (f u v) [sx sy sz] ctr)))))

(defn normal
  [surface [u v]]
  (let [eps 0.00001
        uvs (->> (utils/regular-polygon-pts eps 3)
                 (map #(utils/v+ % [u v])))
        pts (map #(apply surface %) uvs)]
    (utils/normalize (apply utils/normal pts))))

(defn offset
  [surf t]
  (let []
    (fn
      ([] {:fn `offset
           :input [surf t]
           :origin (surf 0.5 0.5)})
      ([u v]
       (let [pt (surf u v)
             n (normal surf [u v])
             v (utils/v* n (repeat t))]
         (utils/v+ pt v))))))
