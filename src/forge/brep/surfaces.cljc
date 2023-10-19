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
        diffs (map #(abs (- %1 %2)) a b)]
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

(defn surface-along*
  [curve-a curve-b]
  (fn
    ([] {})
    ([u v]
     (utils/v+ (curve-a u) (curve-b v)))))

(defn surface-between*
  [curve-a curve-b]
  (fn
    ([] {})
    ([u v]
     (let [l (c/line (curve-a u) (curve-b u))]
       (l v)))))

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
         curves (map c/bezier cpt-sets)]
       (fn [u v]
         (let [curve (c/bezier (map #(% u) curves))]
           (curve v))))))

(defn bezier-patch
  ([apts bpts cpts] (bezier-patch apts bpts cpts nil))
  ([apts bpts cpts dpts]
   (let [cpt-sets (remove nil? [apts bpts cpts dpts])
         curves (map c/bezier cpt-sets)
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
         vv (utils/normalize (utils/v* (utils/normalize direction) (repeat 3 d)))]
     (fn
       ([] {:fn `extrude-curve
            :input [curve direction d]
            :origin (:origin (curve))})
       ([u v]
        (utils/v+ (curve u) (utils/v* vv (repeat 3 (* d v)))))))))

(defn rect
  [w h]
  (let [[wh hh] (map #(/ % 2.0) [w h])
        uline (c/line [(- wh) (- hh)] [(+ wh) (- hh)])
        vline (c/line [(- wh) (- hh)] [(- wh) (+ hh)])]
    (fn
      ([] {:fn `rect
           :input [w h]
           :origin [(first (uline 0.5)) (second (vline 0.5)) 0]})
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
  (let [ctr (utils/centroid-of-pts [a b c])
        perim (c/polygon [a b c])]
    (fn
      ([] {:fn `triangle
           :input [a b c]
           :origin ctr})
      ([u v]
       (let [rad (c/line ctr (perim u))]
         (rad v))))))

(defn sphere
  [r]
  (fn
    ([] {:fn `sphere
         :input [r]
         :origin [0 0 0]})
    ([u v]
     (let [[u v] (map #(* 2 Math/PI %) [u v])
           x (* r (Math/sin u) (Math/cos v))
           y (* r (Math/sin u) (Math/sin v))
           z (* r (Math/cos u))]
       [x y z]))))

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
           #_(utils/v+ (map - ctr))
           (utils/rotate-pt [ax ay az])
           #_(utils/v+ ctr))))))

(defn extrude-along
  ([xs path] (extrude-along xs path (c/find-up path)))
  ([xs path up]
   (let [xs (c/to-3D xs)
         path (c/to-3D path)
         data (path)
         origin (:origin data)]
     (fn
       ([] {:fn `extrude-along
            :input [xs path up]
            :origin origin
            :dimension 3})
       ([u v]
        (let [path-tangent (c/tangent path v)
              m (utils/look-at-matrix (path v) path-tangent up)]
          (-> (xs u)
              (utils/transform-pt-matrix m))))))))

(defn extrude-fn-along
  ([xsfn path] (extrude-fn-along xsfn path (c/find-up path)))
  ([xsfn path up]
   (let [path (c/to-3D path)
         data (path)
         origin (:origin data)]
     (fn
       ([] {:fn `extrude-fn-along
            :input [xsfn path up]
            :origin origin
            :dimension 3})
       ([u v]
        (let [xs (xsfn v)
              path-tangent (c/tangent path v)
              m (utils/look-at-matrix (path v) path-tangent up)]
          (-> (xs u)
              (utils/transform-pt-matrix m))))))))

#_(defn revolve
  ([xs] (revolve xs [[0 0 0] [0 0 1]]))
  ([xs [a b :as axis]]
   (let [up (utils/v- b a)
         path (-> (c/circle 2)
                  c/to-3D
                  (c/translate a)
                  (c/look-at b up))
         xs (c/to-3D xs)
         data (path)
         origin (:origin data)
         nup (c/find-up path)]
     (fn
       ([] {:fn `revolve
            :input [xs axis]
            :origin origin
            :dimension 3})
       ([u v]
        (let [path-tangent (c/tangent path v)
              m (utils/look-at-matrix (path v) path-tangent nup)]
          (-> (xs u)
              (utils/transform-pt-matrix m)
              (utils/v- (path v)))))))))

(defn revolve
  ([xs] (revolve xs [[0 0 0] [0 0 1]]))
  ([xs [a b :as axis]]
   (let [up (utils/v- b a)
         xs (-> xs
                c/to-3D
                #_(c/rotate [0 90 0])
                (c/translate a)
                (c/look-at b up))
         data (xs)
         origin (:origin data)]
     (fn
       ([] {:fn `revolve
            :input [xs axis]
            :origin origin
            :dimension 3})
       ([u v]
        (let [angle (* v 360)]
          (-> (xs u)
              (utils/rotate-pt-aa (utils/v- b a) angle))))))))

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
  (fn
    ([] {:fn `offset
         :input [surf t]
         :origin (surf 0.5 0.5)})
    ([u v]
     (let [pt (surf u v)
           n (normal surf [u v])
           v (utils/v* n (repeat t))]
       (utils/v+ pt v)))))
