(ns forge.utils
  (:require [clojure.string :as str]
            [same :refer [ish? zeroish?]]))

(defn nearly?
  "compare two float values for approximate equality.

   Default epsilon = 0.00001"
  ([^double a ^double b]
   (nearly? a b 0.00001))

  ([^double a ^double b ^double epsilon]
   (let [a (float a)
         b (float b)]
     (if (= a b)
       true
       (let [absA (Math/abs a)
             absB (Math/abs b)
             diff (Math/abs (- a b))
             min-f 1.17549435E-38
             max-f 3.4028235E38]
         (if (or (= a 0.0)
                 (= b 0.0)
                 (< (+ absA absB) min-f))
           (< diff (* epsilon min-f))
           (< (/ diff (Math/min (+ absA absB) max-f)) epsilon)))))))

(defn all-nearly?
  "check if all numbers are nearly equal to one another in two collections"
  [coll-a coll-b]
  (if (not (= (count coll-a) (count coll-b)))
    #?(:clj  (throw (Exception. "collections must be same size."))
       :cljs (throw (js/Error. "collections must be same size.")))
    (let [diffs (mapv #(float (Math/abs ^double (- %1 %2))) coll-a coll-b)]
      (empty?
       (filter
        false?
        (map zeroish? diffs))))))

(defn my-zeroish?
  [x]
  (let [eps 0.00001]
    (< (Math/abs x) eps)))

(def v+ (partial mapv +))
(def v- (partial mapv -))
(def v* (partial mapv *))

(def pow #?(:clj #(Math/pow %1 %2) :cljs js/Math.pow))

(defn to-deg
  [rad]
  (* rad (/ 180 Math/PI)))

(defn to-rad
  [deg]
  (* deg (/ Math/PI 180)))

(def ^:dynamic *rounding* nil)
(defn round
  "Rounds a non-integer number `num` to `places` decimal places."
  ([num]
   (round num *rounding*))
  ([num places]
   (if places
     (let [d #?(:clj (bigdec (Math/pow 10 places))
                :cljs (Math/pow 10 places))]
       (double (/ (Math/round (* num d)) d)))
     num)))

(defn sign
  "returns -1 if x is less than 0, 0 if x is 0 and 1 if x is greater"
  [x]
  (cond
    (or (= 0.0 x) (= 0 x)) 0
    (< 0 x) 1
    (> 0 x) -1))

(defn average
  [& numbers]
  (let [n (count numbers)]
    (/ (apply + numbers) n)))

(defn sq
  [x]
  (* x x))

(defn clamp
  "clamps a value between lower bound and upper bound"
  [x lb ub]
  (cond
    (< x lb) lb
    (> x ub) ub
    :else x))

(defn add-z
  [pt]
  (let [dim (count pt)]
    (if (< dim 3)
      (conj (vec pt) 0)
      pt)))

(defn flip-y
  [pts]
  (map #(v* % [1 -1]) pts))

(defn vec-diff
  "returns the collection difference of two vectors"
  [v1 v2]
  (vec (into #{} (concat v1 v2))))

(defn vec-inner-pop
  "pop the item at index from the collection"
  [coll ind]
  (into [] (conj (subvec coll 0 ind) (subvec coll (inc ind)))))

(defn push-new
  "push item onto vector if it doesn't already exist."
  [v n]
  (if (some #{n} v)
    v
    (conj v n)))

(defn concat-new
  "concat two vectors together pushing only unique items."
  [a b]
  (reduce push-new a b))

#_(defn position
  "returns index of first match to item in the src vector. Otherwise nil"
  [src item]
  (let [res (.indexOf src item)]
    (when (>= res 0)
      res)))

#_(defn link
  "Swap an item for it's index in a different list"
  [src item]
  (let [type-k (first item)
        vals (rest item)]
    (vec (conj (map #(position src %) vals) type-k))))

(defn unlink
  "Swap an item's idx for it's value in a different list"
  [src item]
  (let [type-k (first item)
        indices (rest item)]
    (vec (conj (map #((vec src) %) indices) type-k))))

(defn distance
  "compute distance between two points"
  [a b]
  (let [v (v- b a)
        v2 (reduce + (v* v v))]
    (Math/sqrt ^double v2)))

(defn distance-squared
  [a b]
  (let [v (v- b a)]
    (reduce + (v* v v))))

(defn slope
  [[ax ay] [bx by]]
  (/ (- by ay) (- bx ax)))

(defn perpendicular
  [[x y]]
  [(- y) x])

(defn determinant
  [[ax ay] [bx by]]
  (- (* ax by)
     (* ay bx)))

(defn dot*
  "calculates the dot product of two vectors"
  [a b]
  (reduce + (map * a b)))

(defn cross*
  "calculates cross product of two 3d-vectors"
  [a b]
  (let [[a1 a2 a3] a
        [b1 b2 b3] b
        i (- (* a2 b3) (* a3 b2))
        j (- (* a3 b1) (* a1 b3))
        k (- (* a1 b2) (* a2 b1))]
    [i j k]))

(defn cross*-k
  "Calculates the k component of the cross product of two 2D vectors, assuming Z=0 as the 3rd component."
  [[ax ay] [bx by]]
  (- (* ax by) (* ay bx)))

(defn normal
  "Calculates the normal vector of plane given 3 points or calculates the normal vector of a line given two (2D) points."
  ([a b]
   (let [[x1 y1] a
         [x2 y2] b
         dx (- x2 x1)
         dy (- y2 y1)]
     [(- dy) dx]))
  ([a b c]
   (let [eps 0.00001
         ab (v- a b)
         ac (v- a c)
         [x y z] (cross* (add-z ab) (add-z ac))]
     (when true #_(or (> (abs x) eps) (> (abs y) eps) (> (abs z) eps))
       [x y z]))))

(defn normalize
  "find the unit vector of a given vector"
  [v]
  (when v
    (let [m (Math/sqrt ^double (reduce + (v* v v)))]
      (mapv / v (repeat m)))))

(defn sin-cos-pair [theta]
  [(Math/sin ^long (to-rad theta))
   (Math/cos ^long (to-rad theta))])

(defn rot-pt-2d
  [[x y] theta]
  (let [[s-t c-t] (sin-cos-pair theta)]
    [(- (* x c-t) (* y s-t))
     (+ (* y c-t) (* x s-t))]))

;; this rotates a point around [0,0,0]
(defn rot-pt
  [[x y z] axis theta]
  (cond
    (= axis :x) (into [x] (rot-pt-2d [y z] theta))
    (= axis :y) (apply #(into [] [%2 y %1]) (rot-pt-2d [z x] theta))
    (= axis :z) (into (rot-pt-2d [x y] theta) [z])))

(defn rotate-pt
  [pt [ax ay az]]
  (let [pt (if (< (count pt) 3)
             (conj pt 0)
             pt)]
    (-> pt
        (rot-pt :z az)
        (rot-pt :y ay)
        (rot-pt :x ax))))

(defn scale-pt
  [pt [sx sy sz]]
  (v* pt [sx sy sz]))

(defn scale-pt-from-center
  [[x y z] [sx sy sz] [cx cy cz]]
  [(+ (* (- x cx) sx) cx)
   (+ (* (- y cy) sy) cy)
   (+ (* (- z cz) sz) cz)])

(defn regular-polygon-pts
  [r n]
  (let [angle (* 2 Math/PI (/ 1 n))]
    (for [step (range n)]
      [(* r (Math/cos (* step angle)))
       (* r (Math/sin (* step angle)))])))

(defn random-pts
  [w h n]
  (let [[wh hh] (map #(/ % 2) [w h])
        f #(vector (rand-int w) (rand-int h))]
    (->> (repeatedly n f)
         (map #(v- % [wh hh])))))

(defn on-line?
  "determine if a point is on a capped line"
  [pt line]
  (let [[a b] line
        ap (add-z (v- a pt))
        bp (add-z (v- b pt))]
    (if (or (all-nearly? pt a)
            (all-nearly? pt b))
      true
      (let [na (normalize ap)
            nb (normalize bp)]
        (and
         (all-nearly? (cross* ap bp) [0 0 0])
         (not (all-nearly? na nb)))))))

(defn on-line-inf?
  "determine if a point is on an infinitely extending line"
  [pt line]
  (let [[a b] line
        ap (add-z (v- a pt))
        bp (add-z (v- b pt))]
    (all-nearly? (cross* ap bp) [0 0 0])))

(defn edges
  [pts]
  (vec (partition 2 1 (concat pts [(first pts)]))))

(defn centroid-of-pts
  "Calculates the arithmetic mean position of the given `pts`."
  [pts]
  (let [ndim (count (first (sort-by count pts)))
        splits (for [axis (range 0 ndim)]
                 (map #(nth % axis) pts))]
    (mapv #(apply average %) splits)))

(defn bounds-of-pts
  "Calculates the axis-aligned-bounding-box of 2D points, `pts`."
  [pts]
  (let [xmax (apply max (map first pts))
        ymax (apply max (map second pts))
        xmin (apply min (map first pts))
        ymin (apply min (map second pts))]
    (vector [xmin ymin]
            [xmax ymin]
            [xmax ymax]
            [xmin ymax])))

(defn bb-dims
  [pts]
  (let [[[xmin ymin] _ [xmax ymax] _] (bounds-of-pts pts)]
    [(- xmax xmin) (- ymax ymin)]))

(defn radius-from-pts
  "compute the radius of an arc defined by 3 points"
  [p1 p2 p3]
  (when-not (on-line? p1 [p2 p3])
    (let [a (distance p3 p2)
          b (distance p3 p1)
          c (distance p2 p1)
          s (/ (+ a b c) 2)
          sa ( - s a)
          sb ( - s b)
          sc ( - s c)
          rt (Math/sqrt ^double (* s sa sb sc))
          radius (/ (/ (* a b c) 4) rt)]
      radius)))

;; https://math.stackexchange.com/questions/361412/finding-the-angle-between-three-points
(defn angle-from-pts-old
  [p1 p2 p3]
  (let [eps 0.00001
        v1 (v- p2 p1)
        v2 (v- p2 p3)
        l1 (distance p1 p2)
        l2 (distance p3 p2)
        n (dot* v1 v2)
        d (Math/abs ^double (* l1 l2))]
    (when (> d eps)
      (to-deg (Math/acos ^double (/ n d))))))

;; https://math.stackexchange.com/questions/361412/finding-the-angle-between-three-points
(defn- check-quadrants
  "Using `p2` as the 'origin', return a string indicating positive, negative, or axis-aligned for p1 p2."
  [p1 p2 p3]
  (let [v1 (v- p1 p2)
        v2 (v- p3 p2)
        qf (fn [[x y]]
             (cond (and (pos? x) (pos? y)) "pp"
                   (and (pos? x) (neg? y)) "pn"
                   (and (neg? x) (neg? y)) "nn"
                   (and (neg? x) (pos? y)) "np"
                   (pos? x) "p_"
                   (neg? x) "n_"
                   (pos? y) "_p"
                   (neg? y) "_n"))]
    (apply str (map qf [v1 v2]))))

(defn angle-from-pts
  "Calculates the angle starting at line p3p2 going to line p1p2.
Put another way, the angle is measured following the 'right hand rule' around p2."
  [p1 p2 p3]
  (let [v1 (v- p1 p2)
        v2 (v- p3 p2)
        [v1nx v1ny] (normalize v1)
        [v2nx v2ny] (normalize v2)
        l1 (distance p1 p2)
        l2 (distance p3 p2)
        n (dot* v1 v2)
        d (* l1 l2)]
    (when-not (zeroish? (float d))
      (let [a (to-deg (Math/acos (/ n d)))
            quadrants (check-quadrants p1 p2 p3)]
        (cond
          ;; same quadrant, checking if V2 is before or after V1
          (and (= "pppp" quadrants) (> v2nx v1nx)) a
          (and (= "npnp" quadrants) (> v2nx v1nx)) a
          (and (= "nnnn" quadrants) (< v2nx v1nx)) a
          (and (= "pnpn" quadrants) (< v2nx v1nx)) a
          ;; within same quadrant
          (#{"p_p_" "ppp_" "_ppp" "p_pn"} quadrants) a
          (#{"_p_p" "np_p" "n_np"} quadrants) a
          (#{"n_n_" "nnn_" "_nnn"} quadrants) a
          (#{"_n_n" "pn_n" "pnp_"} quadrants) a
          ;; one quadrant away
          (#{"npp_" "nn_p" "pnn_" "pp_n"} quadrants) a
          (#{"n_pp" "_nnp" "p_nn" "_ppn"} quadrants) a
          (#{"nppp" "nnnp" "pnnn" "pppn"} quadrants) a
          ;; 90 degrees away on axes
          (#{"_pp_" "n__p" "_nn_" "p__n"} quadrants) a
          ;; two quadrants away
          (and (= "ppnn" quadrants) (> (Math/abs v1nx) (Math/abs v2nx))) a
          (and (= "nnpp" quadrants) (> (Math/abs v1nx) (Math/abs v2nx))) a
          (and (= "pnnp" quadrants) (< (Math/abs v1nx) (Math/abs v2nx))) a
          (and (= "nppn" quadrants) (< (Math/abs v1nx) (Math/abs v2nx))) a
          ;; 180 degrees away on axes
          (#{"p_n_" "_p_n" "n_p_" "_n_p"} quadrants) a
          :else (- 360 a))))))

;; https://math.stackexchange.com/a/1743505
(defn arc-center-from-pts
  "compute the center point of an arc through 3 points"
  [p1 p2 p3]
  (when-not (on-line? p1 [p2 p3])
    (let [u1 (mapv - p2 p1)
          u2 (mapv - p3 p1)
          w1 (cross* (mapv - p3 p1) u1)
          u (normalize u1)
          w (normalize w1)
          v (cross* w u)
          [bx by] [(dot* u1 u) 0]
          [cx cy] [(dot* u2 u) (dot* u2 v)]
          h (/ (+ (sq (- cx (/ bx 2))) (sq cy) (- 0 (sq (/ bx 2))))
               (* 2 cy))]
      (v+ p1
          (v* (repeat (/ bx 2)) u)
          (v* (repeat h) v)))))

(defn corner-condition
  [a b c]
  (let [ba (v- a b)
        bc (v- c b)
        eps 0.000001
        k (cross*-k ba bc)]
    (cond
      (> eps (abs k)) :colinear
      (< eps k) :reflex
      (> (- eps) k) :convex)))

(defn on-plane?
  "determine if a point is on a plane"
  [pt plane]
  (let [a (map - pt (first plane))
        n (normal (first plane) (second plane) (nth plane 2))]
    (nearly? (Math/abs ^long (dot* a n)) 0)))

(defn plane-triple
  "gets a list of 3 points on a plane from a list of points"
  [pts]
  (if (< (count pts) 3) nil
    (let [[a b c] pts]
      (if (on-line? c [a b])
        (plane-triple (vec-inner-pop pts 2))
        [a b c]))))

(defn planar?
  "checks if all points in a list share a plane"
  [pts]
  (cond
    (< (count pts) 3) false
    (= (count pts) 3) (let [a (first pts)
                            b (second pts)
                            c (nth pts 2)]
                        (if (on-line? c [a b]) false true))
    :else (let [plane (plane-triple pts)
                chk-pts (vec-diff plane pts)]
            (every? #(= % true) (mapv #(on-plane? % plane) chk-pts)))))

(defn colinear?
  [a b c]
  (let [ba (v- a b)
        bc (v- c b)
        eps 0.000001]
    (> eps (abs (cross*-k ba bc)))))

;; https://youtu.be/hTJFcHutls8?t=1473
;; use k component from cross product to quickly check if vector
;; is on right or left of another vector
;; check each triangle edge vector against corner to pt vectors
(defn pt-inside?
  [[a b c] pt]
  (let [ab (v- b a)
        bc (v- c b)
        ca (v- a c)
        apt (v- pt a)
        bpt (v- pt b)
        cpt (v- pt c)]
    (not
     (or (<= (cross*-k ab apt) 0)
         (<= (cross*-k bc bpt) 0)
         (<= (cross*-k ca cpt) 0)))))

(defn pt-on-perimeter?
  [[a b c] pt]
  (some true? (map #(on-line? pt %) (partition 2 1 [a b c a]))))

(defn axis-angle
  [va vb]
  (let [angle (-> (dot* (normalize va) (normalize vb))
                  Math/acos
                  to-deg)
        axis (normalize (cross* va vb))]
    [axis angle]))

(defn axis-angle->euler
  [axis angle]
  (let [eps 0.00001
        angle (to-rad angle)
        ah (/ angle 2.0)
        [x y z] (normalize axis)]
    (cond
      (and (pos? z) (< (abs (- 1 z)) eps)) ;; points up
      (mapv to-deg
            [0
             (/ Math/PI 2)
             (* 2 (Math/atan2 (* x (Math/sin ah)) (Math/cos ah)))])

      (and (neg? z) (< (abs (- 1 z)) eps)) ;; points down
      (mapv to-deg
            [0
             (/ Math/PI -2)
             (* -2 (Math/atan2 (* x (Math/sin ah)) (Math/cos ah)))])

      :else
      (mapv to-deg
            [(Math/atan2
              (- (* x (Math/sin angle)) (* y z (- 1 (Math/cos angle))))
              (- 1 (* (+ (* x x) (* z z)) (- 1 (Math/cos angle)))))
             (Math/asin (+ (* x y (- 1 (Math/cos angle))) (* z (Math/sin angle))))
             (Math/atan2
              (- (* y (Math/sin angle)) (* x z (- 1 (Math/cos angle))))
              (- 1 (* (+ (* y y) (* z z)) (- 1 (Math/cos angle)))))]))))

(defn axis-angle->quaternion
  [axis angle]
  (let [[ax ay az] (normalize axis)
        ha (/ (to-rad angle) 2.0)
        qx (* ax (Math/sin ha))
        qy (* ay (Math/sin ha))
        qz (* az (Math/sin ha))
        qw (Math/cos ha)]
    (normalize [qx qy qz qw])))

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
      (< (abs (- pole 0.5)) eps) ;; pole close to 0.5 is N
      (mapv to-deg
            [0
             (/ Math/PI 2)
             (* 2 (Math/atan2 qx qw))])

      (< (abs (+ pole 0.5)) eps) ;; pole close to -0.5 is S
      (mapv to-deg
            [0
             (/ Math/PI -2)
             (* -2 (Math/atan2 qx qw))])

      :else
      (mapv to-deg
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
        [qx qy qz qw] (normalize q)
        angle (to-deg (* 2 (Math/acos qw)))
        s (Math/sqrt (- 1 (* qw qw)))]
    (if (< s eps)
      #_[[qx qy qz] angle] [[1 0 0] angle]
      [[(/ qx s) (/ qy s) (/ qz s)] angle])))

(defn rotate-pt-aa
  [pt axis angle]
  (let [angle (to-rad angle)
        axis (normalize axis)
        d (v* (repeat 3 (dot* axis pt)) axis)
        r (v- pt d)
        rp (v+ (v* r (repeat 3 (Math/cos angle)))
                     (v* (cross* axis r) (repeat 3 (Math/sin angle))))]
    (v+ d rp)))

;; https://stackoverflow.com/a/52551983
(defn look-at-quaternion
  [from to world-up]
  (let [world-up (normalize world-up)
        [fx fy fz :as forward] (normalize (v- from to))
        [rx ry rz :as right] (normalize (cross* world-up forward))
        [ux uy uz :as up] (cross* forward right)
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

(defn axis-angle->quaternion
  [axis angle]
  (let [[ax ay az] (normalize axis)
        ha (/ (to-rad angle) 2.0)
        qx (* ax (Math/sin ha))
        qy (* ay (Math/sin ha))
        qz (* az (Math/sin ha))
        qw (Math/cos ha)]
    (normalize [qx qy qz qw])))

;; http://www.euclideanspace.com/maths/algebra/vectors/lookat/index.htm
(defn look-at-axis-angle
  [pt eye current target world-up]
  (let [world-up (normalize world-up)
        ncurrent (normalize (v- current eye))
        ntarget  (normalize (v- target eye))
        axis (cross* ncurrent ntarget)
        a1 (to-deg (dot* ncurrent ntarget))
        [a b c] target
        axis2 [(* (- a) b) (+ (* a a) (* c c)) (* (- c) b)]
        a2 (to-deg (Math/acos (dot* world-up axis2)))
        q1 (axis-angle->quaternion axis a1)
        q2 (axis-angle->quaternion axis2 a2)]
    (-> pt
        (transform-pt-quaternion q2)
        (transform-pt-quaternion q1))))

(defn- grc
  [m [r c]]
  (get-in m [c r]))

(defn transform-pt-matrix
  [pt m]
  (let [eps 0.00001
        [px py pz] pt
        x (+ (* px (grc m [0 0])) (* py (grc m [0 1])) (* pz (grc m [0 2])) (grc m [0 3]))
        y (+ (* px (grc m [1 0])) (* py (grc m [1 1])) (* pz (grc m [1 2])) (grc m [1 3]))
        z (+ (* px (grc m [2 0])) (* py (grc m [2 1])) (* pz (grc m [2 2])) (grc m [2 3]))
        w (+ (* px (grc m [3 0])) (* py (grc m [3 1])) (* pz (grc m [3 2])) (grc m [3 3]))]
    (if (and (not (zeroish? (- w 1)))
             (not (zeroish? w)))
      [(double (/ x w)) (double (/ y w)) (double (/ z w))]
      [x y z])))

(defn look-at-matrix
  [from to world-up]
  (let [w (normalize to)
        u (normalize (cross* world-up w))
        v (normalize (cross* w u))]
    (mapv vec [(concat u [0])
               (concat v [0])
               (concat w [0])
               (concat from [1])])))

(defn ext
  [fname]
  (last (str/split fname #"\.")))
