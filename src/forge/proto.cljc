(ns forge.proto
  (:require [forge.delaunay :as delaunay]
            [same :refer [ish? zeroish?]]))

(defn nearly?
  "compare two float values for approximate equality.
   
   Default epsilon = 0.00001"
  ([a b]
   (nearly? a b 0.00001))

  ([a b epsilon]
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
    (let [diffs (mapv #(float (Math/abs (- %1 %2))) coll-a coll-b)]
      (empty? 
       (filter 
        false? 
        (map zeroish? diffs))))))

(defn to-deg
  [rad]
  (* rad (/ 180 Math/PI)))

(defn to-rad
  [deg]
  (* deg (/ Math/PI 180)))

(defn round
  [num places]
  (let [d (Math/pow 10 places)]
    (/ (Math/round (* num d)) d)))

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
  (conj (vec pt) 0))

(def v+ (partial mapv +))
(def v- (partial mapv -))
(def v* (partial mapv *))

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

(defn position
  "returns index of first match to item in the src vector. Otherwise nil"
  [src item]
  (let [res (.indexOf src item)]
    (if (>= res 0)
      res
      nil)))

(defn link
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

(defn slope-2d
  [a b]
  (let [[x1 y1] a
        [x2 y2] b]
    (/ (- y2 y1) (- x2 x1))))

(defn perpendicular-2d
  [[x y]]
  [(- y) x])

(defn determinant-2d
  [a b]
  (- (* (first a) (second b))
     (* (second a) (first b))))

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

(defn distance
  "compute distance between two points"
  [a b]
  (let [v (v- b a)
        v2 (reduce + (v* v v))]
    (Math/sqrt v2)))

(defn normalize
  "find the unit vector of a given vector"
  [v]
  (let [m (Math/sqrt (reduce + (v* v v)))]
    (mapv / v (repeat m))))

(defn on-line?
  "determine if a point is on a capped line"
  [pt line]
  (let [[a b] line
        ap (mapv - a pt)
        bp (mapv - b pt)]
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
        ap (mapv - a pt)
        bp (mapv - b pt)]
    (all-nearly? (cross* ap bp) [0 0 0])))

(defn normal
  "find normal vector of plane given 3 points"
  [a b c]
  (let [ab (mapv - a b)
        ac (mapv - a c)]
    (if (on-line? c [a b]) nil (cross* ab ac))))

(defn on-plane?
  "determine if a point is on a plane"
  [pt plane]
  (let [a (map - pt (first plane))
        n (normal (first plane) (second plane) (nth plane 2))]
    (nearly? (Math/abs (dot* a n)) 0)))

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
          rt (Math/sqrt (* s sa sb sc))
          R (/ (/ (* a b c) 4) rt)]
      R)))

;; https://math.stackexchange.com/questions/361412/finding-the-angle-between-three-points
(defn angle-from-pts
  [p1 p2 p3]
  (let [v1 (v- p1 p2)
        v2 (v- p3 p2)
        l1 (distance p1 p2)
        l2 (distance p3 p2)
        n (dot* v1 v2)
        d (Math/abs (* l1 l2))]
    (to-deg (Math/acos (/ n d)))))

;; https://math.stackexchange.com/a/1743505
(defn center-from-pts
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

(defn triangle-area
  "compute the area of a triangle defined by three points"
  [a b c]
  ;; use Heron's formula
  (let [la (distance b c)
        lb (distance a c)
        lc (distance a b)
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
    (mapv #(apply average %) splits)))

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
    (reduce + (map (partial apply distance) pts))))

;; this breaks somewhat often. Consider a case where the match is
;; a very tiny difference in parameter space. It's easy to pass over it
;; there's surely a more refined approach, perhaps a combination of several algorithms?
(defn close?
  [p1 p2]
  (let [[x1 y1 z1] p1
        [x2 y2 z2] p2]
    (and (nearly? (+ 1 (Math/abs (- x1 x2))) 1.0)
         (nearly? (+ 1 (Math/abs (- y1 y2))) 1.0)
         (nearly? (+ 1 (Math/abs (- z1 z2))) 1.0))))

(defn estimate-parameter
  [f pt step]
  (let [[x y z] pt
        samples (into [] (range 0 (+ 1 step) step))
        pts (mapv f samples)
        close? (partial close? pt)]
    (get samples
         (count (take-while #(not (= (first (filter close? pts)) %)) pts)))))

(defn remap-within
  [f [start end] x]
  (when (and (>= x start) (< x end))
    (let [step (- end start)
          t (/ (- x start) step)]
      (f t))))

(defn frep-voxel-grid-proto
  [[minc maxc] [xs ys zs]]
  (for [x (range (first minc) (first maxc) xs)
        y (range (second minc) (second maxc) ys)
        z (range (last minc) (last maxc) zs)]
    [x y z]))

(defn on-line?
  "determine if a point is on a capped line"
  [pt line]
  (let [[a b] line
        ap (v- a pt)
        bp (v- b pt)]
    (if (or (all-nearly? pt a) 
            (all-nearly? pt b))
      true
      (let [na (normalize ap)
            nb (normalize bp)]
        (and 
         (all-nearly? (cross* ap bp) [0 0 0]) 
         (not (all-nearly? na nb)))))))

(defn fake-on-line?
  "determine if a point is on a capped line"
  [pt line]
  (let [[a b] line
        ap (v- a pt)
        bp (v- b pt)]
    (let [na (normalize ap)
          nb (normalize bp)]
      (cross* na nb) #_(and 
       (all-nearly? (cross* ap bp) [0 0 0]) 
       (not (all-nearly? na nb))))))

(defn line-intersection
  [[a b] [c d]]
  (let [[ax ay] a
        [bx by] b
        [cx cy] c
        [dx dy] d
        xdiff [(- ax bx) (- cx dx)]
        ydiff [(- ay by) (- cy dy)]
        div (determinant-2d xdiff ydiff)]
    (when (not (zeroish? (Math/abs div))) 
      (let [d [(determinant-2d a b) (determinant-2d c d)]
            x (/ (determinant-2d d xdiff) div)
            y (/ (determinant-2d d ydiff) div)]
        [x y]))))

(defn line-segment-intersection
  [[a b] [c d]]
  (let [pt (line-intersection [a b] [c d])]
    (when (and pt
               (on-line? (add-z pt) (mapv add-z [a b]))
               (on-line? (add-z pt) (mapv add-z [c d])))
      pt)))

(defn identical-polygons?
  [pga pgb]
  (= (into #{} pga)
     (into #{} pgb)))

(declare close-path)
(defn polygon-intersection
  [pga pgb]
  (when (not (identical-polygons? pga pgb))
    (let [lines-a (partition 2 1 (close-path pga))
          lines-b (partition 2 1 (close-path pgb))
          s (for [la lines-a
                  lb lines-b]
              (line-segment-intersection la lb))]
      (->> s
           (filter (complement nil?))
           (into #{})
           (vec)))))

(defn pt-inside-convex?
  [pts pt]
  (let [m (mapv float (midpoint pts))]
    (even? (count (polygon-intersection pts [m pt])))))


;; can I use reduce instead?
;; other recursion scheme?
(defn clip-ears
  ([pts]
   (clip-ears pts []))
  
  ([pts tris]
   (let [tri (into [] (take 3 pts))
         keep [(first tri) (last tri)]
         npts (concat keep (into [] (drop 3 pts)))]
     (if (> (count npts) 2)
       (recur npts (conj tris tri))
       (conj tris tri)))))

(defn pt-inside?
  [pts pt]
  (let [tris (clip-ears pts)]
    (->> tris
         (map #(pt-inside-convex? % pt))
         (filter true?)
         (empty?)
         (not))))

(defn remove-inner-pts
  [pga pgb]

(defn frep-union [f g]
  (fn [pt]
    (let [a (f pt)
          b (g pt)]
      (min a b))))

(defn brep-curve-union
  [& curves]
  (let [n (count curves)
        intervals (map #(vector % (inc %)) (range n))]
    (fn [t]
      (let [t (* n t)]
        (cond
          (= (float t) 0.0) ((first curves) 0)
          (= (float t) (float n)) ((last curves) 1)
          :else
          (first
           (filter 
            some?
            (map #(remap-within %1 %2 t) curves intervals))))))))

(defn brep-surface-union
  [& surfaces]
  (let [n (count surfaces)
        intervals (map #(vector % (inc %)) (range n))]
    (fn [u v]
      (let [partial-surfaces (map #(partial % u) surfaces)]
        ((apply brep-curve-union partial-surfaces) v)))))

(defn union
  [shape1 shape2]
  (let [s1 (dissoc shape1 :frep :history)
        s2 (dissoc shape2 :frep :history)]
    (merge
     (merge-with (comp vec concat) s1 s2)
     {:frep (frep-union (:frep shape1) (:frep shape2))
      :history [`(union ~shape1 ~shape2)]})))



(defn frep-difference [f g]
  (fn [pt]
    (let [a (f pt)
          b (* -1 (g pt))]
      (max a b))))

(defn frep-intersection [f g]
  (fn [pt]
    (let [a (f pt)
          b (g pt)]
      (max a b))))

(defn frep-translate
  [f pos]
  (fn [pt]
    (f (v+ pt pos))))

(defn brep-translate
  [f pos]
  (comp #(v+ pos %) f))

(defn translate
  [shape pos]
  (merge
   shape
   {:history (conj (:history shape) `(translate ~shape ~pos))
    :frep (frep-translate (:frep shape) pos)
    :vertices (mapv (partial v+ pos) (:vertices shape))
    :curves (mapv #(brep-translate % pos) (:curves shape))
    :surfaces (mapv #(brep-translate % pos) (:surfaces shape))
    :volumes (mapv #(brep-translate % pos) (:volumes shape))}))

(defn sin-cos-pair [theta]
  [(Math/sin (to-rad theta)) (Math/cos (to-rad theta))])

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

(defn rotate-point
  [pt [ax ay az]]
  (-> pt
      (rot-pt :z az)
      (rot-pt :y ay)
      (rot-pt :x ax)))

(defn brep-rotate
  [f angles]
  (comp #(rotate-point % angles) f))

(defn frep-rotate
  [f angles]
  (fn [pt]
    (f (rotate-point pt angles))))

(defn rotate
  [shape angles]
  (merge
   shape
   {:history (conj (:history shape) `(rotate ~shape ~angles))
    :frep (frep-rotate (:frep shape) angles)
    :vertices (mapv #(rotate-point % angles) (:vertices shape))
    :curves (mapv #(brep-rotate % angles) (:curves shape))
    :surfaces (mapv #(brep-rotate % angles) (:surfaces shape))
    :volumes (mapv #(brep-rotate % angles) (:volumes shape))}))

(defn frep-scale
  [f scales]
  (fn [pt]
    (f (v* pt scales))))

(defn brep-scale
  [f scales]
  (comp #(v* scales %) f))

(defn scale
  [shape scales]
  (merge
   shape
   {:history (conj (:history shape) `(scale ~shape ~scales))
    :frep (frep-scale (:frep shape) scales)
    :vertices (mapv (partial v* scales) (:vertices shape))
    :curves (mapv #(brep-scale % scales) (:curves shape))
    :surfaces (mapv #(brep-scale % scales) (:surfaces shape))
    :volumes (mapv #(brep-scale % scales) (:volumes shape))}))

(defn frep-extrude
  [f h]
  (fn [pt]
    (let [d (f (drop-last pt))
          w [d (- (Math/abs (last pt)) h)]]
      (+ (min (apply max w) 0)
         (distance [0 0]
                   [(max (first w) 0) (max (second w) 0)])))))

(declare brep-line)
(defn brep-curve-extrude
  [c h]
  (fn [u v]
    (let [c2 (brep-line (c u) (v+ (c u) [0 0 h]))]
      (c2 v))))

(defn brep-surface-extrude
  [s h]
  (fn [u v w]
    (let [c1 (brep-line (s u v) (v+ (s u v) [0 0 h]))]
      c1 v)))

(defn extrude
  [shape h]
  (let [vertices (mapv (partial v+ [0 0 h]) (:vertices shape))]
    (merge
     (merge-with
      (comp vec concat)
      shape
      {:history [`(extrude ~shape ~h)]
       :vertices vertices
       :curves (concat
                (mapv #(brep-translate % [0 0 h]) (:curves shape))
                (mapv #(brep-line %1 %2) (:vertices shape) vertices))
       :surfaces (concat
                  [(brep-translate (first (:surfaces shape)) [0 0 h])]
                  (mapv #(brep-curve-extrude % h) (:curves shape)))
       :volumes [(brep-surface-extrude (first (:surfaces shape)) h)]})
     {:frep (frep-extrude (:frep shape) h)})))

(defn brep-curve-straight-sweep
  [c1 c2]
  (fn [u v]
    (v+ (c1 u) (c2 v))))

(defn brep-surface-straight-sweep
  [s c]
  (fn [u v w]
    (v+ (s u v) (c w))))

(defn frep-revolve
  [f r]
  (fn [pt]
    (let [q [(- (distance [0 0] [(first pt) (last pt)]) r) 
             (second pt)]]
      (f q))))

(defn vertex
  [[x y z]]
  {:history [`(vertex [~x ~y ~z])]
   :frep (fn [pt]
           (distance [x y z] pt))
   :vertices [[x y z]]})

(defn frep-line
  [a b]
  (fn [pt]
    (let [pa (map - pt a)
          ba (map - b a)
          h (clamp (/ (dot* pa ba) (dot* ba ba)) 0 1)]
      (distance (map - pa (map * ba (repeat h))) [0 0 0]))))

(defn brep-line
  [a b]
  (fn [t]
    (cond
      (= t :tag) :line
      (= (float t) 0.0) a
      (= (float t) 1.0) b
      :else
      (v+ a (v* (v- b a) (repeat t))))))

(defn line
  [a b]
  {:history [`(line ~a ~b)]
   :frep (frep-line a b)
   :vertices [a b]
   :curves [(brep-line a b)]})

(defn brep-polyline
  [pts]
  (let [step (/ 1.0 (dec (count pts)))
        intervals (partition 2 1 (range 0 (+ 1 step) step))
        lines (map (partial apply brep-line) (partition 2 1 pts))]
    (fn [t]
      (cond 
        (= t :tag) :polyline
        (= (float t) 0.0) (first pts)
        (= (float t) 1.0) (last pts)
        :else
        (first (filter some?
                       (map #(remap-within %1 %2 t) lines intervals)))))))

(defn frep-circle
  [r]
  (fn [pt]
    (- (distance pt [0 0 0]) r)))

;;https://mathforum.org/library/drmath/view/63755.html
(defn brep-curve-circle
  [a b c]
  (let [n (normalize (normal a b c))
        r (radius-from-pts a b c)
        cp (center-from-pts a b c)
        u (normalize (mapv - a cp))
        v (cross* n u)]
    (fn [t]
      (cond
        (= t :tag) :circle
        (or (< t 0.0) (> t 1.0)) nil
        (= (float t) 0.0) a
        (= (float t) 1.0) a
        :else
        (let [t (* 2 Math/PI t)]
          (v+ cp
              (v* (repeat (* r (Math/cos t))) u)
              (v* (repeat (* r (Math/sin t))) v)))))))

(defn brep-surface-circle
  [a b c]
  (let [cp (center-from-pts a b c)
        c1 (brep-curve-circle a b c)]
    (fn [u v]
      (let [c2 (brep-line cp (c1 u))]
        (c2 v)))))

(defn circle
  [r]
  {:history [`(circle ~r)]
   :frep (frep-circle r)
   :vertices [#_[0 0 0] [r 0 0] [0 r 0] [(- r) 0 0] [0 (- r) 0]]
   :curves [(brep-curve-circle [r 0 0] [0 r 0] [(- r) 0 0])]
   :surfaces [(brep-surface-circle [r 0 0] [0 r 0] [(- r) 0 0])]})

(defn brep-curve-ellipse
  [rx ry]
  (fn [t]
    (let [t (* 2 Math/PI t)
          x (* rx (Math/cos t))
          y (* ry (Math/sin t))]
      [x y])))

(defn old-arc
  [a b c]
  (let [cr (brep-curve-circle a b c)
        c-param (estimate-parameter cr c 0.001)]
    (fn [t]
      (cond
        (or (< t 0.0) (> t 1.0)) nil
        (= (float t) 0.0) a
        (= (float t) 1.0) c
        :else
        (let [t (* c-param t)]
          (cr t))))))

(defn brep-curve-arc
  [a b c]
  (let [circle (brep-curve-circle a b c)
        cp (center-from-pts a b c)
        angle (a cp c)]
    (fn [t]
      (let [t (* t (/ angle 360))]
        (circle t)))))

;; this is not correct. ...-straight-sweep does
;; not account for rotating based on path normal
(defn brep-surface-arc
  [a b c]
  (let [cp (center-from-pts a b c)
        c1 (brep-line cp a)
        c2 (brep-curve-arc a b c)]
    (fn [u v]
      ((brep-curve-straight-sweep c1 c2) u v))))

(defn frep-triangle
  [a b c]
  (fn [pt]
    (let [[e0 e1 e2] (map #(apply v- %) [[b a] [c b] [a c]])
          [v0 v1 v2] (map (partial v- pt) [a b c])
          xf (fn [v e] 
               (v- v (map * e (repeat (clamp (/ (dot* v e) (dot* e e)) 0 1)))))
          [pq0 pq1 pq2] (map #(apply xf %) [[v0 e0] [v1 e1] [v2 e2]])
          s (sign (- (* (first e0) (second e2)) (* (second e0) (first e2))))
          d1 (min (dot* pq0 pq0)
                  (dot* pq1 pq1)
                  (dot* pq2 pq2))
          d2 (min (* s (- (* (first v0) (second e0)) (* (second v0) (first e0))))
                  (* s (- (* (first v1) (second e1)) (* (second v1) (first e1))))
                  (* s (- (* (first v2) (second e2)) (* (second v2) (first e2)))))]
      (* -1 (Math/sqrt d1) (sign d2)))))

(defn brep-surface-triangle
  [a b c]
  (let [l1 (brep-line b a)
        l2 (brep-line c a)]
    (fn [u v]
      (let [l3 (brep-line (l1 v) (l2 v))]
        (l3 u)))))

(declare brep-curve-polygon)

(defn triangle
  [a b c]
  {:history [`(triangle ~a ~b ~c)]
   :frep (frep-triangle a b c)
   :vertices [a b c]
   :curves (conj
            (mapv brep-line [a b c] [b c a])
            (brep-curve-polygon [a b c]))
   :surfaces [(brep-surface-triangle a b c)]})

(defn regular-polygon-pts
  [r n]
  (let [angle (* 2 Math/PI (/ 1 n))]
    (for [step (range n)]
      [(* r (Math/cos (* step angle)))
       (* r (Math/sin (* step angle)))
       0])))

(defn frep-polygon
  [pts]
  (let [tris (map
              #(apply frep-triangle %)
              (:triangles (delaunay/triangulate pts)))]
    (reduce frep-union tris)))

(defn brep-curve-polygon
  [pts]
  (brep-polyline (conj (vec pts) (first pts))))

(defn brep-surface-polygon
  [pts]
  (let [xf (fn [pts] (mapv #(conj % 0) pts))
        tris (mapv xf (:triangles (delaunay/triangulate pts)))]
    (apply brep-surface-union (map #(apply brep-surface-triangle %) tris))))

(defn polygon
  [pts]
  {:history [`(polygon ~pts)]
   :frep (frep-polygon pts)
   :vertices (vec pts)
   :curves (mapv 
            #(apply brep-line %) 
            (partition 2 1 (conj (vec pts) (first pts))))
   :surfaces [(brep-surface-polygon pts)]})

(defn close-path
  [path]
  (conj (vec path) (first path)))

(defn path->brep-lines
  [path]
  (mapv #(apply brep-line %) (partition 2 1 path)))

(defn polygon2
  [& paths]
  (let [paths (mapv vec paths)]
    {:history [`(polygon2 ~@paths)]
     :frep nil
     :vertices (apply concat paths)
     :curves (vec (mapcat (comp path->brep-lines close-path) paths))
     :surfaces (mapv brep-surface-polygon paths)}))

(defn frep-sphere [r]
  (fn [pt]
    (let [[x y z] pt]
      (+ (sq x) (sq y) (sq z) (- (sq r))))))

(defn brep-sphere
  [r]
  (fn [u v]
    (let [[u v] (map #(* 2 Math/PI %) [u v])
          x (* r (Math/sin u) (Math/cos v))
          y (* r (Math/sin u) (Math/sin v))
          z (* r (Math/cos u))]
      [x y z])))

(defn brep-surface-torus
  [R r]
  (fn [u v]
    (let [[u v] (map #(* 2 Math/PI %) [u v])
          x (* (+ R (* r (Math/cos u))) (Math/cos v))
          y (* (+ R (* r (Math/cos u))) (Math/sin v))
          z (* r (Math/sin u))]
      [x y z])))

(defn frep-cylinder [r h]
  (fn [pt]
    (let [[x y z] pt]
      (max (- (Math/sqrt (+ (sq x) (sq y))) r)
           (- z h) (- (- h) z)))))

(defn brep-surface-cylinder
  [r h]
  (fn [u v]
    (let [u (* 2 Math/PI u)
          v (* h v)
          x (* r (Math/cos u))
          y (* r (Math/sin u))
          z v]
      [x y z])))

(defn frep-box [l w h]
  (fn [pt]
    (let [[x y z] pt]
      (max (- x l) (- (- l) x)
           (- y w) (- (- w) y)
           (- z h) (- (- h) z)))))


