(ns forge.utils
  (:require [clojure.string :as str]
            [same :refer [ish? zeroish?]]))

(defn nearly?
  "compare two float values for approximate equality.
   
   Default epsilon = 0.00001"
  ([^long a ^long b]
   (nearly? a b 0.00001))

  ([^long a ^long b ^long epsilon]
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
    (let [diffs (mapv #(float (Math/abs ^long (- %1 %2))) coll-a coll-b)]
      (empty? 
       (filter 
        false? 
        (map zeroish? diffs))))))

(def v+ (partial mapv +))
(def v- (partial mapv -))
(def v* (partial mapv *))

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
    (Math/sqrt ^double v2)))

(defn normalize
  "find the unit vector of a given vector"
  [v]
  (let [m (Math/sqrt ^double (reduce + (v* v v)))]
    (mapv / v (repeat m))))

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

(defn on-line-inf?
  "determine if a point is on an infinitely extending line"
  [pt line]
  (let [[a b] line
        ap (mapv - a pt)
        bp (mapv - b pt)]
    (all-nearly? (cross* ap bp) [0 0 0])))

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

(defn rotate-point
  [pt [ax ay az]]
  (let [pt (if (< (count pt) 3)
             (conj pt 0)
             pt)]
    (-> pt
        (rot-pt :z az)
        (rot-pt :y ay)
        (rot-pt :x ax))))

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
  (let [v1 (v- p2 p1)
        v2 (v- p2 p3)
        l1 (distance p1 p2)
        l2 (distance p3 p2)
        n (dot* v1 v2)
        d (Math/abs ^long (* l1 l2))]
    (when (not (= 0.0 (float d)))
      (to-deg (Math/acos ^long (/ n d))))))

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

(defn regular-polygon-pts
  [r n]
  (let [angle (* 2 Math/PI (/ 1 n))]
    (for [step (range n)]
      [(* r (Math/cos (* step angle)))
       (* r (Math/sin (* step angle)))])))

(defn ext
  [fname]
  (last (str/split fname #"\.")))
