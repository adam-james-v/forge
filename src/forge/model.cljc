(ns forge.model
  (:require [clojure.walk :refer [postwalk]]))

(def pi Math/PI)
(def tau (* 2 pi))

(defn rect
  [x y]
  [:rect {:x x :y y}])

(defn circle
  [r]
  [:circle {:r r}])

(defn polygon
  ([pts]
   [:polygon {:pts pts}])
  ([pts paths]
   [:polygon {:pts pts :paths paths}]))

(defn project
  [cut & block]
  [:project {:cut cut} block])

(defn sphere
  [r]
  [:sphere {:r r}])

(defn box
  [x y z]
  [:box {:x x :y y :z z}])

(defn cylinder
  ([r h]
   [:cylinder {:r r :h h}])
  ([r1 r2 h]
   [:cylinder {:r1 r1 :r2 r2 :h h}]))

(defn polyhedron
  [pts faces]
  [:polyhedron {:pts pts :faces faces}])

(defn extrude
  [{:keys [height twist convexity center slices scale] :as opts} & block]
  [:extrude opts block])

(defn revolve
  [{:keys [convexity angle] :as opts} & block]
  [:revolve {:convexity convexity
             :angle angle} block])

(defn group
  [& block]
  [:group block])

(defn resize
  [[x y z] & block]
  [:resize {:x x :y y :z z} block])

(defn translate
  [[x y z] & block]
  [:translate {:x x :y y :z z} block])

(defn rotatev
  [a [x y z] block]
  [:rotatev {:a a :x x :y y :z z} block])

(defn rotatec
  [[x y z] block]
  [:rotatec {:x x :y y :z z} block])

(defn rotate
  [& block]
  (if (number? (first block))
    (rotatev (first block) (second block) (drop 2 block))
    (rotatec (first block) (rest block))))

(defn scale
  [[x y z] & block]
  [:scale {:x x :y y :z z} block])

(defn mirror
  [[x y z] & block]
  [:mirror {:x x :y y :z z} block])

(defn color
  [[r g b a] & block]
  [:color {:r r :g g :b b :a a} block])

(defn hull
  [& block]
  [:hull {} block])

(defn offset
  [opts & block]
  (if (number? opts)
    [:offset {:r opts} block]
    [:offset opts block]))

(defn minkowski
  [& block]
  [:minkowski {} block])

(defn multmatrix
  [m & block]
  [:multmatrix {:m m} block])

(defn union
  [& forms]
  [:union {} forms])

(defn intersection
  [& forms]
  [:intersection {} forms])

(defn difference
  [& forms]
  [:difference {} forms])
