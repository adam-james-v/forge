(ns forge.model
  (:require [forge.utils :as utils]
            [forge.geom :as geom]))

(def pi Math/PI)
(def tau (* 2 pi))

(defn pt
  ([x y]
   [:pt {:x x :y y}])
  ([x y z]
   [:pt {:x x :y y :z z}]))

(defn line
  [a b]
  [:line {:a a :b b}])

(defn polyline
  [pts]
  [:polyline {:pts pts}])

(defn rect
  [x y]
  [:rect {:x x :y y :center true}])

(defn circle
  [r]
  [:circle {:r r}])

(defn polygon
  ([pts]
   [:polygon {:pts (vec pts) :paths [(vec (range (count pts)))]}])
  ([pts paths]
   [:polygon {:pts (vec pts) :paths paths}]))

(defn project
  [block cut]
  [:project {:cut cut} block])

(defn sphere
  [r]
  [:sphere {:r r}])

(defn box
  [x y z]
  [:box {:x x :y y :z z :center true}])

(defn cylinder
  ([r h]
   [:cylinder {:r r :h h :center true}])
  ([r1 r2 h]
   [:cylinder {:r1 r1 :r2 r2 :h h :center true}]))

(defn polyhedron
  [pts faces]
  [:polyhedron {:pts pts :faces faces}])

#_(defn extrude
  [elem {:keys [height twist convexity center slices scale] :as opts}]
  [:extrude opts elem])

(defn extrude
  [elem h]
  [:extrude {:h h} elem])

#_(defn revolve
  [elem {:keys [convexity angle] :as opts}]
  [:revolve opts elem])

(defn revolve
  [elem a]
  [:revolve {:a a} elem])

(defn union
  [& elems]
  [:union {} elems])

(defn intersection
  [& elems]
  [:intersection {} elems])

(defn difference
  [& elems]
  [:difference {} elems])

(defn translate
  [elem [x y z]]
  [:translate {:x x :y y :z z} elem])

(defn rotate
  ([elem [x y z]]
   [:rotate {:x x :y y :z z} elem])

  ([elem a [x y z]]
   [:rotate {:a a :x x :y y :z z} elem]))

(defn group
  [& elems]
  (if (and (= 1 (count elems))
           (not (keyword? (first (first elems)))))
    ;; elems is a list of a list of elements
    (into [:group {}] (first elems))
    ;; elems is a single element OR a list of elements
    (into [:group {}] (filter (complement nil?) elems))))

(defn scale
  [elem [x y z]]
  [:scale {:x x :y y :z z} elem])

(defn mirror
  [elem [x y z]]
  [:mirror {:x x :y y :z z} elem])

(defn color
  [elem [r g b a]]
  [:color {:r r :g g :b b :a a} elem])

(defn hull
  [elem]
  [:hull {} elem])

(defn offset
  [elem d]
  [:offset {:d d} elem])

(defn minkowski
  [& elems]
  [:minkowski {} elems])

(defn multmatrix
  [elem mtx]
  [:multmatrix {:mtx mtx} elem])


