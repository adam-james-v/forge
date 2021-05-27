(ns forge.model
  (:require [clojure.walk :refer [postwalk]]
            [forge.proto :as f]))

(def pi Math/PI)
(def tau (* 2 pi))

(defn rect
  [x y]
  [:rect {:origin [0 0 0]
          :lcs {:lx [1 0 0]
                :ly [0 1 0]
                :lz [0 0 1]}
          :x x :y y :center true}])

(defn circle
  [r]
  [:circle {:origin [0 0 0]
            :lcs {:lx [1 0 0]
                  :ly [0 1 0]
                  :lz [0 0 1]}
            :r r}])

(defn polygon
  ([pts]
   [:polygon {:origin [0 0 0]
              :lcs {:lx [1 0 0]
                    :ly [0 1 0]
                    :lz [0 0 1]}
              :pts (vec pts) :paths [(vec (range (count pts)))]}])
  ([pts paths]
   [:polygon {:origin [0 0 0]
              :lcs {:lx [1 0 0]
                    :ly [0 1 0]
                    :lz [0 0 1]}
              :pts (vec pts) :paths paths}]))

(defn project
  [block cut]
  [:project {:origin [0 0 0]
             :lcs {:lx [1 0 0]
                   :ly [0 1 0]
                   :lz [0 0 1]}
             :cut cut} block])

(defn sphere
  [r]
  [:sphere {:origin [0 0 0]
            :lcs {:lx [1 0 0]
                  :ly [0 1 0]
                  :lz [0 0 1]}
            :r r}])

(defn box
  [x y z]
  [:box {:origin [0 0 0]
         :lcs {:lx [1 0 0]
               :ly [0 1 0]
               :lz [0 0 1]}
         :x x :y y :z z :center true}])

(defn cylinder
  ([r h]
   [:cylinder {:origin [0 0 0]
               :lcs {:lx [1 0 0]
                     :ly [0 1 0]
                     :lz [0 0 1]}
               :r r :h h :center true}])
  ([r1 r2 h]
   [:cylinder {:origin [0 0 0]
               :lcs {:lx [1 0 0]
                     :ly [0 1 0]
                     :lz [0 0 1]}
               :r1 r1 :r2 r2 :h h :center true}]))

(defn polyhedron
  [pts faces]
  [:polyhedron {:origin [0 0 0]
                :lcs {:lx [1 0 0]
                      :ly [0 1 0]
                      :lz [0 0 1]}
                :pts pts :faces faces}])

(defn extrude
  [block {:keys [height twist convexity center slices scale] :as opts}]
  [:extrude (merge {:origin [0 0 0]
                    :lcs {:lx [1 0 0]
                          :ly [0 1 0]
                          :lz [0 0 1]}}
                   opts) block])

(defn revolve
  [block {:keys [convexity angle] :as opts}]
  [:revolve (merge {:origin [0 0 0]
                    :lcs {:lx [1 0 0]
                          :ly [0 1 0]
                          :lz [0 0 1]}}
                   opts) block])

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
  [block [x y z]]
  (let [[tag {:keys [origin] :as props} content] block
        xf-props (-> props
                     (assoc :origin (f/v+ origin [x y z]))
                     (assoc :translation [x y z]))]
    [:translate {:xf-elem (vec (filter some? [tag xf-props content]))
                 :translation [x y z]
                 :x x :y y :z z} block]))

(defn- rotate-lcs
  [lcs [x y z]]
  (let [[keys vals] ((juxt keys vals) lcs)]
    (zipmap keys (map #(f/rotate-point % [x y z]) vals))))

(defn rotate
  ([block [x y z]]
   (let [[tag {:keys [lcs] :as props} content] block
         xf-props (-> props 
                      (assoc :lcs (rotate-lcs lcs [x y z]))
                      (assoc :rotation [x y z]))]
     [:rotate {:xf-elem (vec (filter some? [tag xf-props content]))
               :rotation [x y z]
               :x x :y y :z z} block]))
  
  ([block a [x y z]]
   [:rotate {:a a :x x :y y :z z} block]))

(defn group
  [& content]
  (if (and (= 1 (count content))
           (not (keyword? (first (first content)))))
    ;; content is a list of a list of elements
    (into [:group {}] (first content))
    ;; content is a single element OR a list of elements
    (into [:group {}] (filter (complement nil?) content))))

(defn resize
  [block [x y z]]
  [:resize {:x x :y y :z z} block])

(defn scale
  [block [x y z]]
  [:scale {:x x :y y :z z} block])

(defn mirror
  [block [x y z]]
  [:mirror {:x x :y y :z z} block])

(defn color
  [block [r g b a]]
  [:color {:r r :g g :b b :a a} block])

(defn hull
  [block]
  [:hull {} block])

(defn offset
  [block opts]
  (if (number? opts)
    [:offset {:r opts} block]
    [:offset opts block]))

(defn minkowski
  [block]
  [:minkowski {} block])

(defn multmatrix
  [block mtx]
  [:multmatrix {:mtx mtx} block])


