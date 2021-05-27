(ns forge.compile.svg
  (:require [clojure.java.shell :refer [sh]]
            [clojure.string :as str]
            [svg-clj.elements :as svg]
            [svg-clj.path :as path]
            [svg-clj.transforms :as tf]
            [svg-clj.tools :as tools]
            [hiccup.core :refer [html]]
            [forge.proto :as f :refer [to-rad
                                       to-deg]]))

;; multimethod
(defmulti elem-to-svg
  (fn [element _]
    (if (keyword? (first element)) 
      (first element) 
      :list)))

(defmethod elem-to-svg :list 
  [[& args] xf]
  (map #(elem-to-svg % xf) args))

(defn block-to-svg [block xf]
  (map #(elem-to-svg % xf) block))

(defmethod elem-to-svg :circle
  [[_ {:keys [r center origin rotation translation]}] xf]
  (let [rot (if rotation rotation [0 0 0])
        tr (if translation translation [0 0 0])
        pts (->> (f/regular-polygon-pts r 40)
                 (mapv #(conj % 0))
                 (mapv #(f/v+ origin %))
                 (mapv #(f/rotate-point % rot))
                 xf)]
    (-> (path/polygon-path pts)
        (tf/style {:fill "none"
                   :stroke "black"
                   :stroke-width "2px"}))))

(defmethod elem-to-svg :rect 
  [[_ {:keys [x y center origin rotation]}] xf]
  (let [pts (->> (if center
                   [ [(/ x -2.0) (/ y -2.0)]
                    [(/ x 2.0) (/ y -2.0)] 
                    [(/ x 2.0) (/ y 2.0)]
                    [(/ x -2.0) (/ y 2.0)] ]
                   [ [0 0] [x 0] [x y] [0 y] ])
                 (mapv #(f/v+ origin %))
                 (mapv #(f/rotate-point % rotation))
                 xf)]
    (-> (path/polygon-path pts)
        (tf/style {:fill "none"
                   :stroke "black"
                   :stroke-width "2px"}))))

(defmethod elem-to-svg :polygon
  [[_ {:keys [pts paths convexity]}] xf]
  (let [polygons (for [path paths]
                   (xf (map #(get pts %) path)))]
    (-> (apply path/merge-paths 
               (map path/polygon-path polygons))
        (tf/style {:fill "none"
                   :stroke "black"
                   :stroke-width "2px"}))))

(defmethod elem-to-svg :translate 
  [[_ {:keys [xf-elem]} block] xf]
  (elem-to-svg xf-elem xf))

(defmethod elem-to-svg :rotate 
  [[_ {:keys [xf-elem]} block] xf]
  (elem-to-svg xf-elem xf))

(def iso-euler-angles [35.264 45 0])
(def origin-angle-adjust-a [90 0 0])
(def origin-angle-adjust-b [0 -90 0])

(defn rotate-points
  [[ax ay az] pts]
  (mapv #(f/rotate-point % [ax ay az]) pts))

(defn isometric-xf
  [pts]
  (->> pts
       (rotate-points origin-angle-adjust-a)
       (rotate-points origin-angle-adjust-b)
       (rotate-points iso-euler-angles)
       (mapv #(into [] (drop-last %)))))

(defn top-xf
  [pts]
  (-> pts
      (rotate-points [0 0 0])))

(defn right-xf
  [pts]
  (-> pts
      (rotate-points [90 0 0])))

(defn write-svg
  [& mdl-data]
  (->> mdl-data
       (mapv #(elem-to-svg % isometric-xf))
       svg/g
       svg/svg
       #_html))
