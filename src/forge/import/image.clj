(ns forge.import.image
  (:require [clojure.java.shell :refer [sh]]
            [clojure.string :as str]
            [clojure.data.xml :as xml]
            [clojure.core :exclude [import]]
            [svg-clj.elements :as svg]
            [svg-clj.path :as path]
            [svg-clj.transforms :as tf]
            [svg-clj.utils :refer [s->v
                                   svg-str->hiccup]]
            [forge.model :as mdl]
            [forge.utils :as utils]))

(defn img->str 
  [fname]
  "Ingest image file `fname` and transform it into a hiccup data structure."
  (let [new-fname (str (first (str/split fname #"\.")) ".svg")]
    (sh "vtracer" 
        "--mode" "polygon"
        "--colormode" "bw"
        "--segment_length" "3.5"
        "--input" fname
        "--output" new-fname)
    (let [svg-str (slurp new-fname)]
      (sh "rm" new-fname)
      (-> svg-str
          (str/replace #"<\?xml.+>" "")
          str/trim))))

(defn- closed?
  [path]
  (let [d (get-in path [1 :d])]
    (-> d
        str/trim
        str/upper-case
        (str/ends-with? "Z"))))

(defn re-center
  [seq]
  (let [group (svg/g seq)
        ctr (mapv float (tf/centroid group))]
    (->> seq
         (map #(tf/translate % (utils/v* [-1 -1] ctr))))))

(defmulti svg->mdl
  (fn [element]
    (if (keyword? (first element))
      (first element)
      :list)))

(defn- clean-props
  [props]
  (let [removable [:cx :cy :transform :width :height :x :y :d :fill-rule]]
    (apply dissoc (concat [props] removable))))

(defn- parse-transform
  [tf-str]
  (let [xf (svg-clj.utils/str->xf-map tf-str)
        tr (:translate xf)
        rot (:rotate xf)
        angle (last rot)]
    [(fn [elem]
       (cond-> elem
         rot (mdl/rotate [0 0 angle])
         tr (mdl/translate tr)))
     xf]))

(defmethod svg->mdl :list
  [elems]
  (map svg->mdl elems))

(defmethod svg->mdl :default [_] nil)

(defmethod svg->mdl :circle
  [[_ {:keys [cx cy r] :as props}]]
  (let [xf-props (clean-props props)]
    (cond-> (mdl/circle r)
      true (mdl/style xf-props)
      (and (not= cx 0)
           (not= cy 0)) (mdl/translate [cx cy 0]))))

(defmethod svg->mdl :ellipse
  [[_ {:keys [cx cy rx ry] :as props}]]
  (let [xf-props (clean-props props)]
    (cond-> (mdl/ellipse rx ry)
      true (mdl/style xf-props)
      (and (not= cx 0)
           (not= cy 0)) (mdl/translate [cx cy 0]))))

(defmethod svg->mdl :line
  [[_ {:keys [x1 y1 x2 y2] :as props}]]
  (let [xf-props (clean-props props)]
    (-> (mdl/line [x1 y1 0] [x2 y2 0])
        (mdl/style xf-props))))

(defmethod svg->mdl :polyline
  [[_ {:keys [points] :as props}]]
  (let [xf-props (clean-props props)
        pts (->> points
                 s->v
                 (partition 2))]
    (-> (mdl/polyline pts)
        (mdl/style xf-props))))

(defmethod svg->mdl :polygon
  [[_ {:keys [points] :as props}]]
  (let [xf-props (clean-props props)
        pts (->> points
                 s->v
                 (partition 2))]
    (-> (mdl/polygon pts)
        (mdl/style xf-props))))

(defmethod svg->mdl :rect
  [[_ {:keys [width height x y transform] :as props}]]
  (let [[xf xf-map] (parse-transform transform)
        angle (last (get xf-map :rotate [0]))
        xf-props (clean-props props)
        pos (-> (utils/v+ [x y] [(/ width 2.0) (/ height 2.0)])
                (utils/rotate-point [0 0 angle]))]
    (-> (mdl/rect width height)
        xf
        (mdl/style xf-props)
        (mdl/translate (conj pos 0)))))

(defmethod svg->mdl :g
  [[_ props & elems]]
  (let [xf-props (clean-props props)]
    [:group xf-props (map svg->mdl elems)]))

(defn- bb-area
  [elem]
  (reduce * (tf/bb-dims elem)))

(defn path->pts
  [path-elem]
  (let [cmds (-> path-elem
                 (get-in [1 :d])
                 path/path-str->cmds)]
    (into [] (filter some? (map :input cmds)))))

(defn- svg-path-elem->polygon
  [path-elem]
  (let [pgs (->> path-elem
                 path/split-path
                 (sort-by bb-area)
                 (map path->pts)
                 (map mdl/polygon)
                 reverse)]
    (if (> (count pgs) 1)
      (apply mdl/difference pgs)
      (first pgs))))

(defn- svg-path-elem->polyline
  [path-elem]
  (let [pgs (->> path-elem
                 path/split-path 
                 (sort-by bb-area)
                 (map path->pts)
                 (map mdl/polyline)
                 reverse)]
    (if (> (count pgs) 1)
      (apply mdl/difference pgs)
      (first pgs))))

(defmethod svg->mdl :path
  [[_ props :as elem]]
  (let [xf-props (clean-props props)
        xf-elem (path/decurve elem)]
    (-> (if (closed? elem)
          (svg-path-elem->polygon xf-elem)
          (svg-path-elem->polyline xf-elem))
        (mdl/style xf-props))))

(defn line-drawing
  [fname & {:keys [r]}]
  (-> fname
      img->str
      svg-str->hiccup
      (->> (drop 2))
      re-center
      (->> (mapcat path/split-path))
      (->> (map path->pts))
      (->> (map #(mdl/polyline %)))
      mdl/union))

(defn import-png
  [fname]
  (-> fname
      img->str
      svg-str->hiccup
      (->> (drop 2))
      re-center
      (->> (map svg-path-elem->polygon))
      mdl/union))

(defn import-svg
  [fname]
  (let [data (-> (slurp fname) svg-str->hiccup)
        elems (if (= :svg (first data)) (drop 2 data) data)]
    (->> elems
         (map svg->mdl)
         (filter some?))))

(defn import
  [fname]
  (let [fext (utils/ext fname)
        f (get {"svg" import-svg
                "png" import-png} fext)]
    (f fname)))
