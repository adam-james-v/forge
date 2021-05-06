(ns forge.import.svg
  (:require [clojure.java.shell :refer [sh]]
            [clojure.string :as str]
            [clojure.data.xml :as xml]
            [svg-clj.main :as svg]
            [svg-clj.path :as path]
            [svg-clj.utils :as utils]
            [svg-clj.transforms :as tf]))

(ns forge.import.images
  (:require [clojure.java.shell :refer [sh]]
            [clojure.string :as str]
            [clojure.data.xml :as xml]
            [svg-clj.main :as svg]
            [svg-clj.path :as path]
            [svg-clj.utils :as utils]
            [svg-clj.transforms :as tf]
            [scad-clj.model :as scad]
            [scad-clj.scad :refer [write-scad]]))

(defn img->str [fname]
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

;; xml parse/transform technique is from:
;; https://github.com/babashka/babashka/blob/master/examples/portal.clj
(defn xml->hiccup [xml]
  (if-let [t (:tag xml)]
    (let [elt [t]
          elt (if-let [attrs (:attrs xml)]
                (conj elt attrs)
                elt)]
      (into elt (map xml->hiccup (:content xml))))
    xml))

(defn str->elements
  [str]
  (-> str
      (xml/parse-str :namespace-aware false)
      xml->hiccup
      (->> (drop 2))))

(defn split-path
  [[k props]]
  (let [ps (-> (:d props)
               (str/split #"(?=M)")
               (->> (map str/trim)))]
    (map #(assoc-in [k props] [1 :d] %) ps)))

(defn path->pts
  [path-elem]
  (let [cmds (path/path-string->commands (get-in path-elem [1 :d]))]
    (mapv :input cmds)))

(defn re-center
  [seq]
  (let [group (svg/g seq)
        ctr (mapv float (tf/centroid group))]
    (->> seq
         (map #(tf/translate (utils/v* [-1 -1] ctr) %)))))

(defn scad->svg
  [scad-block]
  (let [scad (write-scad [(scad/fn! 200) scad-block])
        fname (str (gensym "tmp") ".svg")]
    (do (sh "openscad" "/dev/stdin" "-o" fname :in scad)
        (let [svg (slurp fname)]
          (do (sh "rm" fname)
              (str->elements svg))))))

(defn line
  [from to & {:keys [r]}]
  (let [r (if r r 2)]
    (scad/color 
     [0 0 0 1]
     (if (= from to)
       (scad/sphere r)
       (let [diff (map - to from)
             norm (utils/distance from to)
             rotate-angle (Math/acos (/ (last diff) norm))
             rotate-axis [(- (nth diff 1)) (nth diff 0) 0]]
         (scad/union
          (scad/sphere r)
          (scad/translate to (scad/sphere r))
          (->> (scad/cylinder r norm)
               (scad/translate [0 0 (/ norm 2)])
               (scad/rotate rotate-angle rotate-axis)
               (scad/translate from))))))))

(defn polyline
  [pts & {:keys [r]}]
  (apply scad/union (map #(line (first %) (second %) :r r) (partition 2 1 pts))))

(defn- add-z
  [pts]
  (map #(conj % 0) pts))

(defn flip-y
  [pts]
  (map #(utils/v* % [1 -1]) pts))

(defn line-drawing
  [fname & {:keys [r]}]
  (-> fname
      img->str
      str->elements
      re-center
      (->> (mapcat split-path))
      (->> (map path->pts))
      (->> (map flip-y))
      (->> (map add-z))
      (->> (map #(polyline % :r r)))
      scad/union))

(defn svg-path-elem->scad-polygon
  [path-elem]
  (-> path-elem
      split-path
      (->> (map path->pts))
      (->> (map flip-y))
      (->> (map scad/polygon))
      (->> (apply scad/difference))))

(defn drawing
  [fname]
  (-> fname
      img->str
      str->elements
      re-center
      (->> (map svg-path-elem->scad-polygon))
      scad/union))

(defn linecube
  [x y z]
  (scad/union
   (scad/color [0 1 0 1] (scad/cube x y z))
   (scad/translate [(/ x -2.0) (/ y -2.0) (/ z -2.0)]
    (scad/union
     (line [0 0 0] [x 0 0])
     (line [x 0 0] [x y 0])
     (line [x y 0] [0 y 0])
     (line [0 y 0] [0 0 0])
     (line [0 0 0] [0 0 z])
     (line [x 0 0] [x 0 z])
     (line [x y 0] [x y z])
     (line [0 y 0] [0 y z])
     (line [0 0 z] [x 0 z])
     (line [x 0 z] [x y z])
     (line [x y z] [0 y z])
     (line [0 y z] [0 0 z])))))
