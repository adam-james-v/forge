(ns forge.import.image
  (:require [clojure.java.shell :refer [sh]]
            [clojure.string :as str]
            [clojure.data.xml :as xml]
            [svg-clj.elements :as svg]
            [svg-clj.path :as path]
            [svg-clj.utils :as utils]
            [svg-clj.transforms :as tf]
            [forge.model :as mdl]
            [forge.compile.scad :refer [write]]))

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
         (map #(tf/translate % (utils/v* [-1 -1] ctr))))))

(defn mdl->svg
  [mdl]
  (let [scad (str "$fn=200;\n" (write mdl))
        fname (str (gensym "tmp") ".svg")]
    (do (sh "openscad" "/dev/stdin" "-o" fname :in scad)
        (let [svg (slurp fname)]
          (do (sh "rm" fname)
              (rest 
               (str->elements svg)))))))

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
      (->> (map #(mdl/polyline %)))
      mdl/union))

(defn- svg-path-elem->polygon
  [path-elem]
  (-> path-elem
      split-path
      (->> (map path->pts))
      (->> (map flip-y))
      (->> (map mdl/polygon))
      (->> (apply mdl/difference))))

(defn drawing
  [fname]
  (-> fname
      img->str
      str->elements
      re-center
      (->> (map svg-path-elem->polygon))
      mdl/union))
