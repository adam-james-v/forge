(ns forge.compile.scad
  (:require [clojure.java.shell :refer [sh]]
            [clojure.string :as str]
            [forge.utils :as utils]
            [svg-clj.utils :refer [svg-str->elements]]
            [forge.model :as mdl]))

;; multimethod
(defmulti write-expr
  (fn [depth element]
    (if (keyword? (first element)) (first element) :list)))

(defmethod write-expr :list
  [depth [& elems]]
  (mapcat #(write-expr depth %1) elems))

;; utility
(defn indent
  [depth]
  (str/join (repeat depth "  ")))

(defn write-block
  [depth block]
  (mapcat #(write-expr (inc depth) %1) block))

(declare map-to-arg-string)

(defn make-arguments
  [args]
  (let [arg (first args)
        rest (rest args)
        piece (cond
               (map? arg) (map-to-arg-string arg)
               (coll? arg) (str "[" (make-arguments arg) "]")
               :else arg)]
    (if (empty? rest)
      piece
      (str/join ", " [piece (make-arguments rest)]))))

(defn map-to-arg-string
  [m]
  (str/join ", " (map (fn [[k v]] (str (name k) "=" (make-arguments [v])) ) m)))

(defmethod write-expr :circle
  [depth [_ {:keys [r center]}]] 
  (list (indent depth) 
        "circle (r=" r ");\n"))

(defmethod write-expr :ellipse
  [depth [_ {:keys [rx ry center]}]] 
  (list (indent depth) 
        "scale([1, " (/ ry rx) ", 1])circle (r=" rx ");\n"))

(defmethod write-expr :rect
  [depth [_ {:keys [w h center]}]]
  (list (indent depth)
        "square ([" w ", " h "]"
        (when center ", center=true") ");\n"))

(defmethod write-expr :polygon
  [depth [_ {:keys [pts paths convexity]}]]
  `(~@(indent depth) "polygon ("
    "points=[[" ~(str/join "], [" (map #(str/join ", " %1) pts)) "]]"
    ~@(when paths [", paths=[[" (str/join "], [" (map #(str/join "," %1) paths)) "]]"])
    ~@(when convexity [", convexity=" convexity])
    ");\n"))

(defmethod write-expr :slice
  [depth [_ {:keys [z]} elem]]
  (concat
   (list (indent depth)
         "projection(cut=true){\n")
   (write-expr (inc depth) (-> elem (mdl/translate [0 0 (- z)])))
   (list (indent depth) "}\n")))

(defmethod write-expr :sphere
  [depth [_ {:keys [r center]}]]
  (list (indent depth) 
        "sphere (r=" r ");\n"))

(defmethod write-expr :box
  [depth [_ {:keys [x y z center]}]]
  (list (indent depth)
        "cube ([" x ", " y ", " z "]"
        (when center ", center=true") ");\n"))

(defmethod write-expr :cylinder
  [depth [_ {:keys [h r r1 r2 center]}]]
  (concat
   (list (indent depth) 
         "cylinder (h=" h)
   (if r (list ", r=" r) (list ", r1=" r1 ", r2=" r2))
   (when center (list ", center=true"))
   (list ");\n")))

(defmethod write-expr :polyhedron
  [depth [_ {:keys [pts faces convexity]}]]
  `(~@(indent depth) "polyhedron ("
    "points=[[" ~(str/join "], [" (map #(str/join ", " %1) pts)) "]], "
    "faces=[[" ~(str/join "], [" (map #(str/join ", " %1) faces)) "]]"
    ~@(if (nil? convexity) [] [", convexity=" convexity])
    ");\n"))

(defmethod write-expr :extrude
  [depth [_ {:keys [h]} elem]]
  (concat
   (list (indent depth) 
         "linear_extrude(height=" h
         "){\n")
   (write-expr (inc depth) elem)
   (list (indent depth) 
         "}\n")))

(defmethod write-expr :revolve
  [depth [_ {:keys [a]} elem]]
  (concat
   (list (indent depth)
         "rotate_extrude(angle=" a
         "){\n")
   (write-expr (inc depth) elem)
   (list (indent depth) 
         "}\n")))

(defmethod write-expr :translate
  [depth [_ {:keys [x y z]} elem]]
  (concat
   (list (indent depth) "translate ([" x ", " y ", " z "]) {\n")
   (write-expr (inc depth) elem)
   (list (indent depth) "}\n")))

(defmethod write-expr :rotate
  [depth [_ {:keys [a x y z]} elem]]
  (if a
    (concat
     (list (indent depth) "rotate (a=" a ", v=[" x ", " y ", " z "]) {\n")
     (write-expr (inc depth) elem)
     (list (indent depth) "}\n"))
    (concat
     (list (indent depth) "rotate ([" x "," y "," z "]) {\n")
     (write-expr (inc depth) elem)
     (list (indent depth) "}\n"))))

(defmethod write-expr :scale
  [depth [_ {:keys [x y z]} elem]]
  (concat
   (list (indent depth) "scale ([" x ", " y ", " z "]) {\n")
   (write-expr (inc depth) elem)
   (list (indent depth) "}\n")))

(defmethod write-expr :mirror
  [depth [_ {:keys [x y z]} elem]]
  (concat
   (list (indent depth) "mirror ([" x ", " y ", " z "]) {\n")
   (write-expr (inc depth) elem)
   (list (indent depth) "}\n")))

(defmethod write-expr :hull
  [depth [_ _ elem]]
  (concat
   (list (indent depth) "hull () {\n")
   (write-expr (inc depth) elem)
   (list (indent depth) "}\n")))

(defmethod write-expr :offset
  [depth [_ {:keys [d]} elem]]
  (concat 
   (list (indent depth) "offset (r = " d "){\n")
   (write-expr (inc depth) elem)
   (list (indent depth) "}\n")))

(defmethod write-expr :minkowski
  [depth [_ _ elems]]
  (concat
   (list (indent depth) "minkowski () {\n")
   (write-expr (inc depth) elems)
   (list (indent depth) "}\n")))

(defmethod write-expr :multmatrix
  [depth [_ {:keys [mtx]} elem]]
  (let [w (fn [s] (str "[" s "]"))
        co (fn [c] (apply str (interpose "," c)))]
    (concat
     (list (indent depth) "multmatrix(")
     (w (co (map #(w (co %)) mtx)))
     (list ") {\n")
     (mapcat #(write-expr (inc depth) %1) elem)
     (list (indent depth) "}\n"))))

(defmethod write-expr :union
  [depth [_ _ elems]]
  (concat
   (list (indent depth) "union () {\n")
   (write-block depth elems)
   (list (indent depth) "}\n")))

(defmethod write-expr :difference
  [depth [_ _ elems]]
  (concat
   (list (indent depth) "difference () {\n")
   (mapcat #(write-expr (inc depth) %1) elems)
   (list (indent depth) "}\n")))

(defmethod write-expr :intersection
  [depth [_ _ elems]]
  (concat
   (list (indent depth) "intersection () {\n")
   (mapcat #(write-expr (inc depth) %1) elems)
   (list (indent depth) "}\n")))

(defmethod write-expr :color
  [depth [_ {:keys [r g b a]} elem]]
  (concat
    (list (indent depth) "color (["
          (/ r 255.0) ", "
          (/ g 255.0) ", "
          (/ b 255.0) ", "
          (utils/clamp a 0.0 1.0) "]) {\n")
    (write-expr depth elem)
    (list (indent depth) "}\n")))

(defmethod write-expr :group 
  [depth [_ _ elems]]
  (concat
   (list (indent depth) "group() {\n")
   (write-expr (inc depth) elems)
   (list (indent depth) "}\n")))

(defn- scad-line
  [a b & {:keys [r]}]
  (let [r (if r r 2)
        a (if (< (count a) 3) (forge.utils/add-z a) a)
        b (if (< (count b) 3) (forge.utils/add-z b) b)]
    (if (= a b)
      (-> (mdl/sphere r) (mdl/translate a))
      (let [[dx dy dz] (utils/v- a b)
            norm (utils/distance b a)
            rotate-angle (utils/to-deg (Math/acos (/ dz norm)))
            rotate-axis [(- dy) dx 0]]
        (-> (mdl/union
             (-> (mdl/sphere r) (mdl/translate a))
             (-> (mdl/sphere r) (mdl/translate b))
             (-> (mdl/cylinder r norm)
                 (mdl/translate [0 0 (/ norm 2)])
                 (mdl/rotate rotate-angle rotate-axis)
                 (mdl/translate b)))
            (mdl/color [0 0 0 1]))))))

(defn- scad-polyline
  [pts & {:keys [r]}]
  (apply mdl/union
   (map #(scad-line (first %) (second %) :r r)
        (partition 2 1 pts))))

(defn- linecube
  [x y z]
  (mdl/union
   (-> (mdl/box x y z) (mdl/color [0 1 0 1]))
   (-> (mdl/union
        (mdl/line [0 0 0] [x 0 0])
        (mdl/line [x 0 0] [x y 0])
        (mdl/line [x y 0] [0 y 0])
        (mdl/line [0 y 0] [0 0 0])
        (mdl/line [0 0 0] [0 0 z])
        (mdl/line [x 0 0] [x 0 z])
        (mdl/line [x y 0] [x y z])
        (mdl/line [0 y 0] [0 y z])
        (mdl/line [0 0 z] [x 0 z])
        (mdl/line [x 0 z] [x y z])
        (mdl/line [x y z] [0 y z])
        (mdl/line [0 y z] [0 0 z]))
       (mdl/translate [(/ x -2.0) (/ y -2.0) (/ z -2.0)]))))

(defmethod write-expr :line
  [depth [_ {:keys [a b]}]]
  (write-expr depth (scad-line a b)))

(defmethod write-expr :polyline
  [depth [_ {:keys [pts]}]]
  (write-expr depth (scad-polyline pts)))

(defn write [& block]
  (str/join (write-expr 0 block)))

(defn png!
  [fname mdl-data]
  (let [scad (write [#_(fn! 20) mdl-data])]
    (sh "/usr/local/bin/openscad" "/dev/stdin"
        "--imgsize" "400,400"
        "--projection" "orthogonal"
        #_"--colorscheme" #_"greenscreen" #_"Nord"
        #_"--camera" #_"0,0,0,55,0,25,2900"
        "-o" fname
        :in scad)))

(defn cider-show
  [mdl-data]
  (let [fname "_tmp.png"]
    (do (png! fname mdl-data)
        (clojure.java.io/file fname))))

(defn mdl->svg
  [mdl]
  (let [scad (str "$fn=200;\n" (write mdl))
        fname (str (gensym "tmp") ".svg")]
    (do (sh "openscad" "/dev/stdin" "-o" fname :in scad)
        (let [svg-str (slurp fname)]
          (do (sh "rm" fname)
              (rest 
               (svg-str->elements svg-str)))))))
