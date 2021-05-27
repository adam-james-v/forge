(ns forge.compile.scad
  (:require [clojure.java.shell :refer [sh]]
            [clojure.string :as str]
            [forge.proto :as f :refer [to-rad
                                       to-deg]]))

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

(defmethod write-expr :rect
  [depth [_ {:keys [x y center]}]]
  (list (indent depth)
        "square ([" x ", " y "]"
        (when center ", center=true") ");\n"))

(defmethod write-expr :polygon
  [depth [_ {:keys [pts paths convexity]}]]
  `(~@(indent depth) "polygon ("
    "points=[[" ~(str/join "], [" (map #(str/join ", " %1) pts)) "]]"
    ~@(when paths [", paths=[[" (str/join "], [" (map #(str/join "," %1) paths)) "]]"])
    ~@(when convexity [", convexity=" convexity])
    ");\n"))

(defmethod write-expr :project
  [depth [_ {:keys [cut]} & block]]
  (concat
   (list (indent depth) 
         "projection (cut = " cut ") {\n")
   (mapcat #(write-expr (inc depth) %1) block)
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
  [depth [_ {:keys [height twist convexity center slices scale]} block]]
  (concat
   (list (indent depth) "linear_extrude (height=" height)
   (if (nil? twist) [] (list ", twist=" (to-deg twist)))
   (if (nil? convexity) [] (list ", convexity=" convexity))
   (if (nil? slices) [] (list ", slices=" slices))
   (cond
     (nil? scale) []
     (sequential? scale) (list ", scale=[" (first scale) ", " (second scale) "]")
     :else (list ", scale=" scale))
   (when center (list ", center=true"))
   (list "){\n")

   (mapcat #(write-expr (inc depth) %1) block)
   (list (indent depth) "}\n")))

(defmethod write-expr :revolve
  [depth [_ {:keys [convexity fn angle]} block]]
  (concat
   (list (indent depth) "rotate_extrude (")
   (str/join ", "
     (concat
       (if convexity [(str "convexity=" convexity)])
       (if angle [(str "angle=" angle)])
       (if fn [(str "$fn=" fn)])))
   (list ") {\n")
   (mapcat #(write-expr (inc depth) %1) block)
   (list (indent depth) "}\n")))

(defmethod write-expr :surface
  [depth [_ {:keys [filepath convexity center invert]}]]
  (concat
   (list (indent depth) "surface (file = \"" filepath "\""
         (when convexity (format ", convexity=%d" convexity))
         (when center ", center=true")
         (when invert ", invert=true")
         ");\n")))

(defmethod write-expr :resize
  [depth [_ {:keys [x y z auto]} block]]
  (concat
   (list (indent depth) "resize ([" x ", " y ", " z "]")
   (list (when-not (nil? auto)
           (str " auto="
                (if (coll? auto)
                  (str "[" (str/join ", " (map true? auto)) "]")
                  (true? auto)))))
   "){\n"
   (mapcat #(write-expr (inc depth) %1) block)
   (list (indent depth) "}\n")))

(defmethod write-expr :translate
  [depth [_ {:keys [x y z]} block]]
  (concat
   (list (indent depth) "translate ([" x ", " y ", " z "]) {\n")
   (write-expr (inc depth) block)
   (list (indent depth) "}\n")))

(defmethod write-expr :rotate
  [depth [_ {:keys [a x y z]} block]]
  (if a
    (concat
     (list (indent depth) "rotate (a=" a ", v=[" x ", " y ", " z "]) {\n")
     (write-expr (inc depth) block)
     (list (indent depth) "}\n"))
    (concat
     (list (indent depth) "rotate ([" x "," y "," z "]) {\n")
     (write-expr (inc depth) block)
     (list (indent depth) "}\n"))))

(defmethod write-expr :scale
  [depth [_ {:keys [x y z]} block]]
  (concat
   (list (indent depth) "scale ([" x ", " y ", " z "]) {\n")
   (write-expr (inc depth) block)
   (list (indent depth) "}\n")))

(defmethod write-expr :mirror
  [depth [_ {:keys [x y z]} block]]
  (concat
   (list (indent depth) "mirror ([" x ", " y ", " z "]) {\n")
   (write-expr (inc depth) block)
   (list (indent depth) "}\n")))

(defmethod write-expr :hull
  [depth [_ _ block]]
  (concat
   (list (indent depth) "hull () {\n")
   (write-expr (inc depth) block)
   (list (indent depth) "}\n")))

(defmethod write-expr :offset
  [depth [_ {:keys [r delta chamfer] :or {chamfer false}} block]]
  (concat
   (list (indent depth) "offset (")
   (if r
     (list "r = " r)
     (list "delta = " delta))
   (when chamfer (list ", chamfer=true"))
   (list ") {\n")
   (write-expr (inc depth) block)
   (list (indent depth) "}\n")))

(defmethod write-expr :minkowski
  [depth [_ _ block]]
  (concat
   (list (indent depth) "minkowski () {\n")
   (write-expr (inc depth) block)
   (list (indent depth) "}\n")))

(defmethod write-expr :multmatrix
  [depth [_ {:keys [mtx]} block]]
  (let [w (fn [s] (str "[" s "]"))
        co (fn [c] (apply str (interpose "," c)))]
    (concat
     (list (indent depth) "multmatrix(")
     (w (co (map #(w (co %)) mtx)))
     (list ") {\n")
     (mapcat #(write-expr (inc depth) %1) block)
     (list (indent depth) "}\n"))))

(defmethod write-expr :union
  [depth [_ _ block]]
  (concat
   (list (indent depth) "union () {\n")
   (write-block depth block)
   (list (indent depth) "}\n")))

(defmethod write-expr :difference
  [depth [_ _ block]]
  (concat
   (list (indent depth) "difference () {\n")
   (mapcat #(write-expr (inc depth) %1) block)
   (list (indent depth) "}\n")))

(defmethod write-expr :intersection
  [depth [_ _ block]]
  (concat
   (list (indent depth) "intersection () {\n")
   (mapcat #(write-expr (inc depth) %1) block)
   (list (indent depth) "}\n")))

(defmethod write-expr :color
  [depth [_ [r g b a] block]]
  (concat
    (list (indent depth) "color ([" r ", " g ", " b ", " a"]) {\n")
    (write-block depth block)
    (list (indent depth) "}\n")))

(defmethod write-expr :group 
  [depth [_ _ block]]
  (concat
   (list (indent depth) "group() {\n")
   (mapcat #(write-expr (inc depth) %1) block)
   (list (indent depth) "}\n")))

(defn write-scad [& block]
  (str/join (write-expr 0 block)))

(defn png!
  [fname mdl-data]
  (let [scad (write-scad [#_(fn! 20) mdl-data])]
    (sh "openscad" "/dev/stdin"
        "--imgsize" "400,400"
        "--projection" "orthogonal"
        "--colorscheme" #_"greenscreen" "Nord"
        #_"--camera" #_"0,0,0,55,0,25,2900"
        "-o" fname
        :in scad)))

(defn cider-show
  [mdl-data]
  (let [fname "_imgtmp.png"]
    (do (png! fname mdl-data)
        (clojure.java.io/file fname))))
