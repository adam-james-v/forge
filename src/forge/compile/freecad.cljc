(ns forge.compile.freecad
  (:require [clojure.java.shell :refer [sh]]
            [clojure.string :as str]
            [clojure.core.matrix]
            [forge.utils :as utils]
            [forge.model :as mdl]))

(defn get-name
  [s]
  (-> s
      str/split-lines
      last
      (str/split #"=")
      first
      str/trim))

(defn vecstr
  [v]
  (str "Vector(" (str/join ", " v) ")"))

(defn mtxstr
  [[& vs]]
  (str "Matrix(" (str/join ", " (apply concat vs)) ")"))

(defn vecrot
  [v]
  (vec (concat (drop 1 v) (take 1 v))))

(defmulti write-expr
  (fn [[form & args]]
    (if (keyword? form) form :list)))

(defmethod write-expr :default
  [[form & args]]
  ["//(" form args ")"])

(defmethod write-expr :list
  [forms]
  (str/join "\n" (map write-expr forms)))

(defn write-block
  [block]
  (mapcat write-expr block))

(declare map-to-arg-string)

(defn make-arguments [args]
  (let [arg (first args)
        rest (rest args)
        piece (cond
               (map? arg) (map-to-arg-string arg)
               (coll? arg) (str "[" (make-arguments arg) "]")
               :else arg)]
    (if (empty? rest)
      piece
      (str/join ", " [piece (make-arguments rest)]))))

(defn map-to-arg-string [m]
  (str/join ", " (map (fn [[k v]] (str (name k) "=" (make-arguments [v])) ) m)))

(def fcs-preamble
  "import FreeCAD
from FreeCAD import Base, Vector, Matrix, Placement, Rotation
from math import pi, sin, cos

import Part
import Sketcher
import Import
import sys, os

doc = FreeCAD.activeDocument()
doc_name = \"freecad_clj\"

def clear_doc():
    for obj in doc.Objects:
        doc.removeObject(obj.Name)

def setview():
    FreeCAD.Gui.SendMsgToActiveView(\"ViewFit\")
    FreeCAD.Gui.activeDocument().activeView().viewAxometric()

if doc is None:
    FreeCAD.newDocument(doc_name)
    FreeCAD.setActiveDocument(doc_name)
    doc = FreeCAD.activeDocument()
else:
    clear_doc()

# EPS= tolerance to use to cut the parts
EPS = 0.10
EPS_C = EPS * -0.5\n\n")

(def fcs-postamble
  "doc.recompute()

pathname = os.path.dirname(sys.argv[0])
stepname = os.path.abspath(pathname) + \"/out.step\"
fcname = os.path.abspath(pathname) + \"/out.FCStd\"

#Import.export(doc.RootObjects, stepname)
#doc.saveAs(fcname)")

(defmethod write-expr :circle
  [[_ {:keys [r]}]]
  (let [name (gensym "sk_circle_")]
    (str name " = " "Part.makeCircle(" r  ")")))

(defn line-segment
  [[a b]]
  (str "Part.LineSegment(" (str/join ", " (map vecstr [a b])) ")"))

(defn coincident-constraint
  [[ia ib]]
  (str "Sketcher.Constraint('Coincident'," ia ",2," ib ",1)"))

(defn build-polygon-old
  [pts]
  (let [name (gensym "sk_")
        indices (partition 2 1 (conj (vec (range (count pts))) 0))
        lines (partition 2 1 (conj pts (first pts)))]
    (apply str [name " = doc.addObject('Sketcher::SketchObject','" name "')\n"
                "doc." name ".addGeometry(["
                (str/join "," (map line-segment lines))
                "],False)\n\n"
                "doc." name ".addConstraint(["
                (str/join "," (map coincident-constraint indices))
                "])\n"])))

(defn build-polygon
  [pts]
  (let [pts (conj pts (first pts))]
    (str "Part.makePolygon(["
         (str/join ", " (map vecstr pts))
         "])")))

(defmethod write-expr :polygon
  [[_ {:keys [pts]}]]
  (let [name (gensym "sk_polygon_")]
    (str name " = " (build-polygon pts))))

(defmethod write-expr :square
  [[_ {:keys [x y]}]]
  (let [pts [[(/ x -2) (/ y -2) 0] [(/ x 2)  (/ y -2) 0]
             [(/ x 2)  (/ y 2)  0] [(/ x -2) (/ y 2)  0]]
        name (gensym "sk_sq_")]
    (str name " = " (build-polygon pts))))

(defmethod write-expr :sphere
  [[_ {:keys [r]}]]
  (let [name (gensym "sphere_")]
    (str name " = " "Part.makeSphere(" r ")")))

(defmethod write-expr :cube
  [[_ {:keys [x y z]}]]
  (let [name (gensym "cube_")
        pos (map #(/ % -2.0) [x y z])]
    (str name " = " "Part.makeBox(" (str/join ", " [x y z]) ")"
         ".translate(" (vecstr pos) ")")))

(defmethod write-expr :cylinder
  [[_ {:keys [r h]}]]
  (let [name (gensym "cylinder_")
        pos (map #(/ % -2) [0 0 h])]
    (str name " = " "Part.makeCylinder(" (str/join ", " [r h]) ")"
         ".translate(" (vecstr pos) ")")))

(defn build-face
  [pts]
  (str "Part.Face(" (build-polygon pts) ")"))

(defmethod write-expr :polyhedron
  [[_ {:keys [pts faces]}]]
  (let [name (gensym "polyhedron_")
        get-pts (fn [face] (mapv #(get (vec pts) %) face))
        faces (mapv get-pts faces)]
    (str name " = " "Part.makeSolid(Part.makeShell(" 
         (str/join ", " faces) "))")))

(defmethod write-expr :extrude-linear
  [[_ {:keys [height]} form]]
  (let [sk (write-expr form)
        name (gensym "extrude_")
        sk-name (get-name sk)]
    (str sk "\n"
         name " = " "Part.Face(Part.Wire(" sk-name "))"
         ".extrude(" (vecstr [0 0 height]) ")")))

(defmethod write-expr :extrude-rotate
  [[_ {:keys [angle]} form]]
  (let [angle (if angle angle 360)
        sk (write-expr form)
        name (gensym "revolve_")
        sk-name (get-name sk)]
    (str sk "\n"
         name " = " "Part.Face(Part.Wire(" sk-name "))"
         ".rotate(" (vecstr [0 0 0]) "," (vecstr [1 0 0]) ", 90" ")"
         ".revolve(" (vecstr [0 0 0]) "," (vecstr [0 0 1]) "," angle ")")))

(defn v* [a b] (mapv * a b))

(defn normalize
  "find the unit vector of a given vector"
  [v]
  (let [m (Math/sqrt (reduce + (v* v v)))]
    (mapv / v (repeat m))))

;; rotation matrices sourced from:
;; https://sites.google.com/site/glennmurray/Home/rotation-matrices-and-formulas/rotation-about-an-arbitrary-axis-in-3-dimensions
;; zyx-rotation-matrix from section 3
;; axis-rotation-matrix from section 5.2

(defn axis-rotation-matrix [a [x y z]]
  (let [[u v w] (normalize [x y z])
        c #(Math/cos %)
        s #(Math/sin %)]
    [[(+ (* u u) (* (- 1 (* u u)) (c a)))
      (- (* v u (- 1 (c a))) (* w (s a)))
      (+ (* w u (- 1 (c a))) (* v (s a)))
      0]

     [(+ (* u v (- 1 (c a))) (* w (s a)))
      (+ (* v v) (* (- 1 (* v v)) (c a)))
      (- (* w v (- 1 (c a))) (* u (s a)))
      0]

     [(- (* u w (- 1 (c a))) (* v (s a)))
      (+ (* v w (- 1 (c a))) (* u (s a)))
      (+ (* w w) (* (- 1 (* w w)) (c a)))
      0]

     [0 0 0 1]]))

(defn build-translate
  [[x y z] form]
  (str form ".translate(" (vecstr [x y z]) ")"))

(defmethod write-expr :translate
  [[_ {:keys [x y z]} forms]]
  (let [xforms (mapcat str/split-lines (map write-expr forms))]
    (str/join "\n" (map #(build-translate [x y z] %) xforms))))

(defn build-rotate
  [a [x y z] form]
  (str form ".rotate(" (vecstr [0 0 0]) ", " (vecstr [x y z]) ", " a ")"))

(defmethod write-expr :rotatev
  [[_ {:keys [a x y z]} forms]]
  (let [xforms (mapcat str/split-lines (map write-expr forms))]
    (str/join "\n" (map #(build-rotate a [x y z] %) xforms))))

(defmethod write-expr :rotatec
  [[_ {:keys [x y z]} forms]]
  (let [xforms (map #(->> %
                          (rotate z [0 0 1])
                          (rotate y [0 1 0])
                          (rotate x [1 0 0])) forms)]
    (str/join "\n" (map write-expr xforms))))

(defmethod write-expr :color
  [[_ {:keys [a x y z]} forms]]
  (str/join "\n" (map write-expr forms)))

(defn build-cut
  [str-a str-b]
  (let [name (gensym "difference_")]
    (str str-a "\n"
         str-b "\n"
         name " = " (get-name str-a) ".cut(" (get-name str-b) ")")))

(defmethod write-expr :difference
  [[_ _ forms]]
  (let [xforms (mapcat str/split-lines (map write-expr forms))]
    (str (reduce build-cut xforms) ".removeSplitter()")))

(defn build-fuse
  [str-a str-b]
  (let [name (gensym "union_")]
    (str str-a "\n"
         str-b "\n"
         name " = " (get-name str-a) ".fuse(" (get-name str-b) ")")))

(defmethod write-expr :union
  [[_ _ forms]]
  (let [xforms (mapcat str/split-lines (map write-expr forms))]
    (str (reduce build-fuse xforms) ".removeSplitter()")))

(defn build-common
  [str-a str-b]
  (let [name (gensym "intersection_")]
    (str str-a "\n"
         str-b "\n"
         name " = " (get-name str-a) ".common(" (get-name str-b) ")")))

(defmethod write-expr :intersection
  [[_ _ forms]]
  (let [xforms (mapcat str/split-lines (map write-expr forms))]
    (str (reduce build-common xforms) ".removeSplitter()")))

(defn write-fcs
  [& blocks]
  (let [block-strs (map write-expr blocks)
        names (map #(str "Part.show(" (get-name %) ", '" (gensym "fcs_") "')") block-strs)]
    (str/join "\n"
     (concat
      [fcs-preamble]
      block-strs
      names
      [fcs-postamble]))))

(defn ->step
  [block]
  (sh "freecad" "-c" (write-fcs block)))
