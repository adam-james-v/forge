(ns forge.brep.mesh
  (:require [clojure.string :as str]
            [forge.utils :as utils]
            [forge.geom :as geom]
            [forge.delaunay :as delaunay]
            [forge.clip-ears :as clip-ears]
            [forge.brep.curves :as c]
            [forge.brep.surfaces :as s]))

(defn rect-grid
  [nx ny x-spacing y-spacing]
  (for [b (range ny)
        a (range nx)]
    [(* a x-spacing) (* b y-spacing)]))

(defn- tri-indices
  [seg idx]
  (if (and (not= 0 idx) (= 0 (int (mod (inc idx) seg))))
    ;; when idx is a multiple of seg, loop the surface back to u 0
    [[idx (+ idx seg) (+ 1 idx (- seg))]
     [(+ 1 idx (- seg)) (+ idx seg) (+ 1 idx)]]
    ;; otherwise, use simple idx shifting, assuming the u 1 edge pts are NOT present in the pts list
    [[idx (+ idx seg) (+ 1 idx)]
     [(+ 1 idx) (+ idx seg) (+ 1 idx seg) ]]))

(defn curve
  [curve seg]
  (let [eps 0.00001
        step (double (/ 1 seg))
        ts (range 0 1 step)]
    {:pts (map curve ts)}))

(defn surface
  ([surf seg] (surface seg seg))
  ([surf useg vseg]
   (let [eps 0.00001
         ustep (double (/ 1 useg))
         vstep (double (/ 1 vseg))
         uvs (rect-grid (inc useg) (inc vseg) ustep vstep)
         trifn (fn [idx]
                 (when-not
                     (and (not= 0 idx)
                          (= 0 (int (mod (inc idx) (inc useg)))))
                   (let [a idx
                         b (inc idx)
                         d (- (+ idx 1 useg) 0 #_(- useg vseg))
                         c (inc d)]
                     [[a b c] [a c d]]
                     #_[[idx (+ idx (inc useg)) (inc idx)]
                    [(inc idx) (+ 1 idx useg) (+ 2 idx useg)]])))
         quadfn (fn [[a b c] [_ _ d]] [a b c d])
         tris (->> (range (inc (count uvs)))
                   (take (inc (* (inc useg) vseg)))
                   (mapcat trifn)
                   (remove nil?)
                   (drop-last 2))
         quads (map #(apply quadfn %) (partition 2 tris))]
     {:pts (map #(apply surf %) uvs)
      :tris tris
      :quads quads})))
