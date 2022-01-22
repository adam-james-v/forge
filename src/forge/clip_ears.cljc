(ns forge.clip-ears
  (:require [clojure.set]
            [forge.utils :as utils]))

(defn remove-colinears
  [pts]
  (let [indices (zipmap pts (range (count pts)))
        tris (partition 3 1 (concat pts (take 2 pts)))
        clpts (set (map second (filter #(apply utils/colinear? %) tris)))
        xindices (vals (apply dissoc indices clpts))]
    (map #(get pts %) xindices)))

(defn- clip-one-ear
  [pts]
  (let [pts (vec pts)
        indices (zipmap pts (range (count pts)))
        corners (->> pts
                     (#(concat % (take 2 %)))
                     (partition 3 1)
                     #_(filter #(#{:convex} (apply utils/corner-condition %))))
        clear? (fn [corner]
                 (not (seq (filter #(utils/pt-inside? corner %) pts))))
        tri (first (filter clear? corners))]
    {:pts pts
     :npts (mapv #(get pts %) (sort (vals (dissoc indices (second tri)))))
     :tri tri}))

(defn triangulate
  ([pts] (triangulate {:indices (zipmap pts (range (count pts)))} pts [] []))
  ([data pts tris indices]
   (if (< (count pts) 3)
     (merge data {:tris tris :tri-indices indices})
     (let [{:keys [npts tri]} (clip-one-ear pts)
           local-indices (mapv #(get (:indices data) %) tri)]
       (recur data npts (conj tris tri) (conj indices local-indices))))))
