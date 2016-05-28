(ns htm-mnist.classification
  (:require [org.nfrac.comportex.protocols :as p]
            [org.nfrac.comportex.cells :as cells]
            [org.nfrac.comportex.core :as core]))

(defn spatial-pooling-out
  [columns]
  {:active-cols (set columns)
   :col-overlaps (->> columns
                      (map (fn [col]
                             [[col 0] 1.0]))
                      (into {}))
   :matching-ff-seg-paths (->> columns
                               (map (fn [col]
                                      (let [v [[col 0 0] 1.0]
                                            k [col 0]]
                                        [k v])))
                               (into {}))})

(defmethod cells/spatial-pooling ::single-classification
  [layer ff-bits stable-ff-bits fb-cell-exc]
  (let [spec (:spec layer)]
    (if-let [curr (get-in spec [:classification :curr-class])]
      (spatial-pooling-out [curr])
      ;; if current classification not set, revert to usual behaviour
      (cells/spatial-pooling
       (assoc-in layer [:spec :spatial-pooling]
                 (get-in spec [:classification :inference]))
       ff-bits stable-ff-bits fb-cell-exc))))

(defn htm-step-with-label
  [htm inval]
  (let [label (:label inval)
        cspec-path [:regions (last (core/region-keys htm)) :layer-3 :spec :classification]
        cspec (get-in htm cspec-path)
        lookup (:label-class cspec)]
    (assert cspec)
    (-> htm
        (update-in cspec-path assoc :curr-class (lookup label))
        (p/htm-step inval)
        (update-in cspec-path assoc :curr-class nil))))

(defn classification-layer-spec
  [label-class]
  {:column-dimensions [(count label-class)]
   :activation-level (/ 1 (count label-class))
   :depth 1
   :lateral-synapses? false
   :distal {:learn? false}
   :spatial-pooling ::single-classification
   :classification {:curr-class nil
                    :label-class label-class
                    :inference :standard}
   :adjust-overlap-duty-ratio 0 ;; disable
   :boost-active-duty-ratio 0 ;; disable
   :ff-init-frac 1.0
   :ff-perm-init-hi 0.191
   :ff-perm-init-lo 0.19
   :proximal {:perm-connected 0.20
              :perm-inc 0.01
              :perm-dec 0.01}
   })
