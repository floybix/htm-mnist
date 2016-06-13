(ns htm-mnist.classification
  (:require [org.nfrac.comportex.protocols :as p]
            [org.nfrac.comportex.cells :as cells]
            [org.nfrac.comportex.core :as core]))

(defmethod cells/spatial-pooling ::single-classification
  [layer ff-bits stable-ff-bits fb-cell-exc]
  (let [spec (:spec layer)
        base-sp (get-in spec [:classification :inference])
        out (cells/spatial-pooling (assoc-in layer [:spec :spatial-pooling]
                                             base-sp)
                                   ff-bits stable-ff-bits fb-cell-exc)]
    (if-let [curr (get-in spec [:classification :curr-class])]
      ;; override active columns
      (assoc out :active-cols (set [curr]))
      ;; if current classification not set, revert to usual behaviour
      out)))

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
                    :label-class label-class ;; TODO: don't want this big data structure in spec
                    :inference :standard}
   :adjust-overlap-duty-ratio 0 ;; disable
   :boost-active-duty-ratio 0 ;; disable
   :float-overlap-duty-ratio 0 ;; disable
   :ff-perm-init-hi 0.19
   :ff-perm-init-lo 0.19
   :ff-init-frac 1.0
   :proximal {:perm-connected 0.20
              :perm-inc 0.03
              :perm-dec 0.01
              :perm-punish 0.01
              :punish? true
              :max-segments 1
              :stimulus-threshold 1}
   })
