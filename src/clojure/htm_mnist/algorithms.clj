(ns htm-mnist.algorithms
  (:require [org.nfrac.comportex.core :as core]
            [org.nfrac.comportex.cells :as cells]
            [org.nfrac.comportex.encoders :as e]
            [org.nfrac.comportex.protocols :as p]
            [org.nfrac.comportex.util :as util
             :refer [round]]
            [clojure.data.priority-map :refer [priority-map-by
                                               priority-map-keyfn-by]]))

(defn normalise-weights
  [syns final-total]
  (let [total (reduce + (keys syns))
        scale (/ final-total total)]
    (if (pos? total)
      (util/remap #(* % scale) syns)
      syns)))

(defn connected-active-synapses
  [sg target-id active-source-set pcon]
  (->> (p/in-synapses sg target-id)
       (into {}
             (filter
              (fn [[src p]]
                (and (>= p pcon)
                     (active-source-set src)))))
       ))

(defn adjust-effective-synapses
  [hit-target t-eff-syns selected-syns sg alpha]
  (reduce (fn [t-eff-syns [src p]]
            ;; reduce effect of this source by p.
            ;; do this adjustment on all its targets:
            ;; (except the originating hit)
            (->>
             (p/targets-connected-from sg src)
             (filter #(and (t-eff-syns %)
                           (not= % hit-target)))
             (reduce (fn [t-eff-syns tgt-id]
                       (update-in t-eff-syns [tgt-id src]
                                  #(max 0.0 (- % (* p alpha)))))
                     t-eff-syns)))
          t-eff-syns
          selected-syns))

(defn suck-decorrelation
  "Given a list of active sources and a synapse graph, return a list
  of targets to become active. Both the input sources and the output
  targets are ordered by activation strength/speed, decreasing.  When
  a target is selected it sucks out (a fraction of) the excitatory
  effect of its sources, biasing further selected targets to be driven
  by different sources; segregating inputs between columns."
  [sg active-sources pcon n-on alpha]
  (let [source-set (set active-sources)
        ;; a map from target id to its active connected sources:
        ;; {target-id {source-id permanence}}
        t-asyns (->> active-sources
                     (into
                      {}
                      (comp (mapcat #(p/targets-connected-from sg %))
                            (distinct)
                            (map (fn [tgt-id]
                                   [tgt-id
                                    (connected-active-synapses sg tgt-id
                                                               source-set pcon)])))))]
    (loop [;; target ordered (decreasing) by sum of effective synapse weights
           t-eff-syns (into (priority-map-keyfn-by (fn [syns]
                                                     (reduce + (vals syns)))
                                                   >)
                            t-asyns)
           selected (priority-map-by >)]
      (if (< (count selected) n-on)
        ;; select target with greatest excitation,
        (if-let [[target eff-syns] (peek t-eff-syns)]
          ;; then inhibit the effect of all its sources.
          (let [asyns (t-asyns target)
                adj-t-eff-syns (adjust-effective-synapses
                                target (pop t-eff-syns)
                                asyns sg alpha)]
            (recur adj-t-eff-syns
                   (assoc selected target (reduce + (vals eff-syns)))))
          ;; ran out of targets (weird - maybe overfitted)
          selected)
        ;; reached desired number of targets to become active
        selected))))

(defmethod cells/spatial-pooling ::suck-decorrelation
  [ff-bits stable-ff-bits proximal-sg boosts topology inh-radius fb-cell-exc spec]
  (let [
        ;; TODO:
        ;; (:spontaneous-activation? spec)
        ;;weight (:distal-vs-proximal-weight spec)
        ;;fb-col-exc (->> (cells/best-by-column fb-cell-exc)
        ;;                (util/remap #(* % weight)))

        level (:activation-level spec)
        n-on (max 1 (round (* level (p/size topology))))
        col-seg-selected (suck-decorrelation proximal-sg ff-bits
                                             (:perm-connected (:proximal spec))
                                             n-on
                                             (:suck-alpha spec))
        ;; multiple segments? makes any sense?
        ;; these both keyed by [col 0]
        [col-exc ff-seg-paths]
        (cells/best-segment-excitations-and-paths col-seg-selected)

        a-cols (into #{} (map first) (keys col-seg-selected))]
    {:active-cols a-cols
     :matching-ff-seg-paths ff-seg-paths
     :col-overlaps col-exc}))
