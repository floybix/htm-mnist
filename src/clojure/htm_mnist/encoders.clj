(ns htm-mnist.encoders
  (:require [htm-mnist.images :as imgs]
            [htm-mnist.gabor :as gabor]
            [org.nfrac.comportex.topology :as topology]
            [org.nfrac.comportex.protocols :as p])
  (:import (javax.imageio ImageIO)
           (java.awt.image BufferedImage)))

(defn threshold-encoder
  [dimensions
   {:keys [threshold
           decode-sparsity
           decode-stimulus]
    :or {threshold 128
         decode-sparsity 0.05
         decode-stimulus 1}}]
  (let [topo (topology/make-topology dimensions)]
    (reify
      p/PTopological
      (topology [_]
        topo)
      p/PEncoder
      (encode [_ img-bytes]
        (keep-indexed (fn [i x]
                        (when (>= x threshold) i))
                      img-bytes))
      (decode [_ bit-votes n]
        (let [inbits (->> bit-votes
                          (filter (fn [[i nv]] (>= nv decode-stimulus)))
                          (keys)
                          (sort-by bit-votes >)
                          (take (* (p/size topo) decode-sparsity))
                          (set))
              img-bytes (map #(if (inbits %) 255 0)
                             (range (p/size topo)))]
          [{:value img-bytes}])))))

(defn- mean
  [xs]
  (when (seq xs)
    (/ (reduce + xs)
       (count xs))))

(defn shade-encoder
  [image-dimensions
   {:keys [thresholds
           decode-sparsity
           decode-stimulus
           force-2d?]
    :or {thresholds [0 128 256]
         decode-sparsity 0.05
         decode-stimulus 1}}]
  (let [n-shades (dec (count thresholds))
        [img-w img-h] image-dimensions
        dimensions (if force-2d?
                     [(* img-w n-shades) img-h]
                     [img-w img-h n-shades])
        topo (topology/make-topology dimensions)]
    (reify
      p/PTopological
      (topology [_]
        topo)
      p/PEncoder
      (encode [_ img-bytes]
        (loop [pixels img-bytes
               pix-i 0
               out (list)]
          (if-let [pixel (first pixels)]
            (let [out (loop [th-pairs (partition 2 1 thresholds)
                             th-i 0
                             out out]
                        (if-let [[lo hi] (first th-pairs)]
                          (recur (rest th-pairs)
                                 (inc th-i)
                                 (if (and (<= lo pixel)
                                          (< pixel hi))
                                   (let [j (if (not force-2d?)
                                             ;; shades as stacked images
                                             (+ pix-i (* th-i img-w img-h))
                                             ;; interspersed shades (keeping image topography)
                                             (+ th-i (* pix-i n-shades)))]
                                     (conj out j))
                                   out))
                          ;; checked all thresholds
                          out))]
              (recur (rest pixels)
                     (inc pix-i)
                     out))
            ;; checked all pixels
            out)))
      (decode [_ bit-votes n]
        (let [;; take midpoints of bands between thresholds
              shade-vals (->> thresholds
                              (partition 2 1)
                              (map mean))
              inbits (->> bit-votes
                          (filter (fn [[i nv]] (>= nv decode-stimulus)))
                          (keys)
                          (sort-by bit-votes >)
                          (take (* (p/size topo) decode-sparsity))
                          (set))
              img-bytes (map (fn [pix-i]
                               (let [vs (for [[th-i th-x] (map-indexed vector shade-vals)
                                              :let [bit-i (+ pix-i (* th-i img-w img-h))]
                                              :when (inbits bit-i)]
                                          th-x)]
                                 (if (seq vs)
                                   (mean vs)
                                   nil)))
                             (range (reduce * image-dimensions)))]
          [{:value img-bytes}])))))

(defn gabor-encoder
  [image-dimensions
   gabor-specs
   {:keys [threshold
           decode-stimulus
           force-2d?]
    :or {threshold 128
         decode-stimulus 1}}]
  (let [gf-bank (map gabor/gabor-filter gabor-specs)
        gf-size (->> (map :width gabor-specs)
                     (reduce max))
        pad (quot gf-size 2)
        [img-w img-h] image-dimensions
        dimensions (if force-2d?
                     [(* img-w (count gf-bank)) img-h]
                     [img-w img-h (count gf-bank)])
        topo (topology/make-topology dimensions)]
    (reify
      p/PTopological
      (topology [_]
        topo)
      p/PEncoder
      (encode [_ img-bytes]
        (let [img (imgs/bytes->image img-bytes image-dimensions pad)
              gf-parts (map #(imgs/image->bytes
                              (gabor/gaborize img %)
                              pad)
                           gf-bank)
              gf-out (if force-2d?
                       (apply interleave gf-parts)
                       (apply concat gf-parts))]
          (keep-indexed (fn [i x] (when (>= x threshold) i))
                        gf-out)))
      (decode [_ bit-votes n]
        (let [votes (->> bit-votes
                         (filter (fn [[i n]] (>= n decode-stimulus)))
                         )
              vote-scale (mean (map second votes))
              n-pix (reduce * image-dimensions)
              img-bytes (reduce
                         (fn [v [i n]]
                           (let [j (mod i n-pix)]
                             (assoc v j
                                    (-> (get v j)
                                        (max (* n (/ 255 vote-scale)))
                                        (min 255)))))
                         (vec (repeat n-pix 0))
                         votes)]
          [{:value img-bytes}])))))
