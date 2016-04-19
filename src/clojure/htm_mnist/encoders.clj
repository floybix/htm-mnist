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
   {:keys [juxtapose?
           thresholds
           decode-sparsity
           decode-stimulus]
    :or {juxtapose? false
         thresholds [128]
         decode-sparsity 0.05
         decode-stimulus 1}}]
  (let [thresholds (sort (conj thresholds 255))
        n-shades (count thresholds)
        [img-w img-h] image-dimensions
        dimensions (if juxtapose?
                     [img-w (* img-h n-shades)]
                     [(* img-w n-shades) img-h])
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
            (let [out (loop [ths thresholds
                             prev-th -1
                             th-i 0
                             out out]
                        (if-let [th (first ths)]
                          (recur (rest ths)
                                 th
                                 (inc th-i)
                                 (if (and (< prev-th pixel)
                                          (<= pixel th))
                                   (let [j (if juxtapose?
                                             ;; shades in blocks (for debugging)
                                             (+ pix-i (* th-i (count img-bytes)))
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
                              (cons 0)
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
                                              :let [bit-i (+ th-i (* pix-i n-shades))]
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
   {:keys [juxtapose?
           threshold]
    :or {juxtapose? false
         threshold 128}}]
  (let [gf-bank (map gabor/gabor-filter gabor-specs)
        gf-size (->> (map :width gabor-specs)
                     (reduce max))
        pad (quot gf-size 2)
        [img-w img-h] image-dimensions
        dimensions (if juxtapose?
                     [img-w (* img-h (count gf-bank))]
                     [(* img-w (count gf-bank)) img-h])
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
              gf-out (if juxtapose?
                       (apply concat gf-parts)
                       (apply interleave gf-parts))]
          (keep-indexed (fn [i x] (when (>= x threshold) i))
                        gf-out))))))
