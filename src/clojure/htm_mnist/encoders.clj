(ns htm-mnist.encoders
  (:require [htm-mnist.images :as imgs]
            [htm-mnist.gabor :as gabor]
            [org.nfrac.comportex.topology :as topology]
            [org.nfrac.comportex.protocols :as p])
  (:import GaborFilter
           (javax.imageio ImageIO)
           (java.awt.image BufferedImage)))

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
