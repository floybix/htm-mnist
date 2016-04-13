(ns htm-mnist.images
  (:require [clojure.data.codec.base64 :as base64]
            [hiccup.core :refer [html]])
  (:import (javax.imageio ImageIO)
           (java.awt.image BufferedImage RenderedImage)
           (java.awt Image Graphics Color RenderingHints)
           (java.awt.geom AffineTransform)
           (java.io ByteArrayOutputStream File)))

(defn inline-img-tag
  [^RenderedImage img attrs]
  (let [imgos (ByteArrayOutputStream.)]
    (ImageIO/write img "png" imgos)
    (let [imgbytes (.toByteArray imgos)
          data-url (str "data:image/png;base64,"
                        (String. (bytes (base64/encode imgbytes))))]
      (html [:img (assoc attrs :src data-url)]))))

(defn buffered-image
  [^Image img]
  (let [bimg (BufferedImage. (.getWidth img nil) (.getHeight img nil)
                             BufferedImage/TYPE_INT_RGB)
        g (.createGraphics bimg)]
    (.drawImage g img 0 0 nil)
    (.dispose g)
    bimg))

(defn bytes->image
  [bytes [w h] pad]
  (let [wp (+ w pad pad)
        hp (+ h pad pad)
        buffim (BufferedImage. wp hp BufferedImage/TYPE_INT_RGB)
        n (count bytes)]
    (loop [i 0
           bytes (seq bytes)]
      (if (>= i n)
        buffim
        (let [x (+ (mod i w) pad)
              y (+ (quot i w) pad)
              z (int (first bytes))]
          (.setRGB buffim x y (.getRGB (Color. z z z)))
          (recur (inc i) (rest bytes)))))))

(defn image->bytes
  [^BufferedImage img pad]
  (let [pw (.getWidth img nil)
        ph (.getHeight img nil)
        w (- pw pad pad)
        h (- ph pad pad)]
    (for [y' (range h)
          x' (range w)
          :let [y (+ y' pad)
                x (+ x' pad)]]
      (.getRed (Color. (.getRGB img x y))))))

;;; transformations

(defn scale-buffered
  [^BufferedImage bimg scale]
  (let [w (.getWidth bimg nil)
        h (.getHeight bimg nil)]
    (-> bimg
        (.getScaledInstance (* w scale) (* h scale) BufferedImage/SCALE_SMOOTH)
        (buffered-image))))

(defn transform-image
  [^BufferedImage img xform]
  (let [out-img (BufferedImage. (.getWidth img nil) (.getHeight img nil)
                                BufferedImage/TYPE_INT_RGB)
        g (.createGraphics out-img)
        hw (quot (.getWidth img) 2)
        hh (quot (.getHeight img) 2)]
    (-> g (.getRenderingHints) (.put RenderingHints/KEY_RENDERING
                                     RenderingHints/VALUE_RENDER_QUALITY))
    ;; center the transform, for rotations
    (.preConcatenate xform (AffineTransform/getTranslateInstance hw hh))
    (.concatenate xform (AffineTransform/getTranslateInstance (- hw) (- hh)))
    (.drawRenderedImage g img xform)
    (.dispose g)
    out-img)
  )

(defn affine
  [{:keys [rotate
           translate
           scale
           shear]}]
  (let [angle rotate
        [dx dy] translate
        [shx shy] shear
        xform (AffineTransform.)]
    (cond
      rotate (.rotate xform angle)
      scale (.scale xform scale scale)
      translate (.translate xform dx dy)
      shear (.shear xform shx shy))
    xform))
