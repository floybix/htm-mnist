(ns htm-mnist.gabor
  (:require [htm-mnist.images :as imgs]
            [clojure.java.io :as io])
  (:import GaborFilter
           (javax.imageio ImageIO)
           (java.awt.image BufferedImage)))

(comment

  (doseq [ori [0 45 90 135]]
    (gaborize-file
     (gabor-filter
      {:wavelength 20
       :orientations [(* ori Math/PI (/ 180.0))]
       :bandwidth 3.0
       :aspect-ratio 0.5
       :phase-offset (* Math/PI 1)
       :width 91
       :height 91})
     "public/gabortest-inv.jpg"
     (str "public/gabortest-inv-out-" ori ".jpg")))

  (doseq [ori [0 45 90 135]]
    (gaborize-file
     (gabor-filter
      {:wavelength 35
       :orientations [(* ori Math/PI (/ 180.0))]
       :bandwidth 3.0
       :aspect-ratio 0.25
       :phase-offset (* Math/PI 1)
       :width 91
       :height 91})
     "public/Gabor-ocr-inv.png"
     (str "public/gabor-ocr-inv-out-" ori ".jpg")))

  (->
   (gabor-kernel-img
    (gabor-filter
     {:wavelength 35
      :orientations [(* 45 Math/PI (/ 180.0))]
      :bandwidth 3
      :aspect-ratio 0.25
      :phase-offset (* Math/PI 1)
      :width 91
      :height 91}))
   (ImageIO/write "jpg" (io/file "gfilter.jpg")))

  ;; wavelength 5 => width 31
  ;; wavelength 3 => width 15
  ;; wavelength 20 => width 91

  )

(defn gabor-filter
  [{:keys [wavelength orientations phase-offset aspect-ratio
           bandwidth width height]
    :or {wavelength 1.0
         orientations [0.0]
         phase-offset 0.0
         aspect-ratio 0.5
         bandwidth 1.0
         width 3
         height 3}}]
  (GaborFilter. wavelength (double-array orientations)
                phase-offset aspect-ratio bandwidth width height))

(defn gaborize
  [^BufferedImage image ^GaborFilter filter]
  (.filter filter image nil))

(defn gaborize-file
  [filter in-file out-file]
  (-> (ImageIO/read (io/file in-file))
      (imgs/buffered-image)
      (gaborize filter)
      (ImageIO/write "jpg" (io/file out-file))))

(defn gabor-kernel-image
  [^GaborFilter gf]
  (let [w (.getWidth gf)
        h (.getHeight gf)
        kd (-> gf (.getKernel) (.getKernelData nil))
        z-min (apply min kd)
        z-max (apply max kd)
        z-span (- z-max z-min)
        bytes (map #(-> % (- z-min) (/ z-span) (* 255)) kd)
        im (imgs/bytes->image bytes [w h] 0)]
    (println "kernel min =" z-min "max =" z-max)
    im))
