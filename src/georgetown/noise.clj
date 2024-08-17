(ns georgetown.noise
  (:import
    [org.kdotjpg.noise OpenSimplex2S]))

;; https://github.com/KdotJPG/OpenSimplex2/blob/master/java/OpenSimplex2.java

(defn grid
  [{:keys [width height scale bands seed]}]
  (vec
    (for [x (range width)]
      (vec
        (for [y (range height)]
          (let [nx (* x scale)
                ny (* y scale)
                value (OpenSimplex2S/noise2 seed nx ny)
                normalized (/ (+ value 1.0) 2.0)]
            (/ (Math/round (* normalized (double (dec bands))))
               (double (dec bands)))))))))

#_(grid {:width 100 :height 100 :scale 0.1 :seed 42 :bands 2})

(comment
  (import '(java.awt.image BufferedImage)
          '(javax.imageio ImageIO)
          '(java.io File))

  (defn ->image [filename data]
    (let [height (count data)
          width  (count (first data))
          image  (BufferedImage. width height BufferedImage/TYPE_BYTE_GRAY)]
      (doseq [y (range height)
              x (range width)]
        (let [value (get-in data [y x])
              ;; Convert the value from a range [0, 1] to [0, 255]
              gray-value (int (* 255 value))]
          ;; Set the pixel color
          (.setRGB image x y (bit-or (bit-shift-left gray-value 16)
                                     (bit-shift-left gray-value 8)
                                     gray-value))))
      ;; Write the image to a file
      (ImageIO/write image "png" (File. filename))))

  (->image "noise.png" (grid {:width 20 :height 20 :scale 0.025 :seed 43 :bands 11}))
  (->image "noise.png" (grid {:width 100 :height 100 :scale 0.01 :seed 45 :bands 11}))
  (->image "noise.png" (grid {:width 20 :height 20 :scale 0.1 :seed 60 :bands 10})))





