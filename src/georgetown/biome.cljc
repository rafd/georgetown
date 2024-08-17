(ns georgetown.biome)

(defn elevation->metres
  "0 to 1; returns 0 to "
  [elevation]
  (* elevation 4000))

(defn elevation->temperature [elevation]
  (- 23 ;; could vary the "latitude" of an island to vary this
     (* (/ 6.5 1000) (elevation->metres elevation))))

#_(elevation->temperature 0)
#_(elevation->temperature 1)

;; https://en.wikipedia.org/wiki/Biome#/media/File:Climate_influence_on_terrestrial_biome.svg
;; https://en.wikipedia.org/wiki/Biome#/media/File:Lifezones_Pengo.svg

(def ->color
  {:biome/tundra "#bce1e6"
   :biome/forest "#0b590b"
   :biome/prairie "#8eae18"
   :biome/desert "#d3b541"
   :biome/water "oklch(0.51 0.29 206.49)"

   :biome/boreal-forest "#0b590b"
   :biome/temperate-grassland "#8e8a0a"
   :biome/temperate-shrubland "#6f8e0a"
   :biome/temperate-forest "#168e0a"
   :biome/temperate-rainforest "#0d6629"
   :biome/savanna "#aeb824"
   :biome/rainforest "#0d6629"})

(def ->label
  {:biome/tundra "T"
   :biome/forest "F"
   :biome/prairie "P"
   :biome/desert "D"
   :biome/water "W"

   :biome/boreal-forest "BF"
   :biome/temperate-grassland "TG"
   :biome/temperate-shrubland "TS"
   :biome/temperate-forest "TF"
   :biome/temperate-rainforest "TR"
   :biome/savanna "S"
   :biome/rainforest "RF"})

#_(defn biome ;; complex
    [elevation humidity]
    (let [temperature (elevation->temperature elevation)]
      (cond
        (<= temperature 1)
        :biome/tundra
        (<= temperature 8)
        :biome/boreal-forest
        (<= temperature 22)
        (cond
          (<= humidity 0.2)
          :biome/temperate-grassland
          (<= humidity 0.4)
          :biome/temperate-shrubland
          (<= humidity 0.85)
          :biome/temperate-forest
          :else
          :biome/temperate-rainforest)
        :else
        (cond
          (<= humidity 0.2)
          :biome/desert
          (<= humidity 0.6)
          :biome/savanna
          :else
          :biome/rainforest))))

(defn biome
  [elevation humidity]
  (let [temperature (elevation->temperature elevation)]
    (cond
      (zero? elevation)
      :biome/water
      (<= temperature 1)
      :biome/tundra
      (<= temperature 8)
      :biome/forest ;; boreal forest - coniferous
      (<= temperature 22)
      (cond
        (<= humidity 0.1)
        :biome/desert ;; temperate grassland
        (<= humidity 0.4)
        :biome/prairie  ;; temperate shrubland
        (<= humidity 0.85)
        :biome/forest
        :else
        :biome/forest) ;; temperate rain forest
      :else
      (cond
        (<= humidity 0.2)
        :biome/desert
        (<= humidity 0.6)
        :biome/prairie ;; savanna
        :else
        :biome/forest ;; tropical rain forest
        ))))

(defn lot
  [lot]
  (biome (:lot/elevation lot) (:lot/moisture lot)))

(defn color
  [{:lot/keys [elevation moisture x y side?]}]
  (let [[lightness chroma hue]
        (case (biome elevation moisture)
          :biome/tundra [0.6 0.14 206.49]
          :biome/prairie [0.7 0.16 122.18]
          :biome/forest [0.4 0.13 142.81]
          :biome/water [0.6 0.29 206.49]
          :biome/desert [0.78 0.14 94.33])]
    (str "oklch("
         (+ lightness
            0.05
            (+ -0.15 (* 0.3 elevation))
            (if (even? (+ x y))
              -0.01
              0.01)
            (if side?
              -0.15
              0))
         " "
         (* chroma
            (- 1 elevation))
         " "
         hue
         ")")))

