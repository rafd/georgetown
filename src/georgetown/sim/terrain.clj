(ns georgetown.sim.terrain
  (:require
    [georgetown.sim.util.noise :as noise]))

(defn normalize-kernel [kernel]
  (let [sum (reduce + (mapcat identity kernel))]
    (mapv (fn [x] (mapv #(/ % sum) x)) kernel)))

(defn gaussian-kernel [size sigma]
  (let [center (quot size 2)
        factor (/ 1 (* 2 Math/PI sigma sigma))
        gaussian (fn [x y]
                   (* factor (Math/exp (- (/ (+ (* (- x center) (- x center))
                                               (* (- y center) (- y center)))
                                            (* 2 sigma sigma))))))]
    (normalize-kernel
     (vec (for [x (range size)]
            (vec (for [y (range size)]
                   (gaussian x y))))))))

(defn convolve [matrix kernel]
  (let [rows (count matrix)
        cols (count (first matrix))
        ksize (count kernel)
        kcenter (quot ksize 2)]
    (vec (for [i (range rows)]
           (vec (for [j (range cols)]
                  (reduce + (for [ki (range ksize)
                                  kj (range ksize)]
                              (let [ii (- i (- ki kcenter))
                                    jj (- j (- kj kcenter))]
                                (if (and (>= ii 0) (< ii rows) (>= jj 0) (< jj cols))
                                  (* (get-in matrix [ii jj]) (get-in kernel [ki kj]))
                                  0))))))))))

(defn gaussian-blur [size sigma matrix]
  (let [kernel (gaussian-kernel size sigma)]
    (convolve matrix kernel)))

#_(gaussian-blur
    3 1.0
    [[1 2 3]
     [4 5 6]
     [7 8 9]])

(defn clamp [low high value]
  (max low (min high value)))

(defn moisture
  [elevations]
  ;; moist wind blows from the west and loses moisture as it travels upwards
  ;; https://en.wikipedia.org/wiki/Orographic_lift
  (->> elevations
       (mapv (fn [row]
               (->> row
                    (cons 0)
                    (partition 2 1 [0])
                    (map (fn [[a b]]
                           [b (- b a)]))
                    (reduce (fn [memo [elevation elevation-delta]]
                              (let [moisture-loss (clamp 0.0 1.0
                                                         ;; some amount from traveling over
                                                         ;; more when elevation increases
                                                         (+ (* (::air memo)
                                                               0.05)
                                                            (* (::air memo)
                                                               (* 10
                                                                  (max 0 elevation-delta)
                                                                  (max 0 elevation-delta)
                                                                  elevation
                                                                  elevation))))]
                                (-> memo
                                    (update ::air +
                                            (- moisture-loss)
                                            ;; low lands evaporate some back
                                            (+ (* moisture-loss
                                                  (- 1 elevation)))
                                            ;; water adds more
                                            (if (zero? elevation)
                                              0.1
                                              0)
                                            )
                                    (update ::land conj (clamp 0.0 1.0
                                                               ;; scale air to land values
                                                               (* 12
                                                                  moisture-loss))))))
                            {::air 1.0
                             ::land []})
                    ::land
                    ((fn [v]
                       (subvec v 0 (inc (count row))))))))
       ;; blur, to simulate not "strictly north-south" wind
       (gaussian-blur 3 0.7)
       (gaussian-blur 5 0.5)
       (gaussian-blur 7 0.3)))

#_(moisture [[0.01 0.01 0.01 0.01]])

#_(moisture [[0.02 0.1 0.1 0.9 0.1]])

(defn gaussian [x sigma]
  (Math/exp (- (/ (Math/pow (- x 0.5) 2)
                  (* 2 (Math/pow sigma 2))))))

#_(* (gaussian 0.9 0.2)
     (gaussian 0.9 0.2))

(defn make-interpolator
  [[x0 x1] [y0 y1]]
  (let [m (/ (- y1 y0) (- x1 x0))
        b (- y0 (* m x0))]
    (fn [x]
      (+ (* m x) b))))

(defn elevation [seed]
  (let [initial (noise/grid {:width 20 :height 20
                             :scale 0.1 :seed seed :bands 10})
        gaussified (for [x (range 20)
                         y (range 20)]
                     (* ;; power, for steeper terrain
                        (Math/pow (get-in initial [x y]) 4)
                        ;; reduce height at edges (0.5 => 30% at edges)
                        (gaussian (/ x 19) 0.5)
                        (gaussian (/ y 19) 0.5)))
        rescale (make-interpolator
                  [(apply min gaussified)
                   (apply max gaussified)]
                  [0 1])
        threshold-water (fn [e]
                          (if (<= e 0.003)
                            0
                            e))
        final (->> gaussified
                   (mapv rescale)
                   (mapv threshold-water))]
    (vec
      (for [x (range 20)]
        (vec
          (for [y (range 20)]
            (get final (+ (* x 20) y))))))))

#_(elevation (rand-int 5000))

(defn lot-properties [seed]
  (let [e (elevation seed)]
    {::elevation e
     ::wind :TODO
     ::temperature :todo
     ::moisture (moisture e)}))
