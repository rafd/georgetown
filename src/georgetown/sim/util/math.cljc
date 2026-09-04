(ns georgetown.sim.util.math)

(defn random-normal []
  (let [first-uniform (Math/random)
        second-uniform (Math/random)]
    (* (Math/sqrt (* -2 (Math/log first-uniform)))
       (Math/cos (* 2 Math/PI second-uniform)))))

(defn random-gamma [shape]
  (if (< shape 1)
    (let [uniform (Math/random)]
      (* (random-gamma (inc shape))
         (Math/pow uniform (/ 1 shape))))
    (let [offset (- shape (/ 1 3))
          scale (/ 1 (Math/sqrt (* 9 offset)))]
      (loop []
        (let [standard-normal (random-normal)
              cube (Math/pow (+ 1 (* scale standard-normal)) 3)]
          (if (pos? cube)
            (let [uniform (Math/random)
                  candidate (* offset cube)]
              (if (< (Math/log uniform)
                     (+ (* 0.5 standard-normal standard-normal)
                        (- offset candidate)
                        (* offset (Math/log cube))))
                candidate
                (recur)))
            (recur)))))))
(defn beta [shape-alpha shape-beta]
  ;; https://www.desmos.com/calculator/mnvwjlvnyj
  (let [alpha-gamma (random-gamma shape-alpha)
        beta-gamma (random-gamma shape-beta)]
    (/ alpha-gamma (+ alpha-gamma beta-gamma))))


