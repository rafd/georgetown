(ns georgetown.sim.island
  (:require
    [bloom.commons.uuid :as uuid]
    [georgetown.sim.citizen :as citizen]
    [georgetown.sim.schema :as schema]
    [georgetown.sim.terrain :as terrain]))

(defn generate
  ([seed]
   {:island/id (uuid/random)
    :island/government-money-balance 10000
    :island/epoch 0
    :island/joy 0
    :island/seed seed
    :island/sims (repeatedly 10 (fn [] (citizen/random ::schema/generator-immigrant)))
    :island/lots
    (let [properties (terrain/lot-properties seed)]
      (for [x (range 20)
            y (range 20)]
        {:lot/id (uuid/random)
         :lot/x x
         :lot/y y
         :lot/elevation (get-in properties [::terrain/elevation x y])
         :lot/moisture (get-in properties [::terrain/moisture x y])}))})
  ([] (generate (rand-int 5000))))
