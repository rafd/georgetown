(ns georgetown.sim.citizen
  (:require
    [dat.uuid :as uuid]
    [georgetown.sim.schema :as schema]
    [georgetown.sim.time :as time]))

(defn random [generator-k]
  (assoc (->> schema/schema
              :entity/sim
              (map (fn [[k v]]
                     (when-let [g (get v generator-k)]
                       [k (g)])))
              (into {}))
         :sim/id (uuid/random)))

(defn age-in-years [sim]
  (time/ticks->years (:sim/age-ticks sim)))

#_(random ::schema/generator-immigrant)
#_(random ::schema/generator-baby)
