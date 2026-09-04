(ns georgetown.sim.citizen
  (:require
    [dat.uuid :as uuid]
    [georgetown.sim.schema :as schema]
    [georgetown.sim.time :as time]))

(defn random [generator-k]
  (assoc (->> schema/schema
              :entity/citizen
              (map (fn [[k v]]
                     (when-let [g (get v generator-k)]
                       [k (g)])))
              (into {}))
         :citizen/id (uuid/random)))

(defn age-in-years [citizen]
  (time/ticks->years (:citizen/age-ticks citizen)))

#_(random ::schema/generator-immigrant)
#_(random ::schema/generator-baby)
