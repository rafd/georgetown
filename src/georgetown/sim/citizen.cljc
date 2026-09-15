(ns georgetown.sim.citizen
  (:require
    [dat.uuid :as uuid]
    [georgetown.sim.schema :as schema]
    [georgetown.sim.time :as time]
    [georgetown.sim.util.math :as math]))

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

(defn mean-stress [citizen]
  (/ (+ (:citizen/physical-stress citizen)
        (:citizen/mental-stress citizen))
     2))

(defn stress-citizen [citizen amount]
  (-> citizen
      (update :citizen/physical-stress (fn [stress] (math/clamp01 (+ stress amount))))
      (update :citizen/mental-stress (fn [stress] (math/clamp01 (+ stress amount))))))

#_(random ::schema/generator-immigrant)
#_(random ::schema/generator-baby)
