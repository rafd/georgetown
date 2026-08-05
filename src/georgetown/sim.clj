(ns georgetown.sim
  (:require
   [dat.api :as dat]
   [georgetown.schema :as schema]))

(defn random [generator-k]
  (assoc (->> schema/schema
              :entity/sim
              (map (fn [[k v]]
                     (when-let [g (get v generator-k)]
                       [k (g)])))
              (into {}))
         :sim/id (dat/uuid)))

#_(random ::schema/generator-immigrant)
#_(random ::schema/generator-baby)

