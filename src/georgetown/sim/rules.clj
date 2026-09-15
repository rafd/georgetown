(ns georgetown.sim.rules
  (:require
    [georgetown.sim.rules.allocation :as rules.allocation]
    [georgetown.sim.rules.citizens :as rules.citizens]
    [georgetown.sim.rules.market :as rules.market]
    [georgetown.sim.rules.money :as rules.money]
    [georgetown.sim.rules.stats :as rules.stats]))

;; declaration order breaks ties

(def all-rules
  (vec
    (concat
      rules.market/rules
      rules.allocation/rules
      rules.citizens/rules
      rules.money/rules
      rules.stats/rules
      rules.citizens/population-rules)))

#_(georgetown.sim.engine/plan all-rules)
