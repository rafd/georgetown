(ns georgetown.sim.rules.stats
  (:require
    [com.rpl.specter :as x]
    [georgetown.sim.citizen :as citizen]))

(defn public-stats
  {:rule/description "Joy and the public per-tick stats snapshot"
   :rule/inputs #{:world/epoch :world/shift :world/citizens
                  :world/food-stats :world/shelter-stats :world/allocation-stats
                  :world/dead-citizen-ids :world/emigrant-citizen-ids
                  :world/hungry-citizen-ids :world/unhoused-citizen-ids
                  :world/final-citizen-savings :world/final-net-money-balance
                  :world/final-player-balance-total :world/government-money-balance
                  :world/helicopter-money :world/cash-ratio-before :world/cash-ratio-after
                  :world/interest-rate}
   :rule/outputs #{:world/public-stats :world/joy}}
  [{:world/keys [epoch shift citizens
                 food-stats shelter-stats allocation-stats
                 dead-citizen-ids emigrant-citizen-ids
                 hungry-citizen-ids unhoused-citizen-ids
                 final-citizen-savings final-net-money-balance
                 final-player-balance-total government-money-balance
                 helicopter-money cash-ratio-before cash-ratio-after
                 interest-rate]}]
  (let [population (count citizens)
        joy (->> citizens
                 vals
                 (map (fn [citizen]
                        (- 1 (citizen/mean-stress citizen))))
                 (reduce + 0.0))]
    {:world/joy joy
     :world/public-stats
     ;; transit is struggling with bignums(?)
     ;; for now, just cast all to double
     (x/transform
       (x/walker number?)
       double
       {:sim.out/epoch epoch
        :sim.out/shift shift
        :sim.out/population population
        :sim.out/deaths (count dead-citizen-ids)
        :sim.out/emigrations (count emigrant-citizen-ids)
        :sim.out/total-citizen-savings final-citizen-savings
        :sim.out/mean-physical-stress (if (pos? population)
                                        (/ (->> citizens
                                                vals
                                                (map :citizen/physical-stress)
                                                (reduce + 0.0))
                                           population)
                                        0)
        :sim.out/mean-mental-stress (if (pos? population)
                                      (/ (->> citizens
                                              vals
                                              (map :citizen/mental-stress)
                                              (reduce + 0.0))
                                         population)
                                      0)
        :sim.out/employed-count (:employed-count allocation-stats)
        :sim.out/idle-count (:idle-count allocation-stats)
        :sim.out/hungry-count (:unserved-count food-stats)
        :sim.out/unhoused-count (:unserved-count shelter-stats)
        :sim.out/citizen-states
        (let [citizen-activities (:citizen-activities allocation-stats)]
          (->> citizens
               (map (fn [[citizen-id citizen]]
                      [citizen-id
                       {:citizen-state/hungry? (contains? hungry-citizen-ids citizen-id)
                        :citizen-state/unhoused? (contains? unhoused-citizen-ids citizen-id)
                        :citizen-state/last-activity (get citizen-activities citizen-id)
                        :citizen-state/savings (:citizen/savings citizen)}]))
               (into {})))
        :sim.out/resources {:resource/food food-stats
                            :resource/shelter shelter-stats}
        :sim.out/joy joy
        :sim.out/net-money-balance final-net-money-balance
        :sim.out/player-money-balance final-player-balance-total
        :sim.out/government-money-balance government-money-balance
        :sim.out/helicopter-money helicopter-money
        :sim.out/cash-ratio-before cash-ratio-before
        :sim.out/cash-ratio-after cash-ratio-after
        :sim.out/stabilization-rate interest-rate})}))

(def rules
  [#'public-stats])
