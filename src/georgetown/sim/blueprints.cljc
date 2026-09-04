(ns georgetown.sim.blueprints
  (:require
    [malli.core :as malli]
    [georgetown.sim.types :as types]))

(def blueprints
  (->> [{:blueprint/id :improvement.type/house
         :blueprint/label "House"
         :blueprint/icon "🏠"
         :blueprint/description "Provides shelter"
         :blueprint/player-buildable? true
         :blueprint/price 5000
         :blueprint/offerables
         [{:offerable/id :offer/house.rental
           :offerable/label "Rental"
           :offerable/capacity 2
           :offerable/time-shifts #{:time-shift/morning
                                    :time-shift/afternoon
                                    :time-shift/evening
                                    :time-shift/night}
           :offerable/var [{:var/id :var/rent-rate
                            :var/label "Rent"
                            :var/unit [:/ :resource/money :resource/shelter]}]
           :offerable/effects
           [[:effect.direction/from-citizen :resource/money :var/rent-rate]
            [:effect.direction/to-citizen :resource/shelter 1]
            [:effect.direction/to-player :resource/money :var/rent-rate]]}
          {:offerable/id :offer/house.sleep
           :offerable/label "A Good Nights Sleep"
           :offerable/capacity 2
           :offerable/time-shifts #{:time-shift/night}
           :offerable/var []
           :offerable/effects
           [[:effect.direction/from-citizen :resource/time 1]
            [:effect.direction/to-citizen :citizen/physical-stress -0.1]
            [:effect.direction/to-citizen :citizen/mental-stress -0.05]]}
          {:offerable/id :offer/house.relax
           :offerable/label "Relaxing at Home"
           :offerable/capacity 2
           :offerable/time-shifts #{:time-shift/morning
                                    :time-shift/afternoon
                                    :time-shift/evening}
           :offerable/var []
           :offerable/effects
           [[:effect.direction/from-citizen :resource/time 1]
            [:effect.direction/to-citizen :citizen/physical-stress -0.05]
            [:effect.direction/to-citizen :citizen/mental-stress -0.1]]}]}

        {:blueprint/id :improvement.type/apartment
         :blueprint/label "Apartment"
         :blueprint/icon "🏢"
         :blueprint/description "Provides shelter"
         :blueprint/player-buildable? true
         :blueprint/price 50000
         :blueprint/offerables
         [{:offerable/id :offer/apartment.rental
           :offerable/label "Rental"
           :offerable/capacity 25
           :offerable/time-shifts #{:time-shift/morning
                                    :time-shift/afternoon
                                    :time-shift/evening
                                    :time-shift/night}
           :offerable/var [{:var/id :var/rent-rate
                            :var/label "Rent"
                            :var/unit [:/ :resource/money :resource/shelter]}]
           :offerable/effects
           [[:effect.direction/from-citizen :resource/money :var/rent-rate]
            [:effect.direction/to-citizen :resource/shelter 1]
            [:effect.direction/to-player :resource/money :var/rent-rate]]}
          {:offerable/id :offer/apartment.sleep
           :offerable/label "A Good Nights Sleep"
           :offerable/capacity 25
           :offerable/time-shifts #{:time-shift/night}
           :offerable/var []
           :offerable/effects
           [[:effect.direction/from-citizen :resource/time 1]
            [:effect.direction/to-citizen :citizen/physical-stress -0.1]
            [:effect.direction/to-citizen :citizen/mental-stress -0.05]]}
          {:offerable/id :offer/apartment.relax
           :offerable/label "Relaxing at Home"
           :offerable/capacity 25
           :offerable/time-shifts #{:time-shift/morning
                                    :time-shift/afternoon
                                    :time-shift/evening}
           :offerable/var []
           :offerable/effects
           [[:effect.direction/from-citizen :resource/time 1]
            [:effect.direction/to-citizen :citizen/physical-stress -0.05]
            [:effect.direction/to-citizen :citizen/mental-stress -0.1]]}]}


        {:blueprint/id :improvement.type/park
         :blueprint/label "Park"
         :blueprint/icon "🌳"
         :blueprint/description "A tranquil place for replenish the soul"
         :blueprint/player-buildable? true
         :blueprint/price 5000
         :blueprint/offerables
         [{:offerable/id :offer/park.leisure
           :offerable/label "Stroll"
           :offerable/capacity 10
           :offerable/time-shifts #{:time-shift/morning
                                    :time-shift/afternoon
                                    :time-shift/evening}
           :offerable/var []
           :offerable/effects
           [[:effect.direction/from-citizen :resource/time 1]
            [:effect.direction/to-citizen :citizen/physical-stress -0.05]
            [:effect.direction/to-citizen :citizen/mental-stress -0.05]]}]}

        {:blueprint/id :improvement.type/farm
         :blueprint/label "Farm"
         :blueprint/icon "🌽"
         :blueprint/description "Produces food"
         :blueprint/player-buildable? true
         :blueprint/price 5000
         :blueprint/offerables
         [{:offerable/id :offer/farm.job
           :offerable/label "Job"
           :offerable/capacity 2
           :offerable/time-shifts #{:time-shift/morning
                                    :time-shift/afternoon}
           :offerable/skill-productivity-weights {:citizen/skill.intellect 0.1
                                                  :citizen/skill.fitness 0.8
                                                  :citizen/skill.social 0.1}
           :offerable/var [{:var/id :var/job-rate
                            :var/label "Job Rate"
                            :var/unit [:/ :resource/money :resource/time]}]
           :offerable/effects
           [[:effect.direction/from-citizen :resource/time 1]
            [:effect.direction/to-citizen :resource/money :var/job-rate]
            [:effect.direction/from-player :resource/money :var/job-rate]
            [:effect.direction/to-player :resource/food 12]]}]}

        {:blueprint/id :improvement.type/food-market
         :blueprint/label "Food Market"
         :blueprint/icon "🛒"
         :blueprint/description "Players sell food to Citizens"
         :blueprint/player-buildable? true
         :blueprint/price 5000
         :blueprint/stocks [{:stock/resource :resource/labour}]
         :blueprint/offerables
         [{:offerable/id :offer/food-market.job
           :offerable/label "Job"
           :offerable/time-shifts #{:time-shift/morning
                                    :time-shift/afternoon
                                    :time-shift/evening}
           :offerable/skill-productivity-weights {:citizen/skill.intellect 0.4
                                                  :citizen/skill.fitness 0.1
                                                  :citizen/skill.social 0.5}
           :offerable/var [{:var/id :var/job-rate
                            :var/label "Job Rate"
                            :var/unit [:/ :resource/money :resource/time]}]
           :offerable/effects
           [[:effect.direction/from-citizen :resource/time 1]
            [:effect.direction/to-citizen :resource/money :var/job-rate]
            [:effect.direction/to-citizen :citizen/physical-stress 0.02]
            [:effect.direction/to-self :resource/labour 10]
            [:effect.direction/from-player :resource/money :var/job-rate]]}

          {:offerable/id :offer/food-market.offer
           :offerable/label "Selling Food"
           :offerable/time-shifts #{:time-shift/morning
                                    :time-shift/afternoon
                                    :time-shift/evening}
           :offerable/var [{:var/id :var/food-price
                            :var/label "Food Price"
                            :var/unit [:/ :resource/money :resource/food]}]
           :offerable/effects
           [[:effect.direction/from-citizen :resource/money :var/food-price]
            [:effect.direction/to-citizen :resource/food 1]
            [:effect.direction/from-self :resource/labour 0.5]
            [:effect.direction/from-player :resource/food 1]
            [:effect.direction/to-player :resource/money :var/food-price]]}]}

        {:blueprint/id :improvement.type/big-farm
         :blueprint/label "Big Farm"
         :blueprint/icon "🚜"
         :blueprint/description "Produces food"
         :blueprint/player-buildable? true
         :blueprint/price 50000
         :blueprint/offerables
         [{:offerable/id :offer/big-farm.job
           :offerable/label "Job"
           :offerable/capacity 10
           :offerable/time-shifts #{:time-shift/morning
                                    :time-shift/afternoon
                                    :time-shift/evening}
           :offerable/skill-productivity-weights {:citizen/skill.intellect 0.4
                                                  :citizen/skill.fitness 0.5
                                                  :citizen/skill.social 0.1}
           :offerable/var [{:var/id :var/job-rate
                            :var/label "Job Rate"
                            :var/unit [:/ :resource/money :resource/time]}]
           :offerable/effects
           [[:effect.direction/from-citizen :resource/time 1]
            [:effect.direction/to-citizen :resource/money :var/job-rate]
            [:effect.direction/to-citizen :citizen/physical-stress 0.02]
            [:effect.direction/from-player :resource/money :var/job-rate]
            [:effect.direction/to-player :resource/food 20]]}]}

        {:blueprint/id :improvement.type/monument
         :blueprint/label "Monument"
         :blueprint/icon "🗿"
         :blueprint/description "It's not good for anything, but looks cool I guess?"
         :blueprint/player-buildable? true
         :blueprint/price 500000
         :blueprint/offerables []}
        ]
       (types/key-by :blueprint/id)))

(malli/assert [:map-of :keyword types/Blueprint] blueprints)
#_(malli.error/humanize (malli/explain [:map-of :keyword types/Blueprint] blueprints))

(def offerables
  (->> blueprints
       vals
       (mapcat :blueprint/offerables)
       (types/key-by :offerable/id)))

(defn offer-active?
  "Var-less offers are always active; offers with vars are active once an amount is set."
  [offer]
  (or (some? (:offer/amount offer))
      (empty? (:offerable/var (offerables (:offer/type offer))))))

(def citizen-attributes
  {:citizen/physical-stress {:citizen-attribute/icon "😰"
                         :citizen-attribute/label "physical stress"}
   :citizen/mental-stress {:citizen-attribute/icon "🤯"
                       :citizen-attribute/label "mental stress"}
   :citizen/skill.intellect {:citizen-attribute/icon "🧠"
                         :citizen-attribute/label "intellect"}
   :citizen/skill.fitness {:citizen-attribute/icon "💪"
                       :citizen-attribute/label "fitness"}
   :citizen/skill.social {:citizen-attribute/icon "🗣️"
                      :citizen-attribute/label "social"}})

(defn resolve-effect-amount
  [offer [_direction _target amount]]
  (if (keyword? amount)
    ;; offerables currently have at most one var, so the offer's amount is its value
    (or (:offer/amount offer) 0)
    amount))

(defn effect-sum
  [offer direction target]
  (->> (:offerable/effects (offerables (:offer/type offer)))
       (filter (fn [[effect-direction effect-target _]]
                 (and (= direction effect-direction)
                      (= target effect-target))))
       (map (fn [effect]
              (resolve-effect-amount offer effect)))
       (reduce + 0)))

(defn offer-category
  [offer]
  (let [offerable (offerables (:offer/type offer))
        direction-targets (->> (:offerable/effects offerable)
                               (map (fn [[direction target _amount]]
                                      [direction target]))
                               set)]
    (cond
      (contains? direction-targets [:effect.direction/to-citizen :resource/food])
      :offer.category/food-sale
      (contains? direction-targets [:effect.direction/to-citizen :resource/shelter])
      :offer.category/housing
      (contains? direction-targets [:effect.direction/from-citizen :resource/time])
      :offer.category/time
      :else
      :offer.category/other)))

(defn offer-exchange-resource
  "The non-money resource an offer trades (food for a farm job, shelter for a rental, ...)"
  [offerable]
  (->> (:offerable/effects offerable)
       (keep (fn [[direction target _amount]]
               (when (and (contains? #{:effect.direction/from-player
                                       :effect.direction/to-player
                                       :effect.direction/to-citizen} direction)
                          (contains? types/resources target)
                          (not= :resource/money target))
                 target)))
       first))
