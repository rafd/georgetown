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
           :offerable/icon "🔑"
           :offerable/activity-weights {:citizen/preference.spiritual-activity 0.0
                                        :citizen/preference.social-activity 0.0
                                        :citizen/preference.physical-activity 0.0
                                        :citizen/preference.intellectual-activity 0.0}
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
           :offerable/icon "😴"
           :offerable/activity-weights {:citizen/preference.spiritual-activity 0.0
                                        :citizen/preference.social-activity 0.0
                                        :citizen/preference.physical-activity 0.0
                                        :citizen/preference.intellectual-activity 0.0}
           :offerable/capacity 2
           :offerable/time-shifts #{:time-shift/night}
           :offerable/var []
           :offerable/effects
           [[:effect.direction/from-citizen :resource/time 1]
            [:effect.direction/to-citizen :citizen/physical-stress -0.1]
            [:effect.direction/to-citizen :citizen/mental-stress -0.05]]}
          {:offerable/id :offer/house.relax
           :offerable/label "Relaxing at Home"
           :offerable/icon "🛋️"
           :offerable/activity-weights {:citizen/preference.spiritual-activity 0.1
                                        :citizen/preference.social-activity 0.1
                                        :citizen/preference.physical-activity 0.0
                                        :citizen/preference.intellectual-activity 0.1}
           :offerable/capacity 2
           :offerable/time-shifts #{:time-shift/morning
                                    :time-shift/afternoon
                                    :time-shift/evening}
           :offerable/var []
           :offerable/effects
           [[:effect.direction/from-citizen :resource/time 1]
            [:effect.direction/to-citizen :citizen/physical-stress -0.01]
            [:effect.direction/to-citizen :citizen/mental-stress -0.01]]}]}

        {:blueprint/id :improvement.type/apartment
         :blueprint/label "Apartment"
         :blueprint/icon "🏢"
         :blueprint/description "Provides shelter"
         :blueprint/player-buildable? true
         :blueprint/price 50000
         :blueprint/offerables
         [{:offerable/id :offer/apartment.rental
           :offerable/label "Rental"
           :offerable/icon "🔑"
           :offerable/activity-weights {:citizen/preference.spiritual-activity 0.0
                                        :citizen/preference.social-activity 0.0
                                        :citizen/preference.physical-activity 0.0
                                        :citizen/preference.intellectual-activity 0.0}
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
           :offerable/icon "😴"
           :offerable/activity-weights {:citizen/preference.spiritual-activity 0.0
                                        :citizen/preference.social-activity 0.0
                                        :citizen/preference.physical-activity 0.0
                                        :citizen/preference.intellectual-activity 0.0}
           :offerable/capacity 25
           :offerable/time-shifts #{:time-shift/night}
           :offerable/var []
           :offerable/effects
           [[:effect.direction/from-citizen :resource/time 1]
            [:effect.direction/to-citizen :citizen/physical-stress -0.1]
            [:effect.direction/to-citizen :citizen/mental-stress -0.05]]}
          {:offerable/id :offer/apartment.relax
           :offerable/label "Relaxing at Home"
           :offerable/icon "🛋️"
           :offerable/activity-weights {:citizen/preference.spiritual-activity 0.1
                                        :citizen/preference.social-activity 0.1
                                        :citizen/preference.physical-activity 0.0
                                        :citizen/preference.intellectual-activity 0.1}
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
           :offerable/icon "🚶"
           :offerable/activity-weights {:citizen/preference.spiritual-activity 0.5
                                        :citizen/preference.social-activity 0.1
                                        :citizen/preference.physical-activity 0.3
                                        :citizen/preference.intellectual-activity 0.1}
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
           :offerable/icon "💼"
           :offerable/activity-weights {:citizen/preference.spiritual-activity 0.1
                                        :citizen/preference.social-activity 0.1
                                        :citizen/preference.physical-activity 0.7
                                        :citizen/preference.intellectual-activity 0.1}
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
            [:effect.direction/to-player :resource/food 6]]}]}

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
           :offerable/icon "💼"
           :offerable/activity-weights {:citizen/preference.spiritual-activity 0.0
                                        :citizen/preference.social-activity 0.5
                                        :citizen/preference.physical-activity 0.1
                                        :citizen/preference.intellectual-activity 0.4}
           :offerable/capacity 1
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
            [:effect.direction/to-self :resource/labour 1]
            [:effect.direction/from-player :resource/money :var/job-rate]]}

          {:offerable/id :offer/food-market.offer
           :offerable/label "Selling Food"
           :offerable/icon "🍎"
           :offerable/activity-weights {:citizen/preference.spiritual-activity 0.0
                                        :citizen/preference.social-activity 0.0
                                        :citizen/preference.physical-activity 0.0
                                        :citizen/preference.intellectual-activity 0.0}
           :offerable/capacity 25
           :offerable/time-shifts #{:time-shift/morning
                                    :time-shift/afternoon
                                    :time-shift/evening}
           :offerable/var [{:var/id :var/food-price
                            :var/label "Food Price"
                            :var/unit [:/ :resource/money :resource/food]}]
           :offerable/effects
           [[:effect.direction/from-citizen :resource/money :var/food-price]
            [:effect.direction/to-citizen :resource/food 1]
            [:effect.direction/from-self :resource/labour 0.05]
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
           :offerable/icon "💼"
           :offerable/activity-weights {:citizen/preference.spiritual-activity 0.0
                                        :citizen/preference.social-activity 0.1
                                        :citizen/preference.physical-activity 0.5
                                        :citizen/preference.intellectual-activity 0.4}
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

        {:blueprint/id :improvement.type/movie-theatre
         :blueprint/label "Movie Theatre"
         :blueprint/icon "🎬"
         :blueprint/description "Citizens watch movies to unwind"
         :blueprint/player-buildable? true
         :blueprint/price 20000
         :blueprint/stocks [{:stock/resource :resource/labour}]
         :blueprint/offerables
         [{:offerable/id :offer/movie-theatre.job
           :offerable/label "Job"
           :offerable/icon "💼"
           :offerable/activity-weights {:citizen/preference.spiritual-activity 0.0
                                        :citizen/preference.social-activity 0.6
                                        :citizen/preference.physical-activity 0.1
                                        :citizen/preference.intellectual-activity 0.3}
           :offerable/capacity 2
           :offerable/time-shifts #{:time-shift/afternoon
                                    :time-shift/evening}
           :offerable/skill-productivity-weights {:citizen/skill.intellect 0.3
                                                  :citizen/skill.fitness 0.1
                                                  :citizen/skill.social 0.6}
           :offerable/var [{:var/id :var/job-rate
                            :var/label "Job Rate"
                            :var/unit [:/ :resource/money :resource/time]}]
           :offerable/effects
           [[:effect.direction/from-citizen :resource/time 1]
            [:effect.direction/to-citizen :resource/money :var/job-rate]
            [:effect.direction/to-citizen :citizen/physical-stress 0.01]
            [:effect.direction/to-self :resource/labour 1]
            [:effect.direction/from-player :resource/money :var/job-rate]]}
          {:offerable/id :offer/movie-theatre.screening
           :offerable/label "Movie Screening"
           :offerable/icon "🍿"
           :offerable/activity-weights {:citizen/preference.spiritual-activity 0.2
                                        :citizen/preference.social-activity 0.3
                                        :citizen/preference.physical-activity 0.0
                                        :citizen/preference.intellectual-activity 0.4}
           :offerable/capacity 30
           :offerable/time-shifts #{:time-shift/afternoon
                                    :time-shift/evening}
           :offerable/var [{:var/id :var/ticket-price
                            :var/label "Ticket Price"
                            :var/unit [:/ :resource/money :resource/time]}]
           :offerable/effects
           [[:effect.direction/from-citizen :resource/time 1]
            [:effect.direction/from-citizen :resource/money :var/ticket-price]
            [:effect.direction/to-citizen :citizen/mental-stress -0.15]
            [:effect.direction/from-self :resource/labour 0.1]
            [:effect.direction/to-player :resource/money :var/ticket-price]]}]}

        {:blueprint/id :improvement.type/gym
         :blueprint/label "Gym"
         :blueprint/icon "🏋️"
         :blueprint/description "Citizens work out to build fitness"
         :blueprint/player-buildable? true
         :blueprint/price 10000
         :blueprint/stocks [{:stock/resource :resource/labour}]
         :blueprint/offerables
         [{:offerable/id :offer/gym.job
           :offerable/label "Job"
           :offerable/icon "💼"
           :offerable/activity-weights {:citizen/preference.spiritual-activity 0.0
                                        :citizen/preference.social-activity 0.2
                                        :citizen/preference.physical-activity 0.7
                                        :citizen/preference.intellectual-activity 0.1}
           :offerable/capacity 1
           :offerable/time-shifts #{:time-shift/morning
                                    :time-shift/afternoon
                                    :time-shift/evening}
           :offerable/skill-productivity-weights {:citizen/skill.intellect 0.1
                                                  :citizen/skill.fitness 0.7
                                                  :citizen/skill.social 0.2}
           :offerable/var [{:var/id :var/job-rate
                            :var/label "Job Rate"
                            :var/unit [:/ :resource/money :resource/time]}]
           :offerable/effects
           [[:effect.direction/from-citizen :resource/time 1]
            [:effect.direction/to-citizen :resource/money :var/job-rate]
            [:effect.direction/to-citizen :citizen/physical-stress 0.02]
            [:effect.direction/to-self :resource/labour 1]
            [:effect.direction/from-player :resource/money :var/job-rate]]}
          {:offerable/id :offer/gym.workout
           :offerable/label "Workout"
           :offerable/icon "💪"
           :offerable/activity-weights {:citizen/preference.spiritual-activity 0.1
                                        :citizen/preference.social-activity 0.2
                                        :citizen/preference.physical-activity 0.8
                                        :citizen/preference.intellectual-activity 0.0}
           :offerable/capacity 15
           :offerable/time-shifts #{:time-shift/morning
                                    :time-shift/afternoon
                                    :time-shift/evening}
           :offerable/var [{:var/id :var/entry-fee
                            :var/label "Entry Fee"
                            :var/unit [:/ :resource/money :resource/time]}]
           :offerable/effects
           [[:effect.direction/from-citizen :resource/time 1]
            [:effect.direction/from-citizen :resource/money :var/entry-fee]
            [:effect.direction/to-citizen :citizen/skill.fitness 0.01]
            [:effect.direction/to-citizen :citizen/mental-stress -0.05]
            [:effect.direction/to-citizen :citizen/physical-stress 0.02]
            [:effect.direction/from-self :resource/labour 0.05]
            [:effect.direction/to-player :resource/money :var/entry-fee]]}]}

        {:blueprint/id :improvement.type/library
         :blueprint/label "Library"
         :blueprint/icon "📚"
         :blueprint/description "A quiet place to read and learn"
         :blueprint/player-buildable? true
         :blueprint/price 15000
         :blueprint/offerables
         [{:offerable/id :offer/library.reading
           :offerable/label "Reading"
           :offerable/icon "📖"
           :offerable/activity-weights {:citizen/preference.spiritual-activity 0.2
                                        :citizen/preference.social-activity 0.0
                                        :citizen/preference.physical-activity 0.0
                                        :citizen/preference.intellectual-activity 0.8}
           :offerable/capacity 20
           :offerable/time-shifts #{:time-shift/morning
                                    :time-shift/afternoon
                                    :time-shift/evening}
           :offerable/var []
           :offerable/effects
           [[:effect.direction/from-citizen :resource/time 1]
            [:effect.direction/to-citizen :citizen/skill.intellect 0.01]
            [:effect.direction/to-citizen :citizen/mental-stress -0.05]]}]}

        {:blueprint/id :improvement.type/pub
         :blueprint/label "Pub"
         :blueprint/icon "🍺"
         :blueprint/description "Citizens socialize over a pint"
         :blueprint/player-buildable? true
         :blueprint/price 15000
         :blueprint/stocks [{:stock/resource :resource/labour}]
         :blueprint/offerables
         [{:offerable/id :offer/pub.job
           :offerable/label "Job"
           :offerable/icon "💼"
           :offerable/activity-weights {:citizen/preference.spiritual-activity 0.0
                                        :citizen/preference.social-activity 0.7
                                        :citizen/preference.physical-activity 0.2
                                        :citizen/preference.intellectual-activity 0.1}
           :offerable/capacity 2
           :offerable/time-shifts #{:time-shift/evening}
           :offerable/skill-productivity-weights {:citizen/skill.intellect 0.1
                                                  :citizen/skill.fitness 0.2
                                                  :citizen/skill.social 0.7}
           :offerable/var [{:var/id :var/job-rate
                            :var/label "Job Rate"
                            :var/unit [:/ :resource/money :resource/time]}]
           :offerable/effects
           [[:effect.direction/from-citizen :resource/time 1]
            [:effect.direction/to-citizen :resource/money :var/job-rate]
            [:effect.direction/to-citizen :citizen/physical-stress 0.02]
            [:effect.direction/to-self :resource/labour 1]
            [:effect.direction/from-player :resource/money :var/job-rate]]}
          {:offerable/id :offer/pub.night-out
           :offerable/label "A Night Out"
           :offerable/icon "🍻"
           :offerable/activity-weights {:citizen/preference.spiritual-activity 0.1
                                        :citizen/preference.social-activity 0.8
                                        :citizen/preference.physical-activity 0.1
                                        :citizen/preference.intellectual-activity 0.1}
           :offerable/capacity 20
           :offerable/time-shifts #{:time-shift/evening}
           :offerable/var [{:var/id :var/drink-price
                            :var/label "Drink Price"
                            :var/unit [:/ :resource/money :resource/time]}]
           :offerable/effects
           [[:effect.direction/from-citizen :resource/time 1]
            [:effect.direction/from-citizen :resource/money :var/drink-price]
            [:effect.direction/to-citizen :citizen/mental-stress -0.1]
            [:effect.direction/to-citizen :citizen/skill.social 0.01]
            [:effect.direction/to-citizen :citizen/physical-stress 0.02]
            [:effect.direction/from-self :resource/labour 0.1]
            [:effect.direction/to-player :resource/money :var/drink-price]]}]}

        {:blueprint/id :improvement.type/spa
         :blueprint/label "Spa"
         :blueprint/icon "🧖"
         :blueprint/description "Deep relaxation for body and mind"
         :blueprint/player-buildable? true
         :blueprint/price 30000
         :blueprint/stocks [{:stock/resource :resource/labour}]
         :blueprint/offerables
         [{:offerable/id :offer/spa.job
           :offerable/label "Job"
           :offerable/icon "💼"
           :offerable/activity-weights {:citizen/preference.spiritual-activity 0.1
                                        :citizen/preference.social-activity 0.4
                                        :citizen/preference.physical-activity 0.4
                                        :citizen/preference.intellectual-activity 0.1}
           :offerable/capacity 2
           :offerable/time-shifts #{:time-shift/morning
                                    :time-shift/afternoon
                                    :time-shift/evening}
           :offerable/skill-productivity-weights {:citizen/skill.intellect 0.2
                                                  :citizen/skill.fitness 0.4
                                                  :citizen/skill.social 0.4}
           :offerable/var [{:var/id :var/job-rate
                            :var/label "Job Rate"
                            :var/unit [:/ :resource/money :resource/time]}]
           :offerable/effects
           [[:effect.direction/from-citizen :resource/time 1]
            [:effect.direction/to-citizen :resource/money :var/job-rate]
            [:effect.direction/to-citizen :citizen/physical-stress 0.02]
            [:effect.direction/to-self :resource/labour 1]
            [:effect.direction/from-player :resource/money :var/job-rate]]}
          {:offerable/id :offer/spa.treatment
           :offerable/label "Spa Treatment"
           :offerable/icon "💆"
           :offerable/activity-weights {:citizen/preference.spiritual-activity 0.7
                                        :citizen/preference.social-activity 0.1
                                        :citizen/preference.physical-activity 0.3
                                        :citizen/preference.intellectual-activity 0.0}
           :offerable/capacity 8
           :offerable/time-shifts #{:time-shift/morning
                                    :time-shift/afternoon
                                    :time-shift/evening}
           :offerable/var [{:var/id :var/treatment-price
                            :var/label "Treatment Price"
                            :var/unit [:/ :resource/money :resource/time]}]
           :offerable/effects
           [[:effect.direction/from-citizen :resource/time 1]
            [:effect.direction/from-citizen :resource/money :var/treatment-price]
            [:effect.direction/to-citizen :citizen/physical-stress -0.2]
            [:effect.direction/to-citizen :citizen/mental-stress -0.1]
            [:effect.direction/from-self :resource/labour 0.25]
            [:effect.direction/to-player :resource/money :var/treatment-price]]}]}

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

(def offerable-id->blueprint
  (->> blueprints
       vals
       (mapcat (fn [blueprint]
                 (->> (:blueprint/offerables blueprint)
                      (map (fn [offerable]
                             [(:offerable/id offerable) blueprint])))))
       (into {})))

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

(def skill->talent
  {:citizen/skill.intellect :citizen/talent.intellect
   :citizen/skill.fitness :citizen/talent.fitness
   :citizen/skill.social :citizen/talent.social})

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
