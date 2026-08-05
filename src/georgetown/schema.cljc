(ns georgetown.schema
  (:require
   [malli.registry :as mr]
   [dat.malli :as dm]
   [georgetown.math :as math]))

(defn key-by [f coll]
  (into {} (map (juxt f identity) coll)))

(def resources
  (->> [{:resource/id :resource/citizen
         :resource/icon "👤"
         :resource/label "citizen"
         :resource/unit-label "citizen"
         :resource/description "The island's citizens"}
        {:resource/id :resource/joy
         :resource/icon "😀"
         :resource/label "joy"
         :resource/unit-label "joy"
         :resource/description "The island's citizens' happiness"}
        {:resource/id :resource/time
         :resource/icon "⏱️"
         :resource/label "time"
         :resource/unit-label "day"
         :resource/description "A day; 1 second in the real world is 1 day in game"}

        {:resource/id :resource/food
         :resource/icon "🥕"
         :resource/label "food"
         :resource/unit-label "day"
         :resource/description "1 days worth of food for 1 person"}
        {:resource/id :resource/shelter
         :resource/icon "🛌"
         :resource/label "shelter"
         :resource/unit-label "night"
         :resource/description "1 day of shelter for 1 person"}
        {:resource/id :resource/money
         :resource/icon "💰"
         :resource/label "money"
         :resource/unit-label "dollar"
         :resource/description "currency, exchanged for other goods"}
        {:resource/id :resource/labour
         :resource/icon "👷"
         :resource/label "labour"
         :resource/unit-label "hour"
         :resource/description "1 hour of work by 1 person"}
         ]
       (key-by :resource/id)))

(def blueprints
  (->> [{:blueprint/id :improvement.type/house
         :blueprint/label "House"
         :blueprint/icon "🏠"
         :blueprint/description "Provides shelter"
         :blueprint/price 5000
         :blueprint/io
         [{:io/direction :io.direction/output
           :io/resource :resource/shelter
           :io/amount 2}]
         :blueprint/offerables
         [{:offerable/id :offer/house.rental
           :offerable/label "Rental"
           :offerable/supply-unit :resource/shelter
           :offerable/supply-amount 2
           :offerable/demand-unit :resource/money
           :offerable/demand-amount nil ; :user-value
           }]}

        {:blueprint/id :improvement.type/apartment
         :blueprint/label "Apartment"
         :blueprint/icon "🏢"
         :blueprint/description "Provides shelter"
         :blueprint/price 50000
         :blueprint/io
         [{:io/direction :io.direction/output
           :io/resource :resource/shelter
           :io/amount 25}]
         :blueprint/offerables
         [{:offerable/id :offer/apartment.rental
           :offerable/label "Rental"
           :offerable/supply-unit :resource/shelter
           :offerable/supply-amount 25
           :offerable/demand-unit :resource/money
           :offerable/demand-amount nil ; :user-value
           }]}

        {:blueprint/id :improvement.type/farm
         :blueprint/label "Farm"
         :blueprint/icon "🌽"
         :blueprint/description "Produces food"
         :blueprint/price 5000
         :blueprint/io
         [{:io/direction :io.direction/output
           :io/resource :resource/food
           :io/amount 3}
          {:io/direction :io.direction/input
           :io/resource :resource/labour
           :io/amount 20}]
         :blueprint/offerables
         [{:offerable/id :offer/farm.food
           :offerable/label "Food"
           :offerable/supply-unit :resource/food
           :offerable/supply-amount 3
           :offerable/demand-unit :resource/money
           :offerable/demand-amount nil ; user value
           }
          {:offerable/id :offer/farm.job
           :offerable/label "Job"
           :offerable/invert? true
           :offerable/supply-unit :resource/money
           :offerable/supply-amount nil ; user value
           :offerable/demand-unit :resource/labour
           :offerable/demand-amount 20
           :offerable/prerequisite? true}]}

        {:blueprint/id :improvement.type/big-farm
         :blueprint/label "Big Farm"
         :blueprint/icon "🚜"
         :blueprint/description "Produces food"
         :blueprint/price 50000
         :blueprint/io
         [{:io/direction :io.direction/output
           :io/resource :resource/food
           :io/amount 30}
          {:io/direction :io.direction/input
           :io/resource :resource/labour
           :io/amount 150}]
         :blueprint/offerables
         [{:offerable/id :offer/big-farm.food
           :offerable/label "Food"
           :offerable/supply-unit :resource/food
           :offerable/supply-amount 50
           :offerable/demand-unit :resource/money
           :offerable/demand-amount nil ; user value
           }
          {:offerable/id :offer/big-farm.job
           :offerable/label "Job"
           :offerable/invert? true
           :offerable/supply-unit :resource/money
           :offerable/supply-amount nil ; user value
           :offerable/demand-unit :resource/labour
           :offerable/demand-amount 150
           :offerable/prerequisite? true}]}

        {:blueprint/id :improvement.type/monument
         :blueprint/label "Monument"
         :blueprint/icon "🗿"
         :blueprint/description "It's not good for anything, but looks cool I guess?"
         :blueprint/price 500000
         :blueprint/io []
         :blueprint/offerables []}
        ]
       (key-by :blueprint/id)))

(def offerables
  (->> blueprints
       vals
       (mapcat :blueprint/offerables)
       (key-by :offerable/id)))

(def Email
  [:re #"^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\.[a-zA-Z]{2,}$"])

;; https://docs.datomic.com/schema/schema-reference.html
(def schema
  {:entity/island
   {:island/id {:dat/type :db.type/uuid
                :dat/unique :dat.unique/identity}
    :island/seed {:dat/type :db.type/long
                  :dat/spec :pos-int}
    :island/public-stats {}
    :island/population {:dat/type :db.type/long
                        :dat/spec :pos-int}
    :island/government-money-balance {:dat/type :db.type/long
                                      :dat/spec :pos-int}
    :island/citizen-money-balance {:dat/type :db.type/long
                                   :dat/spec :pos-int}
    :island/citizen-food-balance {:dat/type :db.type/long
                                  :dat/spec :pos-int}
    :island/joy {:dat/type :db.type/long
                 :dat/spec :pos-int}
    :island/residents {:dat/rel [:dat.rel/many :entity/resident :resident/id]
                       :dat/component? true}
    :island/sims {:dat/rel [:dat.rel/many :entity/sim :sim/id]
                  :dat/component? true}
    :island/lots {:dat/rel [:dat.rel/many :entity/lot :lot/id]
                  :dat/component? true}
    :island/epoch {:dat/type :db.type/long
                   :dat/spec :pos-int}}

   :entity/user
   {:user/id {:dat/type :db.type/uuid
              :dat/unique :dat.unique/identity}
    :user/email {:dat/type :db.type/string
                 :dat/spec Email
                 :dat/unique :dat.unique/identity}
    :user/residents {:dat/rel [:dat.rel/many :entity/resident :resident/id]
                     :dat/component? true}}

   :entity/resident ;; users on an island
   {:resident/id {:dat/type :db.type/uuid
                  :dat/unique :dat.unique/identity}
    :resident/private-stats {}
    :resident/money-balance {:dat/type :db.type/long
                             :dat/spec :pos-int}
    :resident/deeds {:dat/rel [:dat.rel/many :entity/deed :deed/id]
                     :dat/component? true}
    :resident/loans {:dat/rel [:dat.rel/many :entity/loan :loan/id]
                     :dat/component? true}}

   :entity/sim
   (-> {:sim/id {:dat/type :db.type/uuid
                 :dat/unique :dat.unique/identity}
        :sim/savings {:dat/type :db.type/long
                      :dat/spec [:int {:min 0}]
                      ::generator-immigrant (fn []
                                              (int (* 10000 (math/beta 4 4))))
                      ::generator-baby (fn [] 0)}
        :sim/age {:dat/type :db.type/long
                  :dat/spec [:int {:min 0}]
                  ::generator-immigrant (fn []
                                          (int (* 100 (math/beta 20 50))))
                  ::generator-baby (fn [] 0)}}
       (into (for [k [:sim/preference.security
                      :sim/preference.self-improvement
                      :sim/preference.physical-stress
                      :sim/preference.mental-stress
                      :sim/preference.spiritual-activity
                      :sim/preference.social-activity
                      :sim/preference.physical-activity
                      :sim/preference.intellectual-activity
                      :sim/talent.intellect
                      :sim/talent.fitness
                      :sim/talent.social]]
               [k {:dat/type :db.type/float
                   :dat/spec [:float {:min 0 :max 1}]
                   ::generator-immigrant (fn [] (math/beta 4 4))
                   ::generator-baby (fn [] (math/beta 4 4))}]))
       (into (for [k [:sim/skill.intellect
                      :sim/skill.fitness
                      :sim/skill.social
                      :sim/physical-stress
                      :sim/mental-stress]]
               [k {:dat/type :db.type/float
                   :dat/spec [:float {:min 0 :max 1}]
                   ::generator-immigrant (fn [] (math/beta 4 4))
                   ::generator-baby (fn [] 0.1)}])))

   :entity/loan
   {:loan/id {:dat/type :db.type/uuid
              :dat/unique :dat.unique/identity}
    :loan/amount {:dat/type :db.type/long
                  :dat/spec :pos-int}
    :loan/annual-interest-rate {:dat/type :db.type/float} ;; positive, typically between 0 and 0.3
    :loan/minimum-daily-payment-amount {:dat/type :db.type/long
                                        :dat/spec :pos-int}
    :loan/daily-payment-amount {:dat/type :db.type/long
                                :dat/spec :pos-int}}

   :entity/lot
   {:lot/id {:dat/type :db.type/uuid
             :dat/unique :dat.unique/identity}
    :lot/x {:dat/type :db.type/long
            :dat/spec :pos-int}
    :lot/y {:dat/type :db.type/long
            :dat/spec :pos-int}
    :lot/deed {:dat/rel [:dat.rel/one :entity/deed :deed/id]}
    :lot/improvement {:dat/rel [:dat.rel/one :entity/improvement :improvement/id]}
    :lot/elevation {:dat/type :db.type/float
                    :dat/spec [:float {:min 0 :max 1}]}
    :lot/moisture {:dat/type :db.type/float
                   :dat/spec [:float {:min 0 :max 1}]}}

   :entity/deed
   {:deed/id {:dat/type :db.type/uuid
              :dat/unique :dat.unique/identity}
    :deed/rate {:dat/type :db.type/long
                :dat/spec :whole-int}
    :deed/rate-changed-at {:dat/type :db.type/long
                           :dat/spec :pos-int}}

   :entity/improvement
   {:improvement/id {:dat/type :db.type/uuid
                     :dat/unique :dat.unique/identity}
    :improvement/type {:dat/type :db.type/keyword
                       :dat/spec (into [:enum] (keys blueprints))}
    :improvement/offers {:dat/rel [:dat.rel/many :entity/offer :offer/id]
                         :dat/component? true}}

   :entity/offer
   {:offer/id {:dat/type :db.type/uuid
               :dat/unique :dat.unique/identity}
    :offer/type {:dat/type :db.type/keyword
                 :dat/spec (into [:enum] (keys offerables))}
    :offer/amount {:dat/type :db.type/long
                   :dat/spec :pos-int}
    :offer/utilization {:dat/type :db.type/float
                        :dat/spec [:float {:min 0 :max 1}]}
    }})

(mr/set-default-registry!
  (dm/->malli-registry schema))
