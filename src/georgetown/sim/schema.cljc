(ns georgetown.sim.schema
  (:require
    [dat.malli :as dat-malli]
    [malli.registry :as malli-registry]
    [georgetown.sim.blueprints :as blueprints]
    [georgetown.sim.time :as time]
    [georgetown.sim.types :as types]
    [georgetown.sim.util.math :as math]))

(def Email
  [:re #"^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\.[a-zA-Z]{2,}$"])

;; https://docs.datomic.com/schema/schema-reference.html
(def schema
  {:entity/island
   {:island/id {:dat/type :db.type/uuid
                :dat/unique :dat.unique/identity}
    :island/seed {:dat/type :db.type/long
                  :dat/spec types/PosInt}
    :island/public-stats {}
    :island/government-money-balance {:dat/type :db.type/long
                                      :dat/spec types/PosInt}
    :island/joy {:dat/type :db.type/long
                 :dat/spec types/PosInt}
    :island/players {:dat/rel [:dat.rel/many :entity/player :player/id]
                     :dat/component? true}
    :island/citizens {:dat/rel [:dat.rel/many :entity/citizen :citizen/id]
                  :dat/component? true}
    :island/lots {:dat/rel [:dat.rel/many :entity/lot :lot/id]
                  :dat/component? true}
    ;; number of ticks since start
    ;; 4 ticks per day
    :island/epoch {:dat/type :db.type/long
                   :dat/spec types/PosInt}}

   :entity/user
   {:user/id {:dat/type :db.type/uuid
              :dat/unique :dat.unique/identity}
    :user/email {:dat/type :db.type/string
                 :dat/spec Email
                 :dat/unique :dat.unique/identity}
    :user/players {:dat/rel [:dat.rel/many :entity/player :player/id]
                   :dat/component? true}}

   :entity/player ;; users on an island
   {:player/id {:dat/type :db.type/uuid
                :dat/unique :dat.unique/identity}
    :player/private-stats {}
    :player/money-balance {:dat/type :db.type/long
                           :dat/spec types/PosInt}
    :player/stocks {:dat/rel [:dat.rel/many :entity/stock :stock/id]
                    :dat/component? true}
    :player/deeds {:dat/rel [:dat.rel/many :entity/deed :deed/id]
                   :dat/component? true}
    :player/loans {:dat/rel [:dat.rel/many :entity/loan :loan/id]
                   :dat/component? true}}

   :entity/citizen
   (-> {:citizen/id {:dat/type :db.type/uuid
                 :dat/unique :dat.unique/identity}
        :citizen/savings {:dat/type :db.type/float
                      :dat/spec [:double {:min 0}]
                      ::generator-immigrant (fn []
                                              (* 1200.0 (math/beta 5 5)))
                      ::generator-baby (fn [] 0.0)}
        :citizen/age-ticks {:dat/type :db.type/long
                        :dat/spec [:int {:min 0}]
                        ::generator-immigrant (fn []
                                                (int (* 100 time/ticks-per-year (math/beta 20 50))))
                        ::generator-baby (fn [] 0)}}
       (into (for [k [;; citizens have different 'preferences' with regards to how they can spend their time
                      ;; preferences range from 0.0 to 1.0, and start around 0.5
                      ;; preferences are set at birth/immigration
                      :citizen/preference.security
                      :citizen/preference.self-improvement
                      :citizen/preference.physical-stress
                      :citizen/preference.mental-stress
                      :citizen/preference.spiritual-activity
                      :citizen/preference.social-activity
                      :citizen/preference.physical-activity
                      :citizen/preference.intellectual-activity
                      ;; citizens have 'talents', which affect the rate which skills improve
                      ;; talents range from 0.0 to 1.0, and start around 0.5
                      ;; talents are set at birth/immigration
                      :citizen/talent.intellect
                      :citizen/talent.fitness
                      :citizen/talent.social]]
               [k {:dat/type :db.type/float
                   :dat/spec [:double {:min 0 :max 1}]
                   ::generator-immigrant (fn [] (math/beta 5 5))
                   ::generator-baby (fn [] (math/beta 5 5))}]))
       (into (for [k [;; citizens have 'skills', which affect productivity, and improve when practised
                      :citizen/skill.intellect
                      :citizen/skill.fitness
                      :citizen/skill.social
                      ;; citizens have stress, which is affected by work, leisure, lack of food and shelter
                      :citizen/physical-stress
                      :citizen/mental-stress]]
               [k {:dat/type :db.type/float
                   :dat/spec [:double {:min 0 :max 1}]
                   ::generator-immigrant (fn [] (math/beta 5 5))
                   ::generator-baby (fn [] 0.1)}])))

   :entity/loan
   {:loan/id {:dat/type :db.type/uuid
              :dat/unique :dat.unique/identity}
    :loan/amount {:dat/type :db.type/long
                  :dat/spec types/PosInt}
    :loan/annual-interest-rate {:dat/type :db.type/float} ;; positive, typically between 0 and 0.3
    :loan/minimum-daily-payment-amount {:dat/type :db.type/long
                                        :dat/spec types/PosInt}
    :loan/daily-payment-amount {:dat/type :db.type/long
                                :dat/spec types/PosInt}}

   :entity/lot
   {:lot/id {:dat/type :db.type/uuid
             :dat/unique :dat.unique/identity}
    :lot/x {:dat/type :db.type/long
            :dat/spec types/PosInt}
    :lot/y {:dat/type :db.type/long
            :dat/spec types/PosInt}
    :lot/deed {:dat/rel [:dat.rel/one :entity/deed :deed/id]}
    :lot/improvement {:dat/rel [:dat.rel/one :entity/improvement :improvement/id]}
    :lot/elevation {:dat/type :db.type/float
                    :dat/spec [:double {:min 0 :max 1}]}
    :lot/moisture {:dat/type :db.type/float
                   :dat/spec [:double {:min 0 :max 1}]}}

   :entity/deed
   {:deed/id {:dat/type :db.type/uuid
              :dat/unique :dat.unique/identity}
    :deed/rate {:dat/type :db.type/long
                :dat/spec :whole-int}
    :deed/rate-changed-at {:dat/type :db.type/long
                           :dat/spec types/PosInt}}

   :entity/improvement
   {:improvement/id {:dat/type :db.type/uuid
                     :dat/unique :dat.unique/identity}
    :improvement/type {:dat/type :db.type/keyword
                       :dat/spec (into [:enum] (keys blueprints/blueprints))}
    :improvement/offers {:dat/rel [:dat.rel/many :entity/offer :offer/id]
                         :dat/component? true}
    :improvement/stocks {:dat/rel [:dat.rel/many :entity/stock :stock/id]
                         :dat/component? true}}

   :entity/stock
   {:stock/id {:dat/type :db.type/uuid
               :dat/unique :dat.unique/identity}
    :stock/resource {:dat/type :db.type/keyword
                     :dat/spec types/Resource}
    :stock/amount {:dat/type :db.type/float
                   :dat/spec [:double {:min 0}]}}

   :entity/offer
   {:offer/id {:dat/type :db.type/uuid
               :dat/unique :dat.unique/identity}
    :offer/type {:dat/type :db.type/keyword
                 :dat/spec (into [:enum] (keys blueprints/offerables))}
    :offer/amount {:dat/type :db.type/long
                   :dat/spec types/PosInt}
    :offer/utilization {:dat/type :db.type/float
                        :dat/spec [:double {:min 0 :max 1}]}
    }})

(malli-registry/set-default-registry!
  (dat-malli/->malli-registry schema))
