(ns georgetown.schema
  (:require
   [malli.core :as m]
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

(def Resource
  (into [:enum] (keys resources)))

(def SimAttribute
  [:enum
   :sim/physical-stress
   :sim/mental-stress
   :sim/skill.intellect
   :sim/skill.fitness
   :sim/skill.social])

(def VarId [:qualified-keyword {:namespace :var}])

(def Blueprint
  [:map {:closed true}
   [:blueprint/id [:qualified-keyword {:namespace :improvement.type}]]
   [:blueprint/label :string]
   [:blueprint/icon :string]
   [:blueprint/description :string]
   [:blueprint/price :pos-int]
   [:blueprint/stocks {:optional true}
    [:vector
     [:map {:closed true}
      [:stock/resource Resource]]]]
   [:blueprint/offerables
    [:vector
     [:and
      [:map {:closed true}
       [:offerable/id [:qualified-keyword {:namespace :offer}]]
       [:offerable/label :string]
       [:offerable/capacity {:optional true} :pos-int]
       [:offerable/time-shifts [:set [:enum
                                      :time-shift/morning
                                      :time-shift/afternoon
                                      :time-shift/evening
                                      :time-shift/night]]]
       [:offerable/var
        [:vector
         [:map {:closed true}
          [:var/id VarId]
          [:var/label :string]
          [:var/unit [:tuple [:enum :/] Resource Resource]]]]]
       [:offerable/effects
        [:vector
         [:tuple
          [:enum
           :effect.direction/from-sim
           :effect.direction/to-sim
           :effect.direction/from-player
           :effect.direction/to-player
           :effect.direction/from-self
           :effect.direction/to-self]
          [:or Resource SimAttribute]
          [:or number? VarId]]]]]
      [:fn {:error/message "effect refers to a var not declared in :offerable/var"}
       (fn [{:offerable/keys [var effects]}]
         (let [declared-var-ids (set (map :var/id var))]
           (->> effects
                (map (fn [[_direction _target amount]] amount))
                (filter keyword?)
                (every? declared-var-ids))))]]]]])

(def blueprints
  (->> [{:blueprint/id :improvement.type/house
         :blueprint/label "House"
         :blueprint/icon "🏠"
         :blueprint/description "Provides shelter"
         :blueprint/price 5000
         :blueprint/offerables
         [{:offerable/id :offer/house.rental
           :offerable/label "Rental"
           :offerable/capacity 2
           :offerable/time-shifts #{:time-shift/night}
           :offerable/var [{:var/id :var/rent-rate
                            :var/label "Rent"
                            :var/unit [:/ :resource/money :resource/shelter]}]
           :offerable/effects
           [[:effect.direction/from-sim :resource/time 1]
            [:effect.direction/from-sim :resource/money :var/rent-rate]
            [:effect.direction/to-sim :resource/shelter 1]
            [:effect.direction/to-player :resource/money :var/rent-rate]]}]}

        {:blueprint/id :improvement.type/apartment
         :blueprint/label "Apartment"
         :blueprint/icon "🏢"
         :blueprint/description "Provides shelter"
         :blueprint/price 50000
         :blueprint/offerables
         [{:offerable/id :offer/apartment.rental
           :offerable/label "Rental"
           :offerable/capacity 25
           :offerable/time-shifts #{:time-shift/night}
           :offerable/var [{:var/id :var/rent-rate
                            :var/label "Rent"
                            :var/unit [:/ :resource/money :resource/shelter]}]
           :offerable/effects
           [[:effect.direction/from-sim :resource/time 1]
            [:effect.direction/from-sim :resource/money :var/rent-rate]
            [:effect.direction/to-sim :resource/shelter 1]
            [:effect.direction/to-sim :sim/mental-stress 0.05]
            [:effect.direction/to-player :resource/money :var/rent-rate]]}]}


        {:blueprint/id :improvement.type/park
         :blueprint/label "Park"
         :blueprint/icon "🌳"
         :blueprint/description "A tranquil place for replenish the soul"
         :blueprint/price 5000
         :blueprint/offerables
         [{:offerable/id :offer/park.leisure
           :offerable/label "Stroll"
           :offerable/time-shifts #{:time-shift/morning
                                    :time-shift/afternoon
                                    :time-shift/evening}
           :offerable/var []
           :offerable/effects
           [[:effect.direction/from-sim :resource/time 1]
            [:effect.direction/to-sim :sim/physical-stress -0.1]
            [:effect.direction/to-sim :sim/mental-stress -0.1]]}]}

        {:blueprint/id :improvement.type/farm
         :blueprint/label "Farm"
         :blueprint/icon "🌽"
         :blueprint/description "Produces food"
         :blueprint/price 5000
         :blueprint/offerables
         [{:offerable/id :offer/farm.job
           :offerable/label "Job"
           :offerable/time-shifts #{:time-shift/morning
                                    :time-shift/afternoon}
           :offerable/var [{:var/id :var/job-rate
                            :var/label "Job Rate"
                            :var/unit [:/ :resource/money :resource/time]}]
           :offerable/effects
           [[:effect.direction/from-sim :resource/time 1]
            [:effect.direction/to-sim :resource/money :var/job-rate]
            [:effect.direction/from-player :resource/money :var/job-rate]
            [:effect.direction/to-player :resource/food 1]]}]}

        {:blueprint/id :improvement.type/food-market
         :blueprint/label "Food Market"
         :blueprint/icon "🛒"
         :blueprint/description "Players sell food to Sims"
         :blueprint/price 5000
         :blueprint/stocks [{:stock/resource :resource/labour}]
         :blueprint/offerables
         [{:offerable/id :offer/food-market.job
           :offerable/label "Job"
           :offerable/time-shifts #{:time-shift/morning
                                    :time-shift/afternoon
                                    :time-shift/evening}
           :offerable/var [{:var/id :var/job-rate
                            :var/label "Job Rate"
                            :var/unit [:/ :resource/money :resource/time]}]
           :offerable/effects
           [[:effect.direction/from-sim :resource/time 1]
            [:effect.direction/to-sim :resource/money :var/job-rate]
            [:effect.direction/to-sim :sim/physical-stress 0.1]
            [:effect.direction/to-self :resource/labour 1]
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
           [[:effect.direction/from-sim :resource/money :var/food-price]
            [:effect.direction/to-sim :resource/food 1]
            [:effect.direction/from-self :resource/labour 1]
            [:effect.direction/from-player :resource/food 1]
            [:effect.direction/to-player :resource/money :var/food-price]]}]}

        {:blueprint/id :improvement.type/big-farm
         :blueprint/label "Big Farm"
         :blueprint/icon "🚜"
         :blueprint/description "Produces food"
         :blueprint/price 50000
         :blueprint/offerables
         [{:offerable/id :offer/big-farm.job
           :offerable/label "Job"
           :offerable/capacity 10
           :offerable/time-shifts #{:time-shift/morning
                                    :time-shift/afternoon
                                    :time-shift/evening}
           :offerable/var [{:var/id :var/job-rate
                            :var/label "Job Rate"
                            :var/unit [:/ :resource/money :resource/time]}]
           :offerable/effects
           [[:effect.direction/from-sim :resource/time 1]
            [:effect.direction/to-sim :resource/money :var/job-rate]
            [:effect.direction/to-sim :sim/physical-stress 0.1]
            [:effect.direction/from-player :resource/money :var/job-rate]
            [:effect.direction/to-player :resource/food 3]]}]}

        {:blueprint/id :improvement.type/monument
         :blueprint/label "Monument"
         :blueprint/icon "🗿"
         :blueprint/description "It's not good for anything, but looks cool I guess?"
         :blueprint/price 500000
         :blueprint/offerables []}
        ]
       (key-by :blueprint/id)))

(m/assert [:map-of :keyword Blueprint] blueprints)

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
