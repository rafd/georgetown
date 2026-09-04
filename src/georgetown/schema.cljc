(ns georgetown.schema
  (:require
   [malli.core :as m]
   [malli.registry :as mr]
   [dat.malli :as dm]
   [georgetown.math :as math]))

(defn key-by [f coll]
  (into {} (map (juxt f identity) coll)))

(def ticks-per-day 4)
(def ticks-per-year (* 365 ticks-per-day))

(defn ticks->years [ticks]
  (/ ticks ticks-per-year))

(defn age-in-years [sim]
  (ticks->years (:sim/age-ticks sim)))

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
         :resource/unit-label "shift"
         :resource/description "A shift; 4 shifts in a day (morning, afternoon, evening, night)"}

        {:resource/id :resource/food
         :resource/icon "🥕"
         :resource/label "food"
         :resource/unit-label "meal"
         :resource/description "1 meal for 1 person"}
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

(def PosInt pos-int?)

(def Blueprint
  [:map {:closed true}
   [:blueprint/id [:qualified-keyword {:namespace :improvement.type}]]
   [:blueprint/label :string]
   [:blueprint/icon :string]
   [:blueprint/description :string]
   [:blueprint/price PosInt]
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
       [:offerable/capacity {:optional true} PosInt]
       [:offerable/time-shifts [:set [:enum
                                      :time-shift/morning
                                      :time-shift/afternoon
                                      :time-shift/evening
                                      :time-shift/night]]]
       ;; jobs have weights per skill
       ;; when combined with the skills of a specific sim, they determine the productivity
       ;; also affect the rate at which sim skills are improved (along with sim talent for that skill)
       [:offerable/skill-productivity-weights {:optional true}
        [:map-of
         [:enum :sim/skill.intellect :sim/skill.social :sim/skill.fitness]
         [:double {:min 0 :max 1}]]]
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
            [:effect.direction/to-sim :sim/physical-stress -0.05]
            [:effect.direction/to-sim :sim/mental-stress -0.05]
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
            [:effect.direction/to-sim :sim/physical-stress -0.05]
            [:effect.direction/to-sim :sim/mental-stress -0.05]
            [:effect.direction/to-sim :sim/mental-stress 0.02]
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
            [:effect.direction/to-sim :sim/physical-stress -0.05]
            [:effect.direction/to-sim :sim/mental-stress -0.05]]}]}

        {:blueprint/id :improvement.type/farm
         :blueprint/label "Farm"
         :blueprint/icon "🌽"
         :blueprint/description "Produces food"
         :blueprint/price 5000
         :blueprint/offerables
         [{:offerable/id :offer/farm.job
           :offerable/label "Job"
           :offerable/capacity 2
           :offerable/time-shifts #{:time-shift/morning
                                    :time-shift/afternoon}
           :offerable/skill-productivity-weights {:sim/skill.intellect 0.1
                                                  :sim/skill.fitness 0.8
                                                  :sim/skill.social 0.1}
           :offerable/var [{:var/id :var/job-rate
                            :var/label "Job Rate"
                            :var/unit [:/ :resource/money :resource/time]}]
           :offerable/effects
           [[:effect.direction/from-sim :resource/time 1]
            [:effect.direction/to-sim :resource/money :var/job-rate]
            [:effect.direction/from-player :resource/money :var/job-rate]
            [:effect.direction/to-player :resource/food 12]]}]}

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
           :offerable/skill-productivity-weights {:sim/skill.intellect 0.4
                                                  :sim/skill.fitness 0.1
                                                  :sim/skill.social 0.5}
           :offerable/var [{:var/id :var/job-rate
                            :var/label "Job Rate"
                            :var/unit [:/ :resource/money :resource/time]}]
           :offerable/effects
           [[:effect.direction/from-sim :resource/time 1]
            [:effect.direction/to-sim :resource/money :var/job-rate]
            [:effect.direction/to-sim :sim/physical-stress 0.02]
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
           [[:effect.direction/from-sim :resource/money :var/food-price]
            [:effect.direction/to-sim :resource/food 1]
            [:effect.direction/from-self :resource/labour 0.5]
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
           :offerable/skill-productivity-weights {:sim/skill.intellect 0.4
                                                  :sim/skill.fitness 0.5
                                                  :sim/skill.social 0.1}
           :offerable/var [{:var/id :var/job-rate
                            :var/label "Job Rate"
                            :var/unit [:/ :resource/money :resource/time]}]
           :offerable/effects
           [[:effect.direction/from-sim :resource/time 1]
            [:effect.direction/to-sim :resource/money :var/job-rate]
            [:effect.direction/to-sim :sim/physical-stress 0.02]
            [:effect.direction/from-player :resource/money :var/job-rate]
            [:effect.direction/to-player :resource/food 20]]}]}

        {:blueprint/id :improvement.type/monument
         :blueprint/label "Monument"
         :blueprint/icon "🗿"
         :blueprint/description "It's not good for anything, but looks cool I guess?"
         :blueprint/price 500000
         :blueprint/offerables []}
        ]
       (key-by :blueprint/id)))

(m/assert [:map-of :keyword Blueprint] blueprints)
#_(malli.error/humanize (m/explain [:map-of :keyword Blueprint] blueprints))

(def offerables
  (->> blueprints
       vals
       (mapcat :blueprint/offerables)
       (key-by :offerable/id)))

(def sim-attributes
  {:sim/physical-stress {:sim-attribute/icon "😰"
                         :sim-attribute/label "physical stress"}
   :sim/mental-stress {:sim-attribute/icon "🤯"
                       :sim-attribute/label "mental stress"}
   :sim/skill.intellect {:sim-attribute/icon "🧠"
                         :sim-attribute/label "intellect"}
   :sim/skill.fitness {:sim-attribute/icon "💪"
                       :sim-attribute/label "fitness"}
   :sim/skill.social {:sim-attribute/icon "🗣️"
                      :sim-attribute/label "social"}})

(defn resolve-effect-amount
  [offer [_direction _target amount]]
  (if (keyword? amount)
    ;; offerables currently have at most one var, so the offer's amount is its value
    (:offer/amount offer)
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
      (contains? direction-targets [:effect.direction/to-sim :resource/food])
      :offer.category/food-sale
      (contains? direction-targets [:effect.direction/to-sim :resource/shelter])
      :offer.category/housing
      (contains? direction-targets [:effect.direction/from-sim :resource/time])
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
                                       :effect.direction/to-sim} direction)
                          (contains? resources target)
                          (not= :resource/money target))
                 target)))
       first))

(def Email
  [:re #"^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\.[a-zA-Z]{2,}$"])

;; https://docs.datomic.com/schema/schema-reference.html
(def schema
  {:entity/island
   {:island/id {:dat/type :db.type/uuid
                :dat/unique :dat.unique/identity}
    :island/seed {:dat/type :db.type/long
                  :dat/spec PosInt}
    :island/public-stats {}
    :island/government-money-balance {:dat/type :db.type/long
                                      :dat/spec PosInt}
    :island/joy {:dat/type :db.type/long
                 :dat/spec PosInt}
    :island/residents {:dat/rel [:dat.rel/many :entity/resident :resident/id]
                       :dat/component? true}
    :island/sims {:dat/rel [:dat.rel/many :entity/sim :sim/id]
                  :dat/component? true}
    :island/lots {:dat/rel [:dat.rel/many :entity/lot :lot/id]
                  :dat/component? true}
    :island/epoch {:dat/type :db.type/long
                   :dat/spec PosInt}}

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
                             :dat/spec PosInt}
    :resident/stocks {:dat/rel [:dat.rel/many :entity/stock :stock/id]
                      :dat/component? true}
    :resident/deeds {:dat/rel [:dat.rel/many :entity/deed :deed/id]
                     :dat/component? true}
    :resident/loans {:dat/rel [:dat.rel/many :entity/loan :loan/id]
                     :dat/component? true}}

   :entity/sim
   (-> {:sim/id {:dat/type :db.type/uuid
                 :dat/unique :dat.unique/identity}
        :sim/savings {:dat/type :db.type/float
                      :dat/spec [:double {:min 0}]
                      ::generator-immigrant (fn []
                                              (* 1200.0 (math/beta 5 5)))
                      ::generator-baby (fn [] 0.0)}
        :sim/age-ticks {:dat/type :db.type/long
                        :dat/spec [:int {:min 0}]
                        ::generator-immigrant (fn []
                                                (int (* 100 ticks-per-year (math/beta 20 50))))
                        ::generator-baby (fn [] 0)}}
       (into (for [k [;; sims have different 'preferences' with regards to how they can spend their time
                      ;; preferences range from 0.0 to 1.0, and start around 0.5
                      ;; preferences are set at birth/immigration
                      :sim/preference.security
                      :sim/preference.self-improvement
                      :sim/preference.physical-stress
                      :sim/preference.mental-stress
                      :sim/preference.spiritual-activity
                      :sim/preference.social-activity
                      :sim/preference.physical-activity
                      :sim/preference.intellectual-activity
                      ;; sims have 'talents', which affect the rate which skills improve
                      ;; talents range from 0.0 to 1.0, and start around 0.5
                      ;; talents are set at birth/immigration
                      :sim/talent.intellect
                      :sim/talent.fitness
                      :sim/talent.social]]
               [k {:dat/type :db.type/float
                   :dat/spec [:double {:min 0 :max 1}]
                   ::generator-immigrant (fn [] (math/beta 5 5))
                   ::generator-baby (fn [] (math/beta 5 5))}]))
       (into (for [k [;; sims have 'skills', which affect productivity, and improve when practised
                      :sim/skill.intellect
                      :sim/skill.fitness
                      :sim/skill.social
                      ;; sims have stress, which is affected by work, leisure, lack of food and shelter
                      :sim/physical-stress
                      :sim/mental-stress]]
               [k {:dat/type :db.type/float
                   :dat/spec [:double {:min 0 :max 1}]
                   ::generator-immigrant (fn [] (math/beta 5 5))
                   ::generator-baby (fn [] 0.1)}])))

   :entity/loan
   {:loan/id {:dat/type :db.type/uuid
              :dat/unique :dat.unique/identity}
    :loan/amount {:dat/type :db.type/long
                  :dat/spec PosInt}
    :loan/annual-interest-rate {:dat/type :db.type/float} ;; positive, typically between 0 and 0.3
    :loan/minimum-daily-payment-amount {:dat/type :db.type/long
                                        :dat/spec PosInt}
    :loan/daily-payment-amount {:dat/type :db.type/long
                                :dat/spec PosInt}}

   :entity/lot
   {:lot/id {:dat/type :db.type/uuid
             :dat/unique :dat.unique/identity}
    :lot/x {:dat/type :db.type/long
            :dat/spec PosInt}
    :lot/y {:dat/type :db.type/long
            :dat/spec PosInt}
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
                           :dat/spec PosInt}}

   :entity/improvement
   {:improvement/id {:dat/type :db.type/uuid
                     :dat/unique :dat.unique/identity}
    :improvement/type {:dat/type :db.type/keyword
                       :dat/spec (into [:enum] (keys blueprints))}
    :improvement/offers {:dat/rel [:dat.rel/many :entity/offer :offer/id]
                         :dat/component? true}
    :improvement/stocks {:dat/rel [:dat.rel/many :entity/stock :stock/id]
                         :dat/component? true}}

   :entity/stock
   {:stock/id {:dat/type :db.type/uuid
               :dat/unique :dat.unique/identity}
    :stock/resource {:dat/type :db.type/keyword
                     :dat/spec Resource}
    :stock/amount {:dat/type :db.type/float
                   :dat/spec [:double {:min 0}]}}

   :entity/offer
   {:offer/id {:dat/type :db.type/uuid
               :dat/unique :dat.unique/identity}
    :offer/type {:dat/type :db.type/keyword
                 :dat/spec (into [:enum] (keys offerables))}
    :offer/amount {:dat/type :db.type/long
                   :dat/spec PosInt}
    :offer/utilization {:dat/type :db.type/float
                        :dat/spec [:double {:min 0 :max 1}]}
    }})

(mr/set-default-registry!
  (dm/->malli-registry schema))
