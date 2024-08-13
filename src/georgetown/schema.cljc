(ns georgetown.schema
  (:require
    [bloom.commons.uuid :as uuid]
    [malli.core :as m]
    [malli.registry :as mr]))

(defn key-by [f coll]
  (into {} (map (juxt f identity) coll)))

(def resources
  (->> [{:resource/id :resource/food
         :resource/icon "🥕"
         :resource/label "food"
         :resource/unit-label "meal"
         :resource/description "1 meal for 1 person; citizens need 21 meals per week"}
        {:resource/id :resource/shelter
         :resource/icon "🛌"
         :resource/label "shelter"
         :resource/unit-label "week"
         :resource/description "1 week of shelter for 1 person; citizens need 1 shelter per week"}
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
         :blueprint/price 100
         :blueprint/offerables
         [{:offerable/id :offer/house.rental
           :offerable/label "Rental"
           :offerable/supply-unit :resource/shelter
           :offerable/supply-amount 2
           :offerable/demand-unit :resource/money
           :offerable/demand-amount nil ; :user-value
           }]}
        {:blueprint/id :improvement.type/farm
         :blueprint/label "Farm"
         :blueprint/icon "🌽"
         :blueprint/description "Produces food"
         :blueprint/price 100
         :blueprint/offerables
         [{:offerable/id :offer/farm.food
           :offerable/label "Food"
           :offerable/supply-unit :resource/food
           :offerable/supply-amount 50
           :offerable/demand-unit :resource/money
           :offerable/demand-amount nil ; user value
           }
          {:offerable/id :offer/farm.job
           :offerable/label "Job"
           :offerable/invert? true
           :offerable/supply-unit :resource/money
           :offerable/supply-amount nil ; user value
           :offerable/demand-unit :resource/labour
           :offerable/demand-amount 100}]}]
       (key-by :blueprint/id)))

(def offerables
  (->> blueprints
       vals
       (mapcat :blueprint/offerables)
       (key-by :offerable/id)))

(def schema
  {:entity/island
   {:island/id {:spec :uuid
                :db/unique :db.unique/identity}
    :island/population {:spec :pos-int}
    :island/residents {:rel/many :entity/resident}
    :island/lots {:rel/many :entity/lot}}

   :entity/user
   {:user/id {:spec :uuid
              :db/unique :db.unique/identity}
    :user/residents {:rel/many :entity/resident}}

   :entity/resident
   {:resident/id {:spec :uuid
                   :db/unique :db.unique/identity}
    :resident/money-balance {:spec :pos-int}
    :resident/deeds {:rel/many :entity/deed}}

   :entity/lot
   {:lot/id {:spec :uuid
             :db/unique :db.unique/identity}
    :lot/x {:spec :pos-int}
    :lot/y {:spec :pos-int}
    :lot/deed {:rel/one :entity/deed}
    :lot/improvement {:rel/one :entity/improvement}}

   :entity/deed
   {:deed/id {:spec :uuid
              :db/unique :db.unique/identity}
    :deed/rate {:spec :pos-int}}

   :entity/improvement
   {:improvement/id {:spec :uuid
                     :db/unique :db.unique/identity}
    :improvement/type {:spec (into [:enum]
                                   (keys blueprints))}
    :improvement/offers {:rel/many :entity/offer}}

   :entity/offer
   {:offer/id {:spec [:vec :uuid :keyword]
               :db/unique :db.unique/identity}
    :offer/type {:spec :keyword}
    :offer/amount {:spec :pos-int}}})

(mr/set-default-registry!
  (merge (mr/schemas m/default-registry)
         {:neg-int (m/-simple-schema {:type :neg-int :pred neg-int?})
          :pos-int (m/-simple-schema {:type :pos-int :pred pos-int?})}
         (->> schema
              (map (fn [[k vs]]
                     [k (into [:map]
                              (update-vals vs :spec))]))
              (into {}))
         (->> schema
              (mapcat val)
              (map (fn [[k v]]
                     [k (:spec v)]))
              (into {}))))

;; https://docs.datomic.com/schema/schema-reference.html
;; https://github.com/metosin/malli?tab=readme-ov-file#built-in-schemas
(def malli-type->datalog-type
  {:uuid :db.type/uuid
   :integer :db.type/long
   :pos-int :db.type/long
   :string :db.type/string
   :float :db.type/float
   :keyword :db.type/keyword
   :boolean :db.type/boolean
   :inst :db.type/instant})

;; register functions
#_(do
    (require 'datalevin.interpret)
    (doseq [v [{:db/ident :fn/withdraw
                :db/fn (datalevin.interpret/inter-fn
                         [db resident-id amount]
                         (if-let [resident (datalevin.core/entity db [:resident/id resident-id])]
                           (if (<=  amount (:resident/money-balance resident))
                             [[:db/add (:db/id resident) :resident/money-balance
                               (- (:resident/money-balance resident) amount)]]
                             (throw (ex-info "Insuffient funds" {})))
                           (throw (ex-info (str "No resident with id " resident-id) {}))))}
               {:db/ident :fn/deposit
                :db/fn (datalevin.interpret/inter-fn
                         [db resident-id amount]
                         (if-let [resident (datalevin.core/entity db [:resident/id resident-id])]
                           [[:db/add (:db/id resident) :resident/money-balance
                             (+ (:resident/money-balance resident) amount)]]
                           (throw (ex-info (str "No resident with id " resident-id) {}))))}]]
      (georgetown.db/transact! [v])))

(defn ->datalevin
  [schema]
  (->> schema
       vals
       (apply concat)
       (map (fn [[k o]]
              [k
               {:db/unique (:db/unique o)
                :db/valueType (or (:db/valueType o)
                                  (when (or (:rel/one o)
                                            (:rel/many o))
                                    :db.type/ref)
                                  (malli-type->datalog-type (:spec o))
                                  ;; datelevin is fine with undefined types
                                  (println "No type for " k))
                :db/cardinality (or (when (:rel/many o)
                                      :db.cardinality/many)
                                  (:db/cardinality o)
                                  :db.cardinality/one)
                :db/isComponent (when (:rel/many o)
                                  true)}]))
       (into {})))

#_(tap> (->datalevin schema))


