(ns georgetown.schema
  (:require
    [bloom.commons.uuid :as uuid]
    [malli.core :as m]
    [malli.registry :as mr]))

(defn key-by [f coll]
  (into {} (map (juxt f identity) coll)))

(def blueprints
  (->> [{:blueprint/id :improvement.type/house
         :blueprint/label "House"
         :blueprint/icon "🏠"
         :blueprint/description "Provides shelter"
         :blueprint/price 100
         :blueprint/offerables
         [{:offerable/id :offer/house.rental
           :offerable/label "Rental"
           :offerable/supply [1 :shelter]
           :offerable/demand [:X :money]}]}
        {:blueprint/id :improvement.type/farm
         :blueprint/label "Farm"
         :blueprint/icon "🌽"
         :blueprint/description "Produces food"
         :blueprint/price 100
         :blueprint/offerables
         [{:offerable/id :offer/farm.food
           :offerable/label "Food"
           :offerable/supply [5 :food]
           :offerable/demand [:X :money]}
          {:offerable/id :offer/farm.job
           :offerable/label "Job"
           :offerable/supply [:X :money]
           :offerable/demand [10 :labour]}]}]
       (key-by :blueprint/id)))

(def schema
  {:entity/user
   {:user/id {:spec :uuid
              :db/unique :db.unique/identity}}

   :entity/lot
   {:lot/id {:spec :uuid
             :db/unique :db.unique/identity}
    :lot/x {:spec :pos-int}
    :lot/y {:spec :pos-int}}

   :entity/deed
   {:deed/id {:spec :uuid
              :db/unique :db.unique/identity}
    :deed/rate {:spec :pos-int}
    :deed/lot {:spec :entity/lot
               :ref :lot/id}
    :deed/owner {:spec :entity/user
                 :ref :user/id}}

   :entity/improvement
   {:improvement/id {:spec :uuid
                     :db/unique :db.unique/identity}
    :improvement/type {:spec (into [:enum]
                                   (keys blueprints))}
    :improvement/lot {:spec :entity/lot
                      :ref :lot/id}}

   :entity/offer
   {:offer/id {:spec [:vec :uuid :keyword]
               :db/unique :db.unique/identity}
    :offer/improvement {:spec :entity/improvement
                        :ref :improvement/id}
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
(defn malli-type->datalog-type [x]
  (get (merge {:uuid :db.type/uuid
               :integer :db.type/long
               :pos-int :db.type/long
               :string :db.type/string
               :float :db.type/float
               :keyword :db.type/keyword
               :boolean :db.type/boolean
               :inst :db.type/instant}
              (zipmap (keys schema)
                      (repeat :db.type/ref)))
    x))

(defn ->datalevin
  [schema]
  (->> schema
       vals
       (apply concat)
       (map (fn [[k o]]
              [k
               {:db/unique (:db/unique o)
                :db/valueType (or (:db/valueType o)
                                  (malli-type->datalog-type (:spec o))
                                  ;; datelevin is fine with undefined types
                                  (println "No type for " k))
                :db/cardinality (or (:db/cardinality o)
                                    :db.cardinality/one)}]))
       (into {})))


#_(tap> (->datalevin schema))


