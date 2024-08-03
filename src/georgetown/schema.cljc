(ns georgetown.schema
  (:require
    [bloom.commons.uuid :as uuid]
    [malli.core :as m]
    [malli.registry :as mr]))

(def improvement-types
  [{:id :improvement.type/house
    :price 100
    :offers
    [{:id :offer.house/rental
      :supply [1 :shelter]
      :demand [:X :money]}]}
   {:id :improvement.type/farm
    :price 100
    :offers
    [{:id :offer/farm.food
      :supply [5 :food]
      :demand [:X :money]}
     {:id :offer/farm.job
      :supply [:X :money]
      :demand [10 :labour]}]}])

(def schema
  {:entity/user
   {:user/id {:spec :uuid
              :default uuid/random
              :db/unique :db.unique/identity}}

   :entity/lot
   {:lot/id {:spec :uuid
             :default uuid/random
             :db/unique :db.unique/identity}
    :lot/x {:spec :pos-int}
    :lot/y {:spec :pos-int}}

   :entity/deed
   {:deed/id {:spec :uuid
              :default uuid/random
              :db/unique :db.unique/identity}
    :deed/rate {:spec :pos-int}
    :deed/lot {:spec :entity/lot
               :ref :lot/id}
    :deed/owner {:spec :entity/user
                 :ref :user/id}}

   :entity/improvement
   {:improvement/id {:spec :uuid
                     :default uuid/random
                     :db/unique :db.unique/identity}
    :improvement/type {:spec (into [:enum]
                                   (->> improvement-types
                                        (map :id)))}
    :improvement/lot {:spec :entity/lot
                      :ref :lot/id}}

   :entity/offer
   {:offer/type {:spec :keyword}
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

(def by-key
  (->> schema
       vals
       (apply concat)
       (into {})))

(defn ->datalevin
  [schema]
  (->> by-key
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

(defn ref-key [attr]
  (-> by-key attr :ref))

#_(ref-key :purchase/product)

(defn ->type [entity]
  (->> entity
       (keep (fn [[k _v]]
                 (when (:db/unique (by-key k))
                   (namespace k))))
       first))

(defn ->ref
  "{:entity/id 123 ...} => [:entity/id 123]"
  [entity]
  (->> entity
       (keep (fn [[k v]]
               (when (= (-> by-key k :db/unique)
                        :db.unique/identity)
                 [k v])))
       first))

#_(->ref {:purchase/id 123})

(defn blank [entity-type]
  (->> (schema entity-type)
       (map (fn [[k v]]
              [k (when-let [f (:default v)]
                   (f))]))
       (into {})))

