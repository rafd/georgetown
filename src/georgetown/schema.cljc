(ns georgetown.schema
  (:require
    [bloom.commons.uuid :as uuid]))

(def schema
  {:user
   {:user/id {:spec :uuid
              :default uuid/random
              :db/unique :db.unique/identity}
    :user/handle {:spec :string}}})

;; https://docs.datomic.com/schema/schema-reference.html
;; https://github.com/metosin/malli?tab=readme-ov-file#built-in-schemas
(defn malli-type->datalog-type [x]
  (get (merge {:uuid :db.type/uuid
               :integer :db.type/long
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

