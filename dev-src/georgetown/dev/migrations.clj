(ns georgetown.dev.migrations
  (:require
    [georgetown.db :as db]))

(defn m2024-09-13-add-deed-changed-at []
  (->> (db/q '[:find [?d ...]
              :where
              [?d :deed/id _]])
       (map (fn [e-id]
              [:db/add e-id :deed/rate-changed-at 0]))
       (db/transact!)))

(defn m2024-09-13-change-offer-ids-to-uuids []
  (->> (db/q '[:find [?d ...]
               :where
               [?d :offer/id _]])
       (map (fn [e-id]
              [:db/add e-id :offer/id (bloom.commons.uuid/random)]))
       (db/transact!)))
