(ns georgetown.dev.misc
  (:require
    [georgetown.server.db :as db]))

;; grant all residencies some money
#_(doseq [r-id (db/q '[:find [?player ...]
                       :where
                       [?player :player/id _]])]
    (db/transact! [[:db/add r-id :player/money-balance 1000]]))

#_(db/q '[:find [(pull ?player [*]) ...]
        :where
        [?player :player/id _]])

(defn clear-orphaned-improvements! []
  (->> (db/q '[:find [?improvement ...]
               :where
               [?lot :lot/improvement ?improvement]
               [(missing? $ ?lot :lot/deed)]])
       (map (fn [e]
              [:db/retractEntity e]))
       (db/transact!)))

(defn remove-bankrupt-players! []
  (->> (db/q '[:find [?e ...]
               :where
               [?e :player/id _]
               [?e :player/money-balance ?balance]
               [(neg? ?balance)]])
       (map (fn [e-id]
              [:db/retractEntity e-id]))
       (db/transact!)))



