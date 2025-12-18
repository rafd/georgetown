(ns georgetown.dev.misc
  (:require
    [georgetown.db :as db]))

;; grant all residencies some money
#_(doseq [r-id (db/q '[:find [?resident ...]
                       :where
                       [?resident :resident/id _]])]
    (db/transact! [[:db/add r-id :resident/money-balance 1000]]))

#_(db/q '[:find [(pull ?resident [*]) ...]
        :where
        [?resident :resident/id _]])

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
               [?e :resident/id _]
               [?e :resident/money-balance ?balance]
               [(neg? ?balance)]])
       (map (fn [e-id]
              [:db/retractEntity e-id]))
       (db/transact!)))

(defn redominate-balances! []
  (do
   (->> (db/q '[:find ?e ?balance
                :where
                [?e :resident/money-balance ?balance]
                [(< 1000 ?balance)]])
        (map (fn [[ e-id balance]]
               [:db/add e-id :resident/money-balance (int (/ balance 10))]))
        (db/transact!))

   (->> (db/q '[:find ?e ?balance
                :where
                [?e :island/citizen-money-balance ?balance]
                [(< 1000 ?balance)]])
        (map (fn [[ e-id balance]]
               [:db/add e-id :island/citizen-money-balance (int (/ balance 10))]))
        (db/transact!))))

