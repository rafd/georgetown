(ns georgetown.dev.misc
  (:require
    [georgetown.db :as db]))

;; grant all residencies some money
#_(doseq [r-id (db/q '[:find [?residency ...]
                       :where
                       [?residency :residency/id _]])]
    (db/transact! [[:db/add r-id :residency/money-balance 1000]]))

#_(db/q '[:find [(pull ?residency [*]) ...]
        :where
        [?residency :residency/id _]])
