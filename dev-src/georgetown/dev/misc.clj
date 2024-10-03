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
