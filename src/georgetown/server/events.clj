(ns georgetown.server.events
  (:require
    [bloom.commons.uuid :as uuid]
    [georgetown.server.db :as db]
    [georgetown.server.state :as s]))

(def feed-limit 100)

(defn event-txs
  [island-id {:event/keys [epoch visibility-player-ids] :as event}]
  [{:island/id island-id
    :island/events
    [(-> {:event/id (uuid/random)
          :event/epoch (or epoch
                           (s/qget [:island/id island-id] [:island/epoch]))
          :event/source :source/player
          :event/visibility :visibility/public}
         (merge (dissoc event :event/visibility-player-ids))
         (merge (when (seq visibility-player-ids)
                  {:event/visibility-players
                   (mapv (fn [player-id]
                           [:player/id player-id])
                         visibility-player-ids)}))
         db/remove-nil-vals)]}])

(defn recent [events]
  (->> events
       (sort-by (juxt :event/epoch
                      (fn [event]
                        (str (:event/id event)))))
       reverse
       (take feed-limit)))

(defn recent-public-events [island-id]
  (->> (db/q '[:find [(pull ?event [*]) ...]
               :in $ ?island-id
               :where
               [?island :island/id ?island-id]
               [?island :island/events ?event]
               [?event :event/visibility :visibility/public]]
             island-id)
       recent))

(defn recent-limited-events [island-id player-id]
  (->> (db/q '[:find [(pull ?event [*]) ...]
               :in $ ?island-id ?player-id
               :where
               [?island :island/id ?island-id]
               [?island :island/events ?event]
               [?event :event/visibility-players ?player]
               [?player :player/id ?player-id]]
             island-id
             player-id)
       recent
       ;; don't ship the recipient list to the client
       (map (fn [event]
              (dissoc event :event/visibility-players)))))

(defn prune-event-txs [island-id cutoff-epoch]
  (->> (db/q '[:find [?event ...]
               :in $ ?island-id ?cutoff-epoch
               :where
               [?island :island/id ?island-id]
               [?island :island/events ?event]
               [?event :event/epoch ?epoch]
               [(< ?epoch ?cutoff-epoch)]]
             island-id
             cutoff-epoch)
       (map (fn [event-entity]
              [:db/retractEntity event-entity]))))
