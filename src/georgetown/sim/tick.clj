(ns georgetown.sim.tick
  (:require
    [bloom.commons.uuid :as uuid]
    [georgetown.server.db :as db]
    [georgetown.server.events :as events]
    [georgetown.sim.blueprints :as blueprints]
    [georgetown.sim.constants :as constants]
    [georgetown.sim.engine :as engine]
    [georgetown.sim.rules :as rules]))

;; ---- world extraction ----

(defn stocks-by-resource [stocks]
  (->> stocks
       (map (fn [stock]
              [(:stock/resource stock) stock]))
       (into {})))

(defn extract-world
  [island-id]
  (let [island (db/q '[:find (pull ?island [:island/epoch
                                            :island/government-money-balance
                                            :island/public-stats
                                            {:island/citizens [*]}]) .
                       :in $ ?island-id
                       :where
                       [?island :island/id ?island-id]]
                     island-id)
        players (->> (db/q '[:find [(pull ?player
                                          [:player/id
                                           :player/money-balance
                                           {:player/stocks [:stock/id
                                                            :stock/resource
                                                            :stock/amount]}]) ...]
                             :in $ ?island-id
                             :where
                             [?island :island/id ?island-id]
                             [?island :island/players ?player]]
                           island-id)
                       (map (fn [player]
                              [(:player/id player)
                               {:player/money-balance (:player/money-balance player)
                                :player/stocks (stocks-by-resource (:player/stocks player))}]))
                       (into {}))
        improvements (->> (db/q '[:find (pull ?improvement
                                              [:improvement/id
                                               :improvement/type
                                               {:improvement/offers [:offer/id
                                                                     :offer/type
                                                                     :offer/amount]}
                                               {:improvement/stocks [:stock/id
                                                                     :stock/resource
                                                                     :stock/amount]}])
                                  ?player-id
                                  :in $ ?island-id
                                  :where
                                  [?island :island/id ?island-id]
                                  [?island :island/lots ?lot]
                                  [?lot :lot/improvement ?improvement]
                                  [?lot :lot/deed ?deed]
                                  [?player :player/deeds ?deed]
                                  [?player :player/id ?player-id]]
                                island-id)
                          (map (fn [[improvement player-id]]
                                 [(:improvement/id improvement)
                                  {:improvement/type (:improvement/type improvement)
                                   :improvement/owner-id player-id
                                   :improvement/offers (:improvement/offers improvement)
                                   :improvement/stocks (stocks-by-resource (:improvement/stocks improvement))}]))
                          (into {}))
        offers (->> improvements
                    (mapcat (fn [[improvement-id improvement]]
                              (->> (:improvement/offers improvement)
                                   (filter blueprints/offer-active?)
                                   (map (fn [offer]
                                          (assoc offer
                                            :offer/improvement-id improvement-id
                                            :offer/owner-id (:improvement/owner-id improvement)
                                            :offer/category (blueprints/offer-category offer))))))))
        loans (->> (db/q
                     ;; need loan-id so that it doesn't dedupe
                     '[:find [(pull ?loan [* {:player/_loans
                                              [:player/id]}]) ...]
                       :in $ ?island-id
                       :where
                       [?island :island/id ?island-id]
                       [?island :island/players ?player]
                       [?player :player/loans ?loan]]
                     island-id)
                   (map (fn [loan]
                          ;; datascript returns reverse component refs as a vector,
                          ;; datalevin as a single map
                          (let [owner (:player/_loans loan)
                                owner (if (sequential? owner)
                                        (first owner)
                                        owner)]
                            (-> loan
                                (dissoc :player/_loans :db/id)
                                (assoc :loan/owner-id (:player/id owner)))))))
        deed-rates (db/q
                     ;; need deed-id so that it doesn't dedupe
                     '[:find ?player-id ?rate ?deed-id
                       :in $ ?island-id
                       :where
                       [?island :island/id ?island-id]
                       [?island :island/players ?player]
                       [?player :player/id ?player-id]
                       [?player :player/deeds ?deed]
                       [?deed :deed/id ?deed-id]
                       [?deed :deed/rate ?rate]]
                     island-id)]
    {:world/epoch (:island/epoch island)
     :world/shift (constants/epoch->shift (:island/epoch island))
     :world/previous-public-stats (:island/public-stats island)
     :world/government-money-balance (:island/government-money-balance island)
     :world/citizens (->> (:island/citizens island)
                      (map (fn [citizen]
                             [(:citizen/id citizen) citizen]))
                      (into {}))
     :world/players players
     :world/initial-player-balances (->> players
                                           (map (fn [[player-id player]]
                                                  [player-id (:player/money-balance player)]))
                                           (into {}))
     :world/improvements improvements
     :world/offers offers
     :world/loans loans
     :world/deed-rates deed-rates
     :world/utilizations (->> improvements
                              vals
                              (mapcat :improvement/offers)
                              (map (fn [offer]
                                     [(:offer/id offer) 0.0]))
                              (into {}))}))

;; ---- persistence ----

(defn stock-txs
  [holder-id-attr holder-id stocks-rel stocks]
  (->> stocks
       (keep (fn [[_resource stock]]
               (if (:stock/id stock)
                 [:db/add [:stock/id (:stock/id stock)]
                  :stock/amount (double (:stock/amount stock))]
                 (when (pos? (:stock/amount stock))
                   {holder-id-attr holder-id
                    stocks-rel [(-> stock
                                    (assoc :stock/id (uuid/random))
                                    (update :stock/amount double))]}))))))

(defn tick!
  [island-id]
  (let [world (extract-world island-id)
        world (engine/run rules/all-rules world)
        new-epoch (inc (:world/epoch world))]
    (db/transact!
      (concat
        (for [[k v] {:island/public-stats (:world/public-stats world)
                     :island/joy (long (:world/joy world))
                     :island/epoch new-epoch
                     :island/government-money-balance (long (:world/government-money-balance world))}]
          [:db/add [:island/id island-id] k v])
        ;; citizens
        (for [citizen (vals (:world/citizens world))]
          (update citizen :citizen/savings double))
        ;; players
        (mapcat (fn [[player-id player]]
                  (concat
                    [[:db/add [:player/id player-id]
                      :player/money-balance (long (:player/money-balance player))]
                     [:db/add [:player/id player-id]
                      :player/private-stats
                      {:stats.private/net-cashflow (double
                                                     (- (:player/money-balance player)
                                                        (get-in world [:world/initial-player-balances player-id] 0)))
                       :stats.private/stabilization-payment (double (get (:world/player-interest-deltas world) player-id 0))}]]
                    (stock-txs :player/id player-id
                               :player/stocks (:player/stocks player))))
                (:world/players world))
        ;; improvement stocks
        (mapcat (fn [[improvement-id improvement]]
                  (stock-txs :improvement/id improvement-id
                             :improvement/stocks (:improvement/stocks improvement)))
                (:world/improvements world))
        ;; offer utilization
        (for [[offer-id utilization] (:world/utilizations world)]
          [:db/add [:offer/id offer-id] :offer/utilization utilization])
        ;; events
        ;; (before the :world/txs retractions, so [:player/id ...] lookup refs still resolve)
        (->> (:world/events world)
             (mapcat (fn [event]
                       (events/event-txs island-id
                                         (assoc event :event/epoch new-epoch)))))
        (events/prune-event-txs island-id (- new-epoch constants/event-retention-ticks))
        (:world/txs world)
        ;; births & immigration
        (when (seq (:world/new-citizens world))
          [{:island/id island-id
            :island/citizens (vec (:world/new-citizens world))}])))))
