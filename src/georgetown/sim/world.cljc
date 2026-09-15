(ns georgetown.sim.world)

(defn player-stock-amount [world player-id resource]
  (or (get-in world [:world/players player-id :player/stocks resource :stock/amount])
      0.0))

(defn improvement-stock-amount [world improvement-id resource]
  (or (get-in world [:world/improvements improvement-id :improvement/stocks resource :stock/amount])
      0.0))

(defn update-player-stock [world player-id resource f amount]
  (update-in world [:world/players player-id :player/stocks resource]
             (fn [stock]
               (-> (or stock {:stock/resource resource
                              :stock/amount 0.0})
                   (update :stock/amount (fn [existing]
                                           (max 0.0 (f existing amount))))))))

(defn update-improvement-stock [world improvement-id resource f amount]
  (update-in world [:world/improvements improvement-id :improvement/stocks resource]
             (fn [stock]
               (-> (or stock {:stock/resource resource
                              :stock/amount 0.0})
                   (update :stock/amount (fn [existing]
                                           (max 0.0 (f existing amount))))))))

(defn update-player-money [world player-id amount]
  (update-in world [:world/players player-id :player/money-balance] + amount))

(defn update-citizen-savings [world citizen-id amount]
  (update-in world [:world/citizens citizen-id :citizen/savings]
             (fn [savings]
               (max 0.0 (+ savings amount)))))
