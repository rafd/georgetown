(ns georgetown.sim.rules.money
  (:require
    [georgetown.sim.constants :as constants]
    [georgetown.sim.debt :as debt]
    [georgetown.sim.world :as world]))

(defn loan-payments
  {:rule/description "Nightly, players make payments on their loans"
   :rule/inputs #{:world/shift :world/loans}
   :rule/outputs #{:world/txs :world/events :world/player-debt-deltas}}
  [{:world/keys [shift loans]}]
  (let [payments (when (= :time-shift/night shift)
                   ;; loan payments are daily, so only charge on the night tick
                   (->> loans
                        (map (fn [loan]
                               {:loan loan
                                :payment-amount (min (Math/ceil (:loan/amount loan))
                                                     (:loan/daily-payment-amount loan))}))))
        paid-off? (fn [{:keys [loan payment-amount]}]
                    (<= (- (:loan/amount loan)
                           payment-amount)
                        0))]
    {:world/txs (->> payments
                     (mapv (fn [{:keys [loan] :as payment}]
                             (if (paid-off? payment)
                               [:db/retractEntity [:loan/id (:loan/id loan)]]
                               [:db/add [:loan/id (:loan/id loan)]
                                :loan/amount (debt/new-amount loan)]))))
     :world/events (->> payments
                        (filter paid-off?)
                        (mapv (fn [{:keys [loan]}]
                                {:event/type :event.type/loan-paid-off
                                 :event/source :source/simulation
                                 :event/visibility :visibility/limited
                                 :event/visibility-player-ids #{(:loan/owner-id loan)}
                                 :event/data {:amount (:loan/amount loan)}})))
     :world/player-debt-deltas (->> payments
                                    (reduce (fn [memo {:keys [loan payment-amount]}]
                                              (update memo (:loan/owner-id loan)
                                                      (fnil + 0) (- payment-amount)))
                                            {}))}))

(defn deed-taxes
  {:rule/description "Players pay taxes on their deeds"
   :rule/inputs #{:world/deed-rates}
   :rule/outputs #{:world/player-tax-deltas}}
  [{:world/keys [deed-rates]}]
  {:world/player-tax-deltas (->> deed-rates
                                 (reduce (fn [memo [owner-id rate _deed-id]]
                                           (update memo owner-id (fnil + 0) (- rate)))
                                         {}))})

(defn apply-player-charges
  {:rule/description "Loan payments and taxes come out of player balances"
   :rule/inputs #{:world/players :world/player-debt-deltas :world/player-tax-deltas}
   :rule/outputs #{:world/players}}
  [{:world/keys [player-debt-deltas player-tax-deltas] :as world}]
  (-> (reduce (fn [memo [player-id amount]]
                (world/update-player-money memo player-id amount))
              world
              (merge-with + player-debt-deltas player-tax-deltas))
      (select-keys [:world/players])))

(defn citizens-dividend
  {:rule/description "Tax revenue and helicopter money go to citizens per capita"
   :rule/inputs #{:world/government-money-balance :world/player-tax-deltas
                  :world/citizens :world/players}
   :rule/outputs #{:world/citizens :world/government-money-balance :world/helicopter-money}}
  ;; government: taxes come in, everything goes back out as a citizens dividend,
  ;; plus helicopter money, to keep the money supply proportional to population
  [{:world/keys [government-money-balance player-tax-deltas citizens players] :as world}]
  (let [population (count citizens)
        government-revenues (->> player-tax-deltas
                                 vals
                                 (reduce + 0)
                                 -)
        dividend (+ government-money-balance government-revenues)
        total-citizen-savings (->> citizens
                                   vals
                                   (map :citizen/savings)
                                   (reduce + 0.0))
        player-balance (->> players
                            vals
                            (map :player/money-balance)
                            (reduce + 0))
        net-money-balance (+ dividend
                             total-citizen-savings
                             player-balance)
        helicopter-money (max 0
                              (- (* constants/money-supply-target-per-citizen population)
                                 net-money-balance))
        per-citizen-dividend (if (pos? population)
                               (/ (+ dividend helicopter-money)
                                  population)
                               0)]
    (-> (reduce (fn [memo citizen-id]
                  (world/update-citizen-savings memo citizen-id per-citizen-dividend))
                world
                (keys citizens))
        (assoc :world/government-money-balance 0
               :world/helicopter-money helicopter-money)
        (select-keys [:world/citizens
                      :world/government-money-balance
                      :world/helicopter-money]))))

(defn interest-demurrage-rate [ratio]
  ;; to prevent hoarding / incentive cash spending, money loses value over time
  ;; bad things happen when players lose all their money, and when citizens lose all their money
  ;; target a 50:50 split, with larger % the further away from 50:50
  ;; which should act as a regulator
  ;; y = 0.005 * ln( x / ( 1 - x ) )
  (cond
    (<= ratio 0)
    10
    (<= 1 ratio)
    0.01
    :else
    (- 1 (* 0.005 (Math/log (/ ratio
                               (- 1 ratio)))))))

(defn demurrage
  {:rule/description "Player cash gains or loses value to regulate the player:citizen money split"
   :rule/inputs #{:world/players :world/citizens :world/government-money-balance}
   :rule/outputs #{:world/players :world/citizens :world/player-interest-deltas
                   :world/cash-ratio-before :world/interest-rate}}
  [{:world/keys [players citizens government-money-balance] :as world}]
  (let [population (count citizens)
        total-citizen-savings (->> citizens
                                   vals
                                   (map :citizen/savings)
                                   (reduce + 0.0))
        player-balance (->> players
                            vals
                            (map :player/money-balance)
                            (reduce + 0))
        net-money-balance (+ government-money-balance
                             total-citizen-savings
                             player-balance)
        cash-ratio-before (if (zero? net-money-balance)
                            0
                            (/ player-balance net-money-balance))
        interest-rate (interest-demurrage-rate cash-ratio-before)
        player-interest-deltas (->> players
                                    (map (fn [[player-id player]]
                                           [player-id
                                            (let [balance (:player/money-balance player)]
                                              (if (pos? balance)
                                                (- (* balance interest-rate) balance)
                                                0))]))
                                    (into {}))
        interest-delta-total (->> player-interest-deltas
                                  vals
                                  (reduce + 0))
        world (reduce (fn [memo [player-id delta]]
                        (world/update-player-money memo player-id delta))
                      world
                      player-interest-deltas)
        ;; players' interest is paid by (or paid to) the citizens, per capita
        world (if (pos? population)
                (reduce (fn [memo citizen-id]
                          (world/update-citizen-savings memo citizen-id
                                                        (/ (- interest-delta-total) population)))
                        world
                        (keys citizens))
                world)]
    (-> world
        (assoc :world/player-interest-deltas player-interest-deltas
               :world/cash-ratio-before cash-ratio-before
               :world/interest-rate interest-rate)
        (select-keys [:world/players
                      :world/citizens
                      :world/player-interest-deltas
                      :world/cash-ratio-before
                      :world/interest-rate]))))

(defn final-balances
  {:rule/description "Post-money-flow balance totals and cash ratio"
   :rule/inputs #{:world/players :world/citizens :world/government-money-balance}
   :rule/outputs #{:world/final-player-balances :world/final-player-balance-total
                   :world/final-citizen-savings :world/final-net-money-balance
                   :world/cash-ratio-after}}
  [{:world/keys [players citizens government-money-balance]}]
  (let [final-player-balances (->> players
                                   (map (fn [[player-id player]]
                                          [player-id (:player/money-balance player)]))
                                   (into {}))
        final-player-balance-total (->> final-player-balances
                                        vals
                                        (reduce + 0))
        final-citizen-savings (->> citizens
                                   vals
                                   (map :citizen/savings)
                                   (reduce + 0.0))
        final-net-money-balance (+ government-money-balance
                                   final-citizen-savings
                                   final-player-balance-total)]
    {:world/final-player-balances final-player-balances
     :world/final-player-balance-total final-player-balance-total
     :world/final-citizen-savings final-citizen-savings
     :world/final-net-money-balance final-net-money-balance
     :world/cash-ratio-after (if (zero? final-net-money-balance)
                               0
                               (/ final-player-balance-total final-net-money-balance))}))

(defn bankruptcies
  {:rule/description "Players with a negative balance are removed from the island"
   :rule/inputs #{:world/final-player-balances :world/improvements}
   :rule/outputs #{:world/txs :world/events}}
  ;; docs.bankruptcy - if a player's money balance ever falls below 0,
  ;; they are bankrupt, and removed from the island
  [{:world/keys [final-player-balances improvements]}]
  (let [bankrupt-player-ids (->> final-player-balances
                                 (keep (fn [[player-id balance]]
                                         (when (neg? balance)
                                           player-id)))
                                 set)]
    {:world/txs (->> bankrupt-player-ids
                     (mapcat (fn [player-id]
                               (conj
                                 ;; retract improvements
                                 (->> improvements
                                      (keep (fn [[improvement-id improvement]]
                                              (when (= player-id (:improvement/owner-id improvement))
                                                [:db/retractEntity [:improvement/id improvement-id]])))
                                      vec)
                                 ;; and the player (and nested entities)
                                 [:db/retractEntity [:player/id player-id]])))
                     vec)
     :world/events (->> bankrupt-player-ids
                        (mapv (fn [player-id]
                                {:event/type :event.type/player-bankrupt
                                 :event/source :source/simulation
                                 :event/data {:player-id player-id}})))}))

(def rules
  [#'loan-payments
   #'deed-taxes
   #'apply-player-charges
   #'citizens-dividend
   #'demurrage
   #'final-balances
   #'bankruptcies])
