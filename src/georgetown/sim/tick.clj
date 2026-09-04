(ns georgetown.sim.tick
  (:require
    [bloom.commons.uuid :as uuid]
    [com.rpl.specter :as x]
    [georgetown.server.db :as db]
    [georgetown.sim.blueprints :as blueprints]
    [georgetown.sim.citizen :as citizen]
    [georgetown.sim.constants :as constants]
    [georgetown.sim.debt :as debt]
    [georgetown.sim.market :as m]
    [georgetown.sim.schema :as schema]))

(defn clamp01 [value]
  (-> value
      (max 0.0)
      (min 1.0)))

(defn mean-stress [citizen]
  (/ (+ (:citizen/physical-stress citizen)
        (:citizen/mental-stress citizen))
     2))

(defn stress-citizen [citizen amount]
  (-> citizen
      (update :citizen/physical-stress (fn [stress] (clamp01 (+ stress amount))))
      (update :citizen/mental-stress (fn [stress] (clamp01 (+ stress amount))))))

(defn epoch->shift [epoch]
  (nth constants/shift-order (mod epoch (count constants/shift-order))))

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
                                   (filter :offer/amount)
                                   (map (fn [offer]
                                          (assoc offer
                                            :offer/improvement-id improvement-id
                                            :offer/owner-id (:improvement/owner-id improvement)
                                            :offer/category (blueprints/offer-category offer))))))))]
    {:world/epoch (:island/epoch island)
     :world/shift (epoch->shift (:island/epoch island))
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
     :world/utilizations (->> improvements
                              vals
                              (mapcat :improvement/offers)
                              (map (fn [offer]
                                     [(:offer/id offer) 0.0]))
                              (into {}))
     :world/stats {}}))

;; ---- world accessors/mutators ----

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

;; ---- food & housing markets ----

(defn food-sale-tenders
  "Sequential per player, so combined offers cannot sell more food than the player holds."
  [world]
  (->> (:world/offers world)
       (filter (fn [offer]
                 (= :offer.category/food-sale (:offer/category offer))))
       (group-by :offer/owner-id)
       (mapcat (fn [[owner-id owner-offers]]
                 (:tenders
                   (reduce
                     (fn [{:keys [remaining-food tenders]} offer]
                       (let [labour-needed (blueprints/effect-sum offer :effect.direction/from-self :resource/labour)
                             labour-stock (improvement-stock-amount world
                                                                    (:offer/improvement-id offer)
                                                                    :resource/labour)
                             quantity (Math/floor
                                        (min remaining-food
                                             (if (pos? labour-needed)
                                               (/ labour-stock labour-needed)
                                               ##Inf)))]
                         {:remaining-food (- remaining-food quantity)
                          :tenders (conj tenders
                                         {:tender/offer-id (:offer/id offer)
                                          :tender/player-id owner-id
                                          :tender/improvement-id (:offer/improvement-id offer)
                                          :tender/unit-price (:offer/amount offer)
                                          :tender/supply [:resource/food quantity]
                                          :tender/demand [:resource/money (* quantity (:offer/amount offer))]})}))
                     {:remaining-food (player-stock-amount world owner-id :resource/food)
                      :tenders []}
                     owner-offers))))))

(defn housing-tenders
  [world]
  (->> (:world/offers world)
       (filter (fn [offer]
                 (= :offer.category/housing (:offer/category offer))))
       (map (fn [offer]
              (let [capacity (or (:offerable/capacity (blueprints/offerables (:offer/type offer)))
                                 0)]
                {:tender/offer-id (:offer/id offer)
                 :tender/player-id (:offer/owner-id offer)
                 :tender/improvement-id (:offer/improvement-id offer)
                 :tender/unit-price (:offer/amount offer)
                 :tender/supply [:resource/shelter capacity]
                 :tender/demand [:resource/money (* capacity (:offer/amount offer))]})))))

(defn run-goods-market
  "Aggregate market for food (every tick) or shelter (night tick).
  Two passes: pass 1 sets the clearing price; citizens that cannot afford it
  go without (and gain stress); pass 2, at the reduced demand, determines
  which suppliers actually sell."
  [world resource]
  (let [tenders (case resource
                  :resource/food (food-sale-tenders world)
                  :resource/shelter (housing-tenders world))
        citizens (vals (:world/citizens world))
        population (count citizens)
        total-savings (->> citizens
                           (map :citizen/savings)
                           (reduce + 0.0))
        {clearing-price :market/clearing-unit-price}
        (m/market resource population
                  :resource/money total-savings
                  tenders)
        buyers (if clearing-price
                 (->> citizens
                      (filter (fn [citizen]
                                (> (:citizen/savings citizen) clearing-price))))
                 [])
        {demand-filled :market/demand-filled
         supply-consumed :market/supply-consumed
         final-tenders :market/tenders}
        (m/market resource (count buyers)
                  :resource/money (->> buyers
                                       (map :citizen/savings)
                                       (reduce + 0.0))
                  tenders)
        served-count (long demand-filled)
        average-price (if (pos? served-count)
                        (/ supply-consumed demand-filled)
                        0.0)
        served-citizens (take served-count (shuffle buyers))
        unserved-citizens (remove (set (map :citizen/id served-citizens))
                              (map :citizen/id citizens))
        stress-increase (case resource
                          :resource/food constants/hungry-stress-increase
                          :resource/shelter constants/unhoused-stress-increase)
        available-supply (->> tenders
                              (map (fn [tender]
                                     (second (:tender/supply tender))))
                              (reduce + 0))]
    (-> world
        ;; buyers pay the average unit price, so money exactly matches supplier receipts
        (as-> world*
          (reduce (fn [memo citizen]
                    (update-citizen-savings memo (:citizen/id citizen) (- average-price)))
                  world*
                  served-citizens))
        (as-> world*
          (reduce (fn [memo citizen-id]
                    (update-in memo [:world/citizens citizen-id] stress-citizen stress-increase))
                  world*
                  unserved-citizens))
        (as-> world*
          (reduce (fn [memo tender]
                    (let [fill-amount (or (:tender/fill-amount tender) 0)
                          revenue (* fill-amount (:tender/unit-price tender))]
                      (-> memo
                          (update-player-money (:tender/player-id tender) revenue)
                          (cond->
                            (= :resource/food resource)
                            (-> (update-player-stock (:tender/player-id tender) :resource/food - fill-amount)
                                (update-improvement-stock (:tender/improvement-id tender) :resource/labour - fill-amount)))
                          (assoc-in [:world/utilizations (:tender/offer-id tender)]
                                    (double (or (:tender/fill-ratio tender) 0))))))
                  world*
                  final-tenders))
        (assoc-in [:world/stats resource]
                  {:demand population
                   :affordable-demand (count buyers)
                   :available-supply available-supply
                   :supply demand-filled
                   :clearing-price clearing-price
                   :average-price average-price
                   :cost supply-consumed
                   :unserved-count (count unserved-citizens)}))))

;; ---- work & leisure allocation ----

(defn allocate-shift
  "Assigns each citizen's current shift to at most one time-offer.
  Draft implementation: random choice among affordable offers with
  remaining capacity (and solvent owners); nil means idle.
  Later: replaced by an optimizer over citizen preferences (same interface),
  which will also use :allocate.in/food-price and :allocate.in/shelter-price."
  [{:allocate.in/keys [citizens offers player-budgets]}]
  (:allocations
    (reduce
      (fn [{:keys [allocations capacities budgets] :as memo} citizen]
        (let [candidates (->> offers
                              (filter (fn [offer]
                                        (let [capacity (get capacities (:offer/id offer))]
                                          (and
                                            (or (nil? capacity)
                                                (pos? capacity))
                                            (<= (:allocate/citizen-money-cost offer)
                                                (:citizen/savings citizen))
                                            (<= (:allocate/wage offer)
                                                (get budgets (:offer/owner-id offer) 0)))))))
              choice (rand-nth (conj (vec candidates) nil))]
          (if (nil? choice)
            (update memo :allocations assoc (:citizen/id citizen) nil)
            {:allocations (assoc allocations (:citizen/id citizen) (:offer/id choice))
             :capacities (if (get capacities (:offer/id choice))
                           (update capacities (:offer/id choice) dec)
                           capacities)
             :budgets (update budgets (:offer/owner-id choice) - (:allocate/wage choice))})))
      {:allocations {}
       :capacities (->> offers
                        (keep (fn [offer]
                                (when-let [capacity (:offerable/capacity (blueprints/offerables (:offer/type offer)))]
                                  [(:offer/id offer) capacity])))
                        (into {}))
       :budgets player-budgets}
      (shuffle citizens))))

(def skill->talent
  {:citizen/skill.intellect :citizen/talent.intellect
   :citizen/skill.fitness :citizen/talent.fitness
   :citizen/skill.social :citizen/talent.social})

(defn productivity [citizen weights]
  (if (seq weights)
    (->> weights
         (map (fn [[skill weight]]
                (* (get citizen skill 0.0) weight)))
         (reduce +))
    1.0))

(defn grow-skills [citizen weights]
  (reduce (fn [citizen* [skill weight]]
            (update citizen* skill
                    (fn [level]
                      (clamp01 (+ level
                                  (* constants/learn-rate
                                     (get citizen* (skill->talent skill))
                                     weight
                                     (- 1 level)))))))
          citizen
          weights))

(defn apply-assignment-effects
  [world citizen-id offer]
  (let [offerable (blueprints/offerables (:offer/type offer))
        citizen (get-in world [:world/citizens citizen-id])
        productivity-factor (productivity citizen (:offerable/skill-productivity-weights offerable))
        world (update-in world [:world/citizens citizen-id]
                         grow-skills (:offerable/skill-productivity-weights offerable))]
    (reduce
      (fn [world* [direction target _ :as effect]]
        (let [amount (blueprints/resolve-effect-amount offer effect)]
          (case direction
            :effect.direction/from-citizen
            (case target
              :resource/time world*
              :resource/money (update-citizen-savings world* citizen-id (- amount))
              world*)
            :effect.direction/to-citizen
            (cond
              (= :resource/money target)
              (update-citizen-savings world* citizen-id amount)
              (contains? #{:citizen/physical-stress :citizen/mental-stress
                           :citizen/skill.intellect :citizen/skill.fitness :citizen/skill.social} target)
              (update-in world* [:world/citizens citizen-id target]
                         (fn [value] (clamp01 (+ value amount))))
              :else
              world*)
            :effect.direction/from-player
            (if (= :resource/money target)
              (update-player-money world* (:offer/owner-id offer) (- amount))
              (update-player-stock world* (:offer/owner-id offer) target - amount))
            :effect.direction/to-player
            (if (= :resource/money target)
              (update-player-money world* (:offer/owner-id offer) amount)
              (update-player-stock world* (:offer/owner-id offer) target +
                                     (* amount productivity-factor)))
            :effect.direction/from-self
            (update-improvement-stock world* (:offer/improvement-id offer) target - amount)
            :effect.direction/to-self
            (update-improvement-stock world* (:offer/improvement-id offer) target +
                                      (* amount productivity-factor)))))
      world
      (:offerable/effects offerable))))

(defn run-allocation
  [world]
  (let [shift (:world/shift world)
        time-offers (->> (:world/offers world)
                         (filter (fn [offer]
                                   (and (= :offer.category/time (:offer/category offer))
                                        (contains? (:offerable/time-shifts (blueprints/offerables (:offer/type offer)))
                                                   shift))))
                         (map (fn [offer]
                                (assoc offer
                                  :allocate/citizen-money-cost (blueprints/effect-sum offer :effect.direction/from-citizen :resource/money)
                                  :allocate/wage (blueprints/effect-sum offer :effect.direction/from-player :resource/money)))))
        allocations (allocate-shift
                      {:allocate.in/citizens (vals (:world/citizens world))
                       :allocate.in/offers time-offers
                       :allocate.in/player-budgets (->> (:world/players world)
                                                          (map (fn [[player-id player]]
                                                                 [player-id
                                                                  (max 0 (:player/money-balance player))]))
                                                          (into {}))
                       :allocate.in/food-price (get-in world [:world/stats :resource/food :clearing-price])
                       :allocate.in/shelter-price (get-in world [:world/stats :resource/shelter :clearing-price])})
        offers-by-id (->> time-offers
                          (map (fn [offer]
                                 [(:offer/id offer) offer]))
                          (into {}))
        assigned-counts (->> allocations
                             vals
                             (remove nil?)
                             frequencies)]
    (-> (reduce (fn [world* [citizen-id offer-id]]
                  (if offer-id
                    (apply-assignment-effects world* citizen-id (offers-by-id offer-id))
                    world*))
                world
                allocations)
        (as-> world*
          (reduce (fn [memo [offer-id offer]]
                    (let [capacity (:offerable/capacity (blueprints/offerables (:offer/type offer)))
                          assigned (get assigned-counts offer-id 0)]
                      (assoc-in memo [:world/utilizations offer-id]
                                (double
                                  (if capacity
                                    (/ assigned capacity)
                                    (if (pos? assigned) 1 0))))))
                  world*
                  offers-by-id))
        (update :world/stats assoc
                :employed-count (->> allocations
                                     vals
                                     (keep offers-by-id)
                                     (filter (fn [offer]
                                               (pos? (:allocate/wage offer))))
                                     count)
                :idle-count (->> allocations
                                 vals
                                 (filter nil?)
                                 count)))))

;; ---- per-citizen maintenance ----

(defn amp-stress [citizen]
  (let [age-factor (+ 0.5 (/ (citizen/age-in-years citizen) 100))]
    (-> citizen
        (update :citizen/physical-stress
                (fn [stress]
                  (clamp01 (+ stress (* constants/stress-amp-base age-factor (+ 0.5 stress))))))
        (update :citizen/mental-stress
                (fn [stress]
                  (clamp01 (+ stress (* constants/stress-amp-base age-factor (+ 0.5 stress)))))))))

(defn decline-skills [citizen]
  (let [decline-factor (* constants/skill-decline-base
                          (+ 0.5 (/ (citizen/age-in-years citizen) 100))
                          (+ 0.5 (mean-stress citizen)))]
    (reduce (fn [citizen* skill]
              (update citizen* skill (fn [level] (clamp01 (* level (- 1 decline-factor))))))
            citizen
            (keys skill->talent))))

(defn run-citizen-maintenance
  [world]
  (update world :world/citizens
          (fn [citizens]
            (->> citizens
                 (map (fn [[citizen-id citizen]]
                        [citizen-id (-> citizen
                                    (update :citizen/age-ticks inc)
                                    amp-stress
                                    decline-skills)]))
                 (into {})))))

(defn death-chance [citizen]
  (* constants/base-death-chance
     (+ 1 (* constants/death-stress-factor (mean-stress citizen)))
     (Math/pow (/ (+ (citizen/age-in-years citizen) 1) 40) 2)))

(defn pick-dead-citizen-ids [world]
  (->> (:world/citizens world)
       vals
       (filter (fn [citizen]
                 (< (rand) (death-chance citizen))))
       (map :citizen/id)))

(defn randomize [n odds]
  (->> (repeatedly (fn [] (< (rand) odds)))
       (take n)
       (filter true?)
       count))

;; ---- money regulation (moved over from the old loop) ----

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

(defn player-bankruptcy-txs
  [player-money-balances]
  ;; docs.bankruptcy - if a player's money balance every falls below 0, they are bankrupt, and removed from the island
  (->> player-money-balances
       (keep (fn [[player-id balance]]
               (when (< balance 0)
                 player-id)))
       (mapcat (fn [player-id]
                 (conj
                   ;; retract improvements
                   (->> (db/q '[:find [?improvement ...]
                                :in $ ?player-id
                                :where
                                [?player :player/id ?player-id]
                                [?player :player/deeds ?deed]
                                [?lot :lot/deed ?deed]
                                [?lot :lot/improvement ?improvement]]
                              player-id)
                        (map (fn [improvement-entity]
                               [:db/retractEntity improvement-entity])))
                   ;; and the player (and nested entities)
                   [:db/retractEntity [:player/id player-id]])))))

(defn loans
  [island-id]
  (let [payments
        (->> (db/q
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
                    [loan
                     (-> loan :player/_loans :player/id)
                     (min (Math/ceil (:loan/amount loan))
                          (:loan/daily-payment-amount loan))])))]
    {:loan-txs
     (->> payments
          (map (fn [[loan _player-id payment-amount]]
                 (if (<= (- (:loan/amount loan)
                            payment-amount)
                         0)
                   [:db/retractEntity [:loan/id (:loan/id loan)]]
                   [:db/add [:loan/id (:loan/id loan)]
                    :loan/amount (debt/new-amount loan)]))))
     :player-debt-payments
     (->> payments
          (reduce (fn [memo [_loan player-id payment-amount]]
                    (update memo player-id (fnil + 0) (- payment-amount)))
                  {}))}))

(defn taxes
  "For each player, money spent on deed taxes."
  [island-id]
  (->> (db/q
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
         island-id)
       (reduce (fn [memo [owner-id rate _]]
                 (update memo owner-id (fnil + 0) (- rate)))
               {})))

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
        night? (= :time-shift/night (:world/shift world))
        world (-> world
                  (run-goods-market :resource/food)
                  (cond-> night?
                    (run-goods-market :resource/shelter))
                  run-allocation
                  run-citizen-maintenance)
        dead-citizen-ids (set (pick-dead-citizen-ids world))
        world (update world :world/citizens
                      (fn [citizens]
                        (apply dissoc citizens dead-citizen-ids)))
        citizens (vals (:world/citizens world))
        population (count citizens)

        ;; loans & taxes
        {:keys [loan-txs player-debt-payments]} (loans island-id)
        player-taxes (taxes island-id)
        world (reduce (fn [memo [player-id amount]]
                        (update-player-money memo player-id amount))
                      world
                      (merge-with + player-debt-payments player-taxes))

        ;; government: taxes come in, everything goes back out as a citizens dividend
        government-money-balance (:world/government-money-balance world)
        government-revenues (->> player-taxes
                                 vals
                                 (reduce + 0)
                                 -)
        citizens-dividend (+ government-money-balance government-revenues)
        new-government-balance 0

        ;; helicopter money, to keep the money supply proportional to population
        total-citizen-savings (->> citizens
                               (map :citizen/savings)
                               (reduce + 0.0))
        player-balance (->> (:world/players world)
                              vals
                              (map :player/money-balance)
                              (reduce + 0))
        net-money-balance (+ new-government-balance
                             citizens-dividend
                             total-citizen-savings
                             player-balance)
        helicopter-money (max 0
                              (- (* constants/money-supply-target-per-citizen population)
                                 net-money-balance))
        per-citizen-dividend (if (pos? population)
                           (/ (+ citizens-dividend helicopter-money)
                              population)
                           0)
        world (reduce (fn [memo citizen-id]
                        (update-citizen-savings memo citizen-id per-citizen-dividend))
                      world
                      (keys (:world/citizens world)))

        ;; DEMURRAGE / INTEREST
        total-citizen-savings (->> (:world/citizens world)
                               vals
                               (map :citizen/savings)
                               (reduce + 0.0))
        net-money-balance (+ new-government-balance
                             total-citizen-savings
                             player-balance)
        cash-ratio-before (if (zero? net-money-balance)
                            0
                            (/ player-balance net-money-balance))
        interest-rate (interest-demurrage-rate cash-ratio-before)
        player-interest-deltas (->> (:world/players world)
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
                        (update-player-money memo player-id delta))
                      world
                      player-interest-deltas)
        ;; players' interest is paid by (or paid to) the citizens, per capita
        world (if (pos? population)
                (reduce (fn [memo citizen-id]
                          (update-citizen-savings memo citizen-id (/ (- interest-delta-total) population)))
                        world
                        (keys (:world/citizens world)))
                world)

        ;; final balances
        final-player-balances (->> (:world/players world)
                                     (map (fn [[player-id player]]
                                            [player-id (:player/money-balance player)]))
                                     (into {}))
        final-player-balance (->> final-player-balances
                                    vals
                                    (reduce + 0))
        final-citizen-savings (->> (:world/citizens world)
                               vals
                               (map :citizen/savings)
                               (reduce + 0.0))
        final-net-money-balance (+ new-government-balance
                                   final-citizen-savings
                                   final-player-balance)
        cash-ratio-after (if (zero? final-net-money-balance)
                           0
                           (/ final-player-balance final-net-money-balance))

        ;; JOY
        joy (->> (:world/citizens world)
                 vals
                 (map (fn [citizen]
                        (- 1 (mean-stress citizen))))
                 (reduce + 0.0))

        public-stats
        ;; transit is struggling with bignums(?)
        ;; for now, just cast all to double
        (x/transform
          (x/walker number?)
          double
          {:sim.out/shift (:world/shift world)
           :sim.out/population population
           :sim.out/deaths (count dead-citizen-ids)
           :sim.out/total-citizen-savings final-citizen-savings
           :sim.out/mean-physical-stress (if (pos? population)
                                           (/ (->> (:world/citizens world)
                                                   vals
                                                   (map :citizen/physical-stress)
                                                   (reduce + 0.0))
                                              population)
                                           0)
           :sim.out/mean-mental-stress (if (pos? population)
                                         (/ (->> (:world/citizens world)
                                                 vals
                                                 (map :citizen/mental-stress)
                                                 (reduce + 0.0))
                                            population)
                                         0)
           :sim.out/employed-count (get-in world [:world/stats :employed-count])
           :sim.out/idle-count (get-in world [:world/stats :idle-count])
           :sim.out/hungry-count (get-in world [:world/stats :resource/food :unserved-count])
           :sim.out/unhoused-count (get-in world [:world/stats :resource/shelter :unserved-count])
           :sim.out/resources (-> (:world/stats world)
                                  (select-keys [:resource/food :resource/shelter]))
           :sim.out/joy joy
           :sim.out/net-money-balance final-net-money-balance
           :sim.out/government-money-balance new-government-balance
           :sim.out/helicopter-money helicopter-money
           :sim.out/cash-ratio-before cash-ratio-before
           :sim.out/cash-ratio-after cash-ratio-after
           :sim.out/stabilization-rate interest-rate})]
    (db/transact!
      (concat
        (for [[k v] {:island/public-stats public-stats
                     :island/joy (long joy)
                     :island/epoch (inc (:world/epoch world))
                     :island/government-money-balance (long new-government-balance)}]
          [:db/add [:island/id island-id] k v])
        ;; citizens
        (for [citizen (vals (:world/citizens world))]
          (update citizen :citizen/savings double))
        (for [citizen-id dead-citizen-ids]
          [:db/retractEntity [:citizen/id citizen-id]])
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
                       :stats.private/stabilization-payment (double (get player-interest-deltas player-id 0))}]]
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
        loan-txs
        (player-bankruptcy-txs final-player-balances)))
    ;; births & immigration
    (dotimes [_ (randomize population constants/birth-chance-per-citizen-per-tick)]
      (db/add-citizen! island-id (citizen/random ::schema/generator-baby)))
    (when (< (rand) constants/citizen-immigration-chance)
      (db/add-citizen! island-id (citizen/random ::schema/generator-immigrant)))))
