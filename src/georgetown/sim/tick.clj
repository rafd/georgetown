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

(defn mean-stress [sim]
  (/ (+ (:sim/physical-stress sim)
        (:sim/mental-stress sim))
     2))

(defn stress-sim [sim amount]
  (-> sim
      (update :sim/physical-stress (fn [stress] (clamp01 (+ stress amount))))
      (update :sim/mental-stress (fn [stress] (clamp01 (+ stress amount))))))

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
                                            {:island/sims [*]}]) .
                       :in $ ?island-id
                       :where
                       [?island :island/id ?island-id]]
                     island-id)
        residents (->> (db/q '[:find [(pull ?resident
                                            [:resident/id
                                             :resident/money-balance
                                             {:resident/stocks [:stock/id
                                                                :stock/resource
                                                                :stock/amount]}]) ...]
                               :in $ ?island-id
                               :where
                               [?island :island/id ?island-id]
                               [?island :island/residents ?resident]]
                             island-id)
                       (map (fn [resident]
                              [(:resident/id resident)
                               {:resident/money-balance (:resident/money-balance resident)
                                :resident/stocks (stocks-by-resource (:resident/stocks resident))}]))
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
                                  ?resident-id
                                  :in $ ?island-id
                                  :where
                                  [?island :island/id ?island-id]
                                  [?island :island/lots ?lot]
                                  [?lot :lot/improvement ?improvement]
                                  [?lot :lot/deed ?deed]
                                  [?resident :resident/deeds ?deed]
                                  [?resident :resident/id ?resident-id]]
                                island-id)
                          (map (fn [[improvement resident-id]]
                                 [(:improvement/id improvement)
                                  {:improvement/type (:improvement/type improvement)
                                   :improvement/owner-id resident-id
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
     :world/sims (->> (:island/sims island)
                      (map (fn [sim]
                             [(:sim/id sim) sim]))
                      (into {}))
     :world/residents residents
     :world/initial-resident-balances (->> residents
                                           (map (fn [[resident-id resident]]
                                                  [resident-id (:resident/money-balance resident)]))
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

(defn resident-stock-amount [world resident-id resource]
  (or (get-in world [:world/residents resident-id :resident/stocks resource :stock/amount])
      0.0))

(defn improvement-stock-amount [world improvement-id resource]
  (or (get-in world [:world/improvements improvement-id :improvement/stocks resource :stock/amount])
      0.0))

(defn update-resident-stock [world resident-id resource f amount]
  (update-in world [:world/residents resident-id :resident/stocks resource]
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

(defn update-resident-money [world resident-id amount]
  (update-in world [:world/residents resident-id :resident/money-balance] + amount))

(defn update-sim-savings [world sim-id amount]
  (update-in world [:world/sims sim-id :sim/savings]
             (fn [savings]
               (max 0.0 (+ savings amount)))))

;; ---- food & housing markets ----

(defn food-sale-tenders
  "Sequential per resident, so combined offers cannot sell more food than the resident holds."
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
                                          :tender/resident-id owner-id
                                          :tender/improvement-id (:offer/improvement-id offer)
                                          :tender/unit-price (:offer/amount offer)
                                          :tender/supply [:resource/food quantity]
                                          :tender/demand [:resource/money (* quantity (:offer/amount offer))]})}))
                     {:remaining-food (resident-stock-amount world owner-id :resource/food)
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
                 :tender/resident-id (:offer/owner-id offer)
                 :tender/improvement-id (:offer/improvement-id offer)
                 :tender/unit-price (:offer/amount offer)
                 :tender/supply [:resource/shelter capacity]
                 :tender/demand [:resource/money (* capacity (:offer/amount offer))]})))))

(defn run-goods-market
  "Aggregate market for food (every tick) or shelter (night tick).
  Two passes: pass 1 sets the clearing price; sims that cannot afford it
  go without (and gain stress); pass 2, at the reduced demand, determines
  which suppliers actually sell."
  [world resource]
  (let [tenders (case resource
                  :resource/food (food-sale-tenders world)
                  :resource/shelter (housing-tenders world))
        sims (vals (:world/sims world))
        population (count sims)
        total-savings (->> sims
                           (map :sim/savings)
                           (reduce + 0.0))
        {clearing-price :market/clearing-unit-price}
        (m/market resource population
                  :resource/money total-savings
                  tenders)
        buyers (if clearing-price
                 (->> sims
                      (filter (fn [sim]
                                (> (:sim/savings sim) clearing-price))))
                 [])
        {demand-filled :market/demand-filled
         supply-consumed :market/supply-consumed
         final-tenders :market/tenders}
        (m/market resource (count buyers)
                  :resource/money (->> buyers
                                       (map :sim/savings)
                                       (reduce + 0.0))
                  tenders)
        served-count (long demand-filled)
        average-price (if (pos? served-count)
                        (/ supply-consumed demand-filled)
                        0.0)
        served-sims (take served-count (shuffle buyers))
        unserved-sims (remove (set (map :sim/id served-sims))
                              (map :sim/id sims))
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
          (reduce (fn [memo sim]
                    (update-sim-savings memo (:sim/id sim) (- average-price)))
                  world*
                  served-sims))
        (as-> world*
          (reduce (fn [memo sim-id]
                    (update-in memo [:world/sims sim-id] stress-sim stress-increase))
                  world*
                  unserved-sims))
        (as-> world*
          (reduce (fn [memo tender]
                    (let [fill-amount (or (:tender/fill-amount tender) 0)
                          revenue (* fill-amount (:tender/unit-price tender))]
                      (-> memo
                          (update-resident-money (:tender/resident-id tender) revenue)
                          (cond->
                            (= :resource/food resource)
                            (-> (update-resident-stock (:tender/resident-id tender) :resource/food - fill-amount)
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
                   :unserved-count (count unserved-sims)}))))

;; ---- work & leisure allocation ----

(defn allocate-shift
  "Assigns each sim's current shift to at most one time-offer.
  Draft implementation: random choice among affordable offers with
  remaining capacity (and solvent owners); nil means idle.
  Later: replaced by an optimizer over sim preferences (same interface),
  which will also use :allocate.in/food-price and :allocate.in/shelter-price."
  [{:allocate.in/keys [sims offers resident-budgets]}]
  (:allocations
    (reduce
      (fn [{:keys [allocations capacities budgets] :as memo} sim]
        (let [candidates (->> offers
                              (filter (fn [offer]
                                        (let [capacity (get capacities (:offer/id offer))]
                                          (and
                                            (or (nil? capacity)
                                                (pos? capacity))
                                            (<= (:allocate/sim-money-cost offer)
                                                (:sim/savings sim))
                                            (<= (:allocate/wage offer)
                                                (get budgets (:offer/owner-id offer) 0)))))))
              choice (rand-nth (conj (vec candidates) nil))]
          (if (nil? choice)
            (update memo :allocations assoc (:sim/id sim) nil)
            {:allocations (assoc allocations (:sim/id sim) (:offer/id choice))
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
       :budgets resident-budgets}
      (shuffle sims))))

(def skill->talent
  {:sim/skill.intellect :sim/talent.intellect
   :sim/skill.fitness :sim/talent.fitness
   :sim/skill.social :sim/talent.social})

(defn productivity [sim weights]
  (if (seq weights)
    (->> weights
         (map (fn [[skill weight]]
                (* (get sim skill 0.0) weight)))
         (reduce +))
    1.0))

(defn grow-skills [sim weights]
  (reduce (fn [sim* [skill weight]]
            (update sim* skill
                    (fn [level]
                      (clamp01 (+ level
                                  (* constants/learn-rate
                                     (get sim* (skill->talent skill))
                                     weight
                                     (- 1 level)))))))
          sim
          weights))

(defn apply-assignment-effects
  [world sim-id offer]
  (let [offerable (blueprints/offerables (:offer/type offer))
        sim (get-in world [:world/sims sim-id])
        productivity-factor (productivity sim (:offerable/skill-productivity-weights offerable))
        world (update-in world [:world/sims sim-id]
                         grow-skills (:offerable/skill-productivity-weights offerable))]
    (reduce
      (fn [world* [direction target _ :as effect]]
        (let [amount (blueprints/resolve-effect-amount offer effect)]
          (case direction
            :effect.direction/from-sim
            (case target
              :resource/time world*
              :resource/money (update-sim-savings world* sim-id (- amount))
              world*)
            :effect.direction/to-sim
            (cond
              (= :resource/money target)
              (update-sim-savings world* sim-id amount)
              (contains? #{:sim/physical-stress :sim/mental-stress
                           :sim/skill.intellect :sim/skill.fitness :sim/skill.social} target)
              (update-in world* [:world/sims sim-id target]
                         (fn [value] (clamp01 (+ value amount))))
              :else
              world*)
            :effect.direction/from-player
            (if (= :resource/money target)
              (update-resident-money world* (:offer/owner-id offer) (- amount))
              (update-resident-stock world* (:offer/owner-id offer) target - amount))
            :effect.direction/to-player
            (if (= :resource/money target)
              (update-resident-money world* (:offer/owner-id offer) amount)
              (update-resident-stock world* (:offer/owner-id offer) target +
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
                                  :allocate/sim-money-cost (blueprints/effect-sum offer :effect.direction/from-sim :resource/money)
                                  :allocate/wage (blueprints/effect-sum offer :effect.direction/from-player :resource/money)))))
        allocations (allocate-shift
                      {:allocate.in/sims (vals (:world/sims world))
                       :allocate.in/offers time-offers
                       :allocate.in/resident-budgets (->> (:world/residents world)
                                                          (map (fn [[resident-id resident]]
                                                                 [resident-id
                                                                  (max 0 (:resident/money-balance resident))]))
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
    (-> (reduce (fn [world* [sim-id offer-id]]
                  (if offer-id
                    (apply-assignment-effects world* sim-id (offers-by-id offer-id))
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

;; ---- per-sim maintenance ----

(defn amp-stress [sim]
  (let [age-factor (+ 0.5 (/ (citizen/age-in-years sim) 100))]
    (-> sim
        (update :sim/physical-stress
                (fn [stress]
                  (clamp01 (+ stress (* constants/stress-amp-base age-factor (+ 0.5 stress))))))
        (update :sim/mental-stress
                (fn [stress]
                  (clamp01 (+ stress (* constants/stress-amp-base age-factor (+ 0.5 stress)))))))))

(defn decline-skills [sim]
  (let [decline-factor (* constants/skill-decline-base
                          (+ 0.5 (/ (citizen/age-in-years sim) 100))
                          (+ 0.5 (mean-stress sim)))]
    (reduce (fn [sim* skill]
              (update sim* skill (fn [level] (clamp01 (* level (- 1 decline-factor))))))
            sim
            (keys skill->talent))))

(defn run-sim-maintenance
  [world]
  (update world :world/sims
          (fn [sims]
            (->> sims
                 (map (fn [[sim-id sim]]
                        [sim-id (-> sim
                                    (update :sim/age-ticks inc)
                                    amp-stress
                                    decline-skills)]))
                 (into {})))))

(defn death-chance [sim]
  (* constants/base-death-chance
     (+ 1 (* constants/death-stress-factor (mean-stress sim)))
     (Math/pow (/ (+ (citizen/age-in-years sim) 1) 40) 2)))

(defn pick-dead-sim-ids [world]
  (->> (:world/sims world)
       vals
       (filter (fn [sim]
                 (< (rand) (death-chance sim))))
       (map :sim/id)))

(defn randomize [n odds]
  (->> (repeatedly (fn [] (< (rand) odds)))
       (take n)
       (filter true?)
       count))

;; ---- money regulation (moved over from the old loop) ----

(defn interest-demurrage-rate [ratio]
  ;; to prevent hoarding / incentive cash spending, money loses value over time
  ;; bad things happen when residents lose all their money, and when citizens lose all their money
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

(defn resident-bankruptcy-txs
  [resident-money-balances]
  ;; docs.bankruptcy - if a resident's money balance every falls below 0, they are bankrupt, and removed from the island
  (->> resident-money-balances
       (keep (fn [[resident-id balance]]
               (when (< balance 0)
                 resident-id)))
       (mapcat (fn [resident-id]
                 (conj
                   ;; retract improvements
                   (->> (db/q '[:find [?improvement ...]
                                :in $ ?resident-id
                                :where
                                [?resident :resident/id ?resident-id]
                                [?resident :resident/deeds ?deed]
                                [?lot :lot/deed ?deed]
                                [?lot :lot/improvement ?improvement]]
                              resident-id)
                        (map (fn [improvement-entity]
                               [:db/retractEntity improvement-entity])))
                   ;; and the resident (and nested entities)
                   [:db/retractEntity [:resident/id resident-id]])))))

(defn loans
  [island-id]
  (let [payments
        (->> (db/q
               ;; need loan-id so that it doesn't dedupe
               '[:find [(pull ?loan [* {:resident/_loans
                                        [:resident/id]}]) ...]
                 :in $ ?island-id
                 :where
                 [?island :island/id ?island-id]
                 [?island :island/residents ?resident]
                 [?resident :resident/loans ?loan]]
               island-id)
             (map (fn [loan]
                    [loan
                     (-> loan :resident/_loans :resident/id)
                     (min (Math/ceil (:loan/amount loan))
                          (:loan/daily-payment-amount loan))])))]
    {:loan-txs
     (->> payments
          (map (fn [[loan _resident-id payment-amount]]
                 (if (<= (- (:loan/amount loan)
                            payment-amount)
                         0)
                   [:db/retractEntity [:loan/id (:loan/id loan)]]
                   [:db/add [:loan/id (:loan/id loan)]
                    :loan/amount (debt/new-amount loan)]))))
     :resident-debt-payments
     (->> payments
          (reduce (fn [memo [_loan resident-id payment-amount]]
                    (update memo resident-id (fnil + 0) (- payment-amount)))
                  {}))}))

(defn taxes
  "For each resident, money spent on deed taxes."
  [island-id]
  (->> (db/q
         ;; need deed-id so that it doesn't dedupe
         '[:find ?resident-id ?rate ?deed-id
           :in $ ?island-id
           :where
           [?island :island/id ?island-id]
           [?island :island/residents ?resident]
           [?resident :resident/id ?resident-id]
           [?resident :resident/deeds ?deed]
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
                  run-sim-maintenance)
        dead-sim-ids (set (pick-dead-sim-ids world))
        world (update world :world/sims
                      (fn [sims]
                        (apply dissoc sims dead-sim-ids)))
        sims (vals (:world/sims world))
        population (count sims)

        ;; loans & taxes
        {:keys [loan-txs resident-debt-payments]} (loans island-id)
        resident-taxes (taxes island-id)
        world (reduce (fn [memo [resident-id amount]]
                        (update-resident-money memo resident-id amount))
                      world
                      (merge-with + resident-debt-payments resident-taxes))

        ;; government: taxes come in, everything goes back out as a citizens dividend
        government-money-balance (:world/government-money-balance world)
        government-revenues (->> resident-taxes
                                 vals
                                 (reduce + 0)
                                 -)
        citizens-dividend (+ government-money-balance government-revenues)
        new-government-balance 0

        ;; helicopter money, to keep the money supply proportional to population
        total-sim-savings (->> sims
                               (map :sim/savings)
                               (reduce + 0.0))
        resident-balance (->> (:world/residents world)
                              vals
                              (map :resident/money-balance)
                              (reduce + 0))
        net-money-balance (+ new-government-balance
                             citizens-dividend
                             total-sim-savings
                             resident-balance)
        helicopter-money (max 0
                              (- (* constants/money-supply-target-per-sim population)
                                 net-money-balance))
        per-sim-dividend (if (pos? population)
                           (/ (+ citizens-dividend helicopter-money)
                              population)
                           0)
        world (reduce (fn [memo sim-id]
                        (update-sim-savings memo sim-id per-sim-dividend))
                      world
                      (keys (:world/sims world)))

        ;; DEMURRAGE / INTEREST
        total-sim-savings (->> (:world/sims world)
                               vals
                               (map :sim/savings)
                               (reduce + 0.0))
        net-money-balance (+ new-government-balance
                             total-sim-savings
                             resident-balance)
        cash-ratio-before (if (zero? net-money-balance)
                            0
                            (/ resident-balance net-money-balance))
        interest-rate (interest-demurrage-rate cash-ratio-before)
        resident-interest-deltas (->> (:world/residents world)
                                      (map (fn [[resident-id resident]]
                                             [resident-id
                                              (let [balance (:resident/money-balance resident)]
                                                (if (pos? balance)
                                                  (- (* balance interest-rate) balance)
                                                  0))]))
                                      (into {}))
        interest-delta-total (->> resident-interest-deltas
                                  vals
                                  (reduce + 0))
        world (reduce (fn [memo [resident-id delta]]
                        (update-resident-money memo resident-id delta))
                      world
                      resident-interest-deltas)
        ;; residents' interest is paid by (or paid to) the sims, per capita
        world (if (pos? population)
                (reduce (fn [memo sim-id]
                          (update-sim-savings memo sim-id (/ (- interest-delta-total) population)))
                        world
                        (keys (:world/sims world)))
                world)

        ;; final balances
        final-resident-balances (->> (:world/residents world)
                                     (map (fn [[resident-id resident]]
                                            [resident-id (:resident/money-balance resident)]))
                                     (into {}))
        final-resident-balance (->> final-resident-balances
                                    vals
                                    (reduce + 0))
        final-sim-savings (->> (:world/sims world)
                               vals
                               (map :sim/savings)
                               (reduce + 0.0))
        final-net-money-balance (+ new-government-balance
                                   final-sim-savings
                                   final-resident-balance)
        cash-ratio-after (if (zero? final-net-money-balance)
                           0
                           (/ final-resident-balance final-net-money-balance))

        ;; JOY
        joy (->> (:world/sims world)
                 vals
                 (map (fn [sim]
                        (- 1 (mean-stress sim))))
                 (reduce + 0.0))

        public-stats
        ;; transit is struggling with bignums(?)
        ;; for now, just cast all to double
        (x/transform
          (x/walker number?)
          double
          {:sim.out/shift (:world/shift world)
           :sim.out/population population
           :sim.out/deaths (count dead-sim-ids)
           :sim.out/total-sim-savings final-sim-savings
           :sim.out/mean-physical-stress (if (pos? population)
                                           (/ (->> (:world/sims world)
                                                   vals
                                                   (map :sim/physical-stress)
                                                   (reduce + 0.0))
                                              population)
                                           0)
           :sim.out/mean-mental-stress (if (pos? population)
                                         (/ (->> (:world/sims world)
                                                 vals
                                                 (map :sim/mental-stress)
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
        ;; sims
        (for [sim (vals (:world/sims world))]
          (update sim :sim/savings double))
        (for [sim-id dead-sim-ids]
          [:db/retractEntity [:sim/id sim-id]])
        ;; residents
        (mapcat (fn [[resident-id resident]]
                  (concat
                    [[:db/add [:resident/id resident-id]
                      :resident/money-balance (long (:resident/money-balance resident))]
                     [:db/add [:resident/id resident-id]
                      :resident/private-stats
                      {:stats.private/net-cashflow (double
                                                     (- (:resident/money-balance resident)
                                                        (get-in world [:world/initial-resident-balances resident-id] 0)))
                       :stats.private/stabilization-payment (double (get resident-interest-deltas resident-id 0))}]]
                    (stock-txs :resident/id resident-id
                               :resident/stocks (:resident/stocks resident))))
                (:world/residents world))
        ;; improvement stocks
        (mapcat (fn [[improvement-id improvement]]
                  (stock-txs :improvement/id improvement-id
                             :improvement/stocks (:improvement/stocks improvement)))
                (:world/improvements world))
        ;; offer utilization
        (for [[offer-id utilization] (:world/utilizations world)]
          [:db/add [:offer/id offer-id] :offer/utilization utilization])
        loan-txs
        (resident-bankruptcy-txs final-resident-balances)))
    ;; births & immigration
    (dotimes [_ (randomize population constants/birth-chance-per-sim-per-tick)]
      (db/add-sim! island-id (citizen/random ::schema/generator-baby)))
    (when (< (rand) constants/sim-immigration-chance)
      (db/add-sim! island-id (citizen/random ::schema/generator-immigrant)))))
