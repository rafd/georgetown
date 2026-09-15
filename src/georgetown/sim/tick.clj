(ns georgetown.sim.tick
  (:require
    [bloom.commons.uuid :as uuid]
    [com.rpl.specter :as x]
    [georgetown.server.db :as db]
    [georgetown.server.events :as events]
    [georgetown.sim.allocate :as allocate]
    [georgetown.sim.blueprints :as blueprints]
    [georgetown.sim.citizen :as citizen]
    [georgetown.sim.constants :as constants]
    [georgetown.sim.debt :as debt]
    [georgetown.sim.engine :as engine]
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
                                          :tender/labour-per-unit labour-needed
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
  "Aggregate market for food or shelter (every tick).
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
                                (update-improvement-stock (:tender/improvement-id tender) :resource/labour -
                                                          (* fill-amount (:tender/labour-per-unit tender 0.0)))))
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
                   :unserved-count (count unserved-citizens)})
        (assoc-in [:world/stats (case resource
                                  :resource/food :hungry-citizen-ids
                                  :resource/shelter :unhoused-citizen-ids)]
                  (set unserved-citizens)))))

;; ---- work & leisure allocation ----

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
                                     (get citizen* (blueprints/skill->talent skill))
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
        allocations (allocate/allocate-shift
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
                :citizen-activities (->> allocations
                                         (map (fn [[citizen-id offer-id]]
                                                [citizen-id
                                                 (if offer-id
                                                   (:offer/type (offers-by-id offer-id))
                                                   :activity/idle)]))
                                         (into {}))
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
            (keys blueprints/skill->talent))))

(defn run-citizen-maintenance
  [world]
  (update world :world/citizens
          (fn [citizens]
            (->> citizens
                 (map (fn [[citizen-id citizen]]
                        [citizen-id (-> citizen
                                    (update :citizen/age-ticks inc)
                                    (update :citizen/residency-ticks inc)
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

(defn emigration-chance
  "Very stressed citizens are likely to leave the island."
  [citizen]
  (* constants/max-emigration-chance
     (Math/pow (mean-stress citizen) 4)))

(defn pick-emigrant-citizen-ids [world]
  (->> (:world/citizens world)
       vals
       (filter (fn [citizen]
                 (< (rand) (emigration-chance citizen))))
       (map :citizen/id)))

(defn randomize [n odds]
  (->> (repeatedly (fn [] (< (rand) odds)))
       (take n)
       (filter true?)
       count))


;; ---- money rules ----
;; run by georgetown.sim.engine; each rule sees only its declared inputs
;; and must return exactly its declared outputs

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
                (update-player-money memo player-id amount))
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
                  (update-citizen-savings memo citizen-id per-citizen-dividend))
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
                        (update-player-money memo player-id delta))
                      world
                      player-interest-deltas)
        ;; players' interest is paid by (or paid to) the citizens, per capita
        world (if (pos? population)
                (reduce (fn [memo citizen-id]
                          (update-citizen-savings memo citizen-id
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

(def money-rules
  [#'loan-payments
   #'deed-taxes
   #'apply-player-charges
   #'citizens-dividend
   #'demurrage
   #'final-balances
   #'bankruptcies])

#_(engine/plan money-rules)

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
        world (-> world
                  (run-goods-market :resource/food)
                  (run-goods-market :resource/shelter)
                  run-allocation
                  run-citizen-maintenance)
        new-epoch (inc (:world/epoch world))
        dead-citizen-ids (set (pick-dead-citizen-ids world))
        dead-citizens (map (:world/citizens world) dead-citizen-ids)
        world (update world :world/citizens
                      (fn [citizens]
                        (apply dissoc citizens dead-citizen-ids)))
        emigrant-citizen-ids (set (pick-emigrant-citizen-ids world))
        emigrant-citizens (map (:world/citizens world) emigrant-citizen-ids)
        world (update world :world/citizens
                      (fn [citizens]
                        (apply dissoc citizens emigrant-citizen-ids)))
        population (count (:world/citizens world))

        ;; money flows (loans, taxes, dividend, demurrage, bankruptcies)
        world (engine/run money-rules world)

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
          {:sim.out/epoch (:world/epoch world)
           :sim.out/shift (:world/shift world)
           :sim.out/population population
           :sim.out/deaths (count dead-citizen-ids)
           :sim.out/emigrations (count emigrant-citizen-ids)
           :sim.out/total-citizen-savings (:world/final-citizen-savings world)
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
           :sim.out/citizen-states
           (let [{:keys [hungry-citizen-ids unhoused-citizen-ids citizen-activities]} (:world/stats world)]
             (->> (:world/citizens world)
                  (map (fn [[citizen-id citizen]]
                         [citizen-id
                          {:citizen-state/hungry? (contains? hungry-citizen-ids citizen-id)
                           :citizen-state/unhoused? (contains? unhoused-citizen-ids citizen-id)
                           :citizen-state/last-activity (get citizen-activities citizen-id)
                           :citizen-state/savings (:citizen/savings citizen)}]))
                  (into {})))
           :sim.out/resources (-> (:world/stats world)
                                  (select-keys [:resource/food :resource/shelter]))
           :sim.out/joy joy
           :sim.out/net-money-balance (:world/final-net-money-balance world)
           :sim.out/player-money-balance (:world/final-player-balance-total world)
           :sim.out/government-money-balance (:world/government-money-balance world)
           :sim.out/helicopter-money (:world/helicopter-money world)
           :sim.out/cash-ratio-before (:world/cash-ratio-before world)
           :sim.out/cash-ratio-after (:world/cash-ratio-after world)
           :sim.out/stabilization-rate (:world/interest-rate world)})]
    (db/transact!
      (concat
        (for [[k v] {:island/public-stats public-stats
                     :island/joy (long joy)
                     :island/epoch new-epoch
                     :island/government-money-balance (long (:world/government-money-balance world))}]
          [:db/add [:island/id island-id] k v])
        ;; citizens
        (for [citizen (vals (:world/citizens world))]
          (update citizen :citizen/savings double))
        (for [citizen-id (into dead-citizen-ids emigrant-citizen-ids)]
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
        ;; (before bankruptcy txs, so [:player/id ...] lookup refs still resolve)
        (->> dead-citizens
             (mapcat (fn [citizen]
                       (events/event-txs island-id
                                         {:event/type :event.type/citizen-died
                                          :event/source :source/simulation
                                          :event/epoch new-epoch
                                          :event/data {:citizen-id (:citizen/id citizen)
                                                       :age-years (int (citizen/age-in-years citizen))}}))))
        (->> emigrant-citizens
             (mapcat (fn [citizen]
                       (events/event-txs island-id
                                         {:event/type :event.type/citizen-emigrated
                                          :event/source :source/simulation
                                          :event/epoch new-epoch
                                          :event/data {:citizen-id (:citizen/id citizen)
                                                       :age-years (int (citizen/age-in-years citizen))}}))))
        (->> (:world/events world)
             (mapcat (fn [event]
                       (events/event-txs island-id
                                         (assoc event :event/epoch new-epoch)))))
        (events/prune-event-txs island-id (- new-epoch constants/event-retention-ticks))
        (:world/txs world)))
    ;; births & immigration
    (dotimes [_ (randomize population constants/birth-chance-per-citizen-per-tick)]
      (let [citizen (citizen/random ::schema/generator-baby)]
        (db/transact!
          (concat
            [{:island/id island-id
              :island/citizens [citizen]}]
            (events/event-txs island-id
                              {:event/type :event.type/citizen-born
                               :event/source :source/simulation
                               :event/epoch new-epoch
                               :event/data {:citizen-id (:citizen/id citizen)
                                            :citizen-name (:citizen/name citizen)}})))))
    (when (< (rand) constants/citizen-immigration-chance)
      (let [citizen (citizen/random ::schema/generator-immigrant)]
        (db/transact!
          (concat
            [{:island/id island-id
              :island/citizens [citizen]}]
            (events/event-txs island-id
                              {:event/type :event.type/citizen-immigrated
                               :event/source :source/simulation
                               :event/epoch new-epoch
                               :event/data {:citizen-id (:citizen/id citizen)
                                            :citizen-name (:citizen/name citizen)}})))))))
