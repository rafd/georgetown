(ns georgetown.loop
  (:require
    [com.rpl.specter :as x]
    [chime.core :as chime]
    [georgetown.schema :as schema]
    [georgetown.db :as db]
    [georgetown.market :as m])
  (:import
    [java.time Instant Duration]))

(defn extract-data-for-simulation
  [island-id]
  (let [island (db/q '[:find (pull ?island [:island/population
                                            :island/epoch
                                            :island/citizen-food-balance
                                            :island/citizen-money-balance]) .
                       :in $ ?island-id
                       :where
                       [?island :island/id ?island-id]]
                     island-id)
        resident-money-balances (->> (db/q '[:find ?resident-id ?money-balance
                                             :in $ ?island-id
                                             :where
                                             [?island :island/id ?island-id]
                                             [?island :island/residents ?resident]
                                             [?resident :resident/money-balance ?money-balance]
                                             [?resident :resident/id ?resident-id]]
                                           island-id)
                                     (into {}))]
    {:sim.in/population (:island/population island)
     :sim.in/epoch (:island/epoch island)
     :sim.in/citizen-money-balance (:island/citizen-money-balance island)
     :sim.in/citizen-food-balance (:island/citizen-food-balance island)
     :sim.in/tenders
     (->> (db/q '[:find (pull ?improvement [*]) ?resident-id
                  :in $ ?island-id
                  :where
                  [?island :island/id ?island-id]
                  [?island :island/lots ?lot]
                  [?lot :lot/improvement ?improvement]
                  [?lot :lot/deed ?deed]
                  [?resident :resident/deeds ?deed]
                  [?resident :resident/id ?resident-id]]
                island-id)
          (mapcat (fn [[improvement resident-id]]
                    ;; scale non-prerequisite offer amounts
                    ;; based on prerequisite offer utilization
                    (let [grouped-offers (->> (:improvement/offers improvement)
                                              ;; ignore offers that don't have an amount set
                                              ;; (newly created improvements)
                                              (filter :offer/amount)
                                              (group-by (fn [offer]
                                                          (if (:offerable/prerequisite?
                                                                (schema/offerables
                                                                  (:offer/type offer)))
                                                            ::prereq
                                                            ::no-prereq))))
                          has-prerequisites? (->> (:improvement/type improvement)
                                                  schema/blueprints
                                                  :blueprint/offerables
                                                  (some :offerable/prerequisite?))
                          overall-prerequisite-utilization (if has-prerequisites?
                                                             (->> (grouped-offers true)
                                                                  (map :offer/utilization)
                                                                  (filter some?) ;; when starting out, prereqs are nil
                                                                  (apply min 1))
                                                             1)]
                      (->> (concat (grouped-offers ::prereq)
                                   (->> (grouped-offers ::no-prereq)
                                        (map (fn [offer]
                                               (-> offer
                                                   (assoc ::adjusted-utilization overall-prerequisite-utilization))))))
                           (map (fn [offer]
                                  (let [offerable (schema/offerables (:offer/type offer))
                                        adjusted-utilization (or (::adjusted-utilization offer) 1)]
                                    {:tender/resident-id resident-id
                                     :tender/offer-id (:offer/id offer)
                                     :tender/improvement-id (:improvement/id improvement)
                                     :tender/prerequisite-utilization adjusted-utilization
                                     :tender/supply [(:offerable/supply-unit offerable)
                                                     (* adjusted-utilization
                                                        (or (:offerable/supply-amount offerable)
                                                            (:offer/amount offer)))]
                                     :tender/demand [(:offerable/demand-unit offerable)
                                                     (* adjusted-utilization
                                                        (or (:offerable/demand-amount offerable)
                                                            (:offer/amount offer)))]})))))))
          ;; if resident doesn't have enough money to pay for all their labour tenders
          ;; scale them down by the ratio of their money to the total labour cost (potentially 0)
          (group-by :tender/resident-id)
          (mapcat (fn [[resident-id tenders]]
                    (let [total-labour-cost (->> tenders
                                                 (keep (fn [tender]
                                                         (when (= :resource/money (get-in tender [:tender/supply 0]))
                                                           (get-in tender [:tender/supply 1]))))
                                                 (reduce +))]
                      (if (zero? total-labour-cost)
                        tenders
                        (if (<= total-labour-cost (get resident-money-balances resident-id))
                            tenders
                            (let [labour-improvements (->> tenders
                                                           (keep (fn [tender]
                                                                   (when (= :resource/money (get-in tender [:tender/supply 0]))
                                                                     (:tender/improvement-id tender))))
                                                           set)
                                  ratio (/ (max 0 ;; shouldn't be possible for the balance to be < 0, but it happens
                                                (get resident-money-balances resident-id))
                                           total-labour-cost)]
                              (->> tenders
                                   (map (fn [tender]
                                          (if (contains? labour-improvements (:tender/improvement-id tender))
                                            (-> tender
                                                (update :tender/prerequisite-utilization * ratio)
                                                (update-in [:tender/supply 1] * ratio)
                                                (update-in [:tender/demand 1] * ratio))
                                            tender)))))))))))}))

#_(tick-all!)

;; 1 tick ~= 1 day
(def ticks-of-money-savings 5)
(def ticks-of-food-savings 5)
(def food-demand-per-person-per-tick 1) ;; 1 food ~= meals
(def shelter-demand-per-person-per-tick 1) ;; 1 shelter ~= 1 week of rent
(def max-labour-supply-per-person-per-tick 15) ;; 1 labour ~= 1 hour

(defn simulate
  [{:sim.in/keys [population tenders citizen-money-balance citizen-food-balance]}]
  (let [
        ;; FOOD
        base-food-demand (* food-demand-per-person-per-tick population)
        food-savings-goal (* ticks-of-food-savings base-food-demand)
        food-demand (max (- food-savings-goal citizen-food-balance)
                         ;; HACK: force a minimum, so there's always a food market
                         base-food-demand)
        {food-market-clearing-price :market/clearing-unit-price
         food-supplied :market/amount-supplied
         food-cost :market/total-cost
         food-tenders :market/tenders}
        (m/market :resource/food food-demand :resource/money tenders)

        ;; SHELTER
        base-shelter-demand (* shelter-demand-per-person-per-tick population)
        ;; can't "store" shelter, so demand = base-demand
        shelter-demand base-shelter-demand
        {shelter-market-clearing-price :market/clearing-unit-price
         shelter-supplied :market/amount-supplied
         shelter-cost :market/total-cost
         shelter-tenders :market/tenders}
        (m/market :resource/shelter shelter-demand :resource/money tenders)

        ;; MONEY
        ;; citizens will work to save up shelter and food
        money-savings-goal (Math/ceil (* ticks-of-money-savings (+ shelter-cost food-cost)))
        money-demand (max (- money-savings-goal citizen-money-balance)
                          ;; HACK: force a minimum
                          ;; b/c sims "don't realize that food supply requires labor"
                          (if food-market-clearing-price
                            (+ (* shelter-market-clearing-price base-shelter-demand)
                               (* food-market-clearing-price base-food-demand))
                            1000))
        potential-money-supply (->> tenders
                                    (keep
                                      (fn [tender]
                                        (let [[resource amount] (:tender/supply tender)]
                                          (when (= resource :resource/money)
                                            amount))))
                                    (apply +))
        {money-market-clearing-price :market/clearing-unit-price
         money-supplied :market/amount-supplied
         money-cost :market/total-cost
         money-tenders :market/tenders}
        (m/market :resource/money money-demand :resource/labour tenders)
        money-cost (Math/ceil money-cost) ;; in terms of labour hours

        ;; LABOUR
        potential-labour-demand (->> tenders
                                     (keep
                                       (fn [tender]
                                         (let [[resource amount] (:tender/demand tender)]
                                           (when (= resource :resource/labour)
                                             amount))))
                                     (apply +))
        potential-labour-supply (* max-labour-supply-per-person-per-tick population)
        labour-supplied (min potential-labour-supply
                             money-cost)

        ;; LEISURE
        leisure-hours (- potential-labour-supply
                         labour-supplied)
        leisure-percent (/ leisure-hours
                           potential-labour-supply)

        ;; CITIZEN BALANCES
        new-citizen-money-balance (+ citizen-money-balance
                                     (- food-cost)
                                     (- shelter-cost)
                                     ;; citizens dividend added elsewhere
                                     (+ money-supplied))
        food-consumed (* food-demand-per-person-per-tick population)
        new-citizen-food-balance (+ citizen-food-balance
                                    (+ food-supplied)
                                    (- food-consumed))

        ;; POPULATION
        potential-food-supply (->> tenders
                                   (keep
                                     (fn [tender]
                                       (let [[resource amount] (:tender/supply tender)]
                                         (when (= resource :resource/food)
                                           amount))))
                                   (apply +))
        potential-shelter-supply (->> tenders
                                      (keep
                                        (fn [tender]
                                          (let [[resource amount] (:tender/supply tender)]
                                            (when (= resource :resource/shelter)
                                              amount))))
                                      (apply +))
        potential-supported-population (Math/floor
                                         (min (/ potential-food-supply
                                                 food-demand-per-person-per-tick)
                                              (/ potential-shelter-supply
                                                 shelter-demand-per-person-per-tick)))
        supported-population (Math/floor
                               (min (/ food-supplied
                                       food-demand-per-person-per-tick)
                                    (/ shelter-supplied
                                       shelter-demand-per-person-per-tick)))
        ;; emigrating citizens take a fraction of the money with them
        #_#_citizen-money-balance (* (- 1 (/ population-emigration population))
                                     citizen-money-balance)

        randomize (fn [population odds]
                    (->> (repeatedly (fn []
                                       (< (rand) odds)))
                         (take population)
                         (filter true?)
                         count))
        emigration-count (randomize (- population supported-population)
                                    0.5)
        death-count (randomize supported-population
                               (/ (- 1 leisure-percent)
                                  100))
        newcomer-count (randomize (max (- potential-supported-population population) 0)
                                  (/ leisure-percent
                                     40))
        ;; don't let population go to 0, or there's no going back
        new-population (max 1
                            (+ population
                               (- emigration-count)
                               (- death-count)
                               newcomer-count))]
    ;; transit is struggling with bignums(?)
    ;; for now, just cast all to double
    (x/transform
      (x/walker number?)
      double
      {:sim.out/max-supported-population potential-supported-population
       :sim.out/citizen-money-balance new-citizen-money-balance
       :sim.out/money-savings-goal money-savings-goal
       :sim.out/citizen-food-balance new-citizen-food-balance
       :sim.out/food-savings-goal food-savings-goal
       :sim.out/resources
       {:resource/shelter {:demand shelter-demand
                           :available-supply potential-shelter-supply
                           :supply shelter-supplied
                           :clearing-price shelter-market-clearing-price
                           :cost shelter-cost
                           :tenders shelter-tenders}
        :resource/food {:demand food-demand
                        :available-supply potential-food-supply
                        :supply food-supplied
                        :clearing-price food-market-clearing-price
                        :cost food-cost
                        :tenders food-tenders}
        :resource/money {:demand money-demand
                         :available-supply potential-money-supply
                         :supply money-supplied
                         :clearing-price money-market-clearing-price
                         :cost money-cost
                         :tenders money-tenders}
        :resource/labour {:demand potential-labour-demand
                          :available-supply potential-labour-supply
                          :supply labour-supplied
                          :clearing-price nil}}
       :sim.out/leisure-percent leisure-percent
       :sim.out/population new-population})))

(defn resident-market-net-amounts
  "For each resident, +/- for each resource, from buying/selling on markets"
  [sim-out]
  (->> sim-out
       (x/select
         [:sim.out/resources
          x/MAP-VALS
          :tenders
          x/ALL])
       (mapcat (fn [tender]
                 ;; tender might only have been partially filled
                 ;; these are output tenders, so they have already been adjusted by utilization
                 [[(:tender/resident-id tender)
                   (first (:tender/demand tender))
                   (* (second (:tender/demand tender))
                      (:tender/fill-ratio tender))]
                  [(:tender/resident-id tender)
                   (first (:tender/supply tender))
                   (* (- (second (:tender/supply tender)))
                      (:tender/fill-ratio tender))]]))
       ;; for now, only care about money
       (filter (fn [[_ resource _]]
                 (= resource :resource/money)))
       (reduce (fn [memo [resident-id resource amount]]
                 (update-in memo [resident-id resource] (fnil + 0) amount))
               {})))

#_(defn resident-operations-net-amounts
  "For each resident, +/- for each resource, from running their improvements"
  [island-id]
  (->> (db/q
         ;; need improvement-id so that it doesn't dedupe
         '[:find ?resident-id ?improvement-type ?improvement-id
           :in $ ?island-id
           :where
           [?island :island/id ?island-id]
           [?island :island/residents ?resident]
           [?resident :resident/id ?resident-id]
           [?resident :resident/deeds ?deed]
           [?lot :lot/deed ?deed]
           [?lot :lot/improvement ?improvement]
           [?improvement :improvement/type ?improvement-type]
           [?improvement :improvement/id ?improvement-id]]
         island-id)
       (mapcat (fn [[owner-id improvement-type _]]
                 (->> (schema/blueprints improvement-type)
                      :blueprint/io
                      (map (fn [io]
                             [owner-id io])))))
       (reduce (fn [memo [owner-id {:io/keys [direction resource amount]}]]
                 (update-in memo [owner-id resource]
                            (fnil
                              (case direction
                                :io.direction/input -
                                :io.direction/output +)
                              0)
                            (* improvement-utilization amount)))
               {})))

#_(resident-operations-net-amounts
    #uuid "0191a53b-28d2-7445-9b82-019b57ef9800")

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
                 (update-in memo [owner-id :resource/money] (fnil + 0) (- rate)))
               {})))

(defn resident-resource-balances
  [island-id]
  (->> (db/q '[:find ?resident-id ?money-balance
               :in $ ?island-id
               :where
               [?island :island/id ?island-id]
               [?island :island/residents ?resident]
               [?resident :resident/money-balance ?money-balance]
               [?resident :resident/id ?resident-id]]
             island-id)
       (map (fn [[resident-id money-amount]]
              [resident-id
               {:resource/money money-amount}]))
       (into {})))

(defn tick!
  [island-id]
  (let [sim-in (extract-data-for-simulation island-id)
        sim-out (simulate sim-in)
        ;; joy
        joy (* (:sim.out/leisure-percent sim-out)
               (:sim.out/population sim-out))
        ;; resident money
        resident-balances (resident-resource-balances island-id)
        resident-market-amounts (resident-market-net-amounts sim-out)
        resident-operations-amounts {} #_(resident-operations-net-amounts island-id)
        resident-taxes (taxes island-id)
        new-resident-balances (merge-with (partial merge-with (fnil + 0))
                                          resident-balances
                                          resident-market-amounts
                                          resident-operations-amounts
                                          resident-taxes)
        ;; GOVERNMENT
        government-money-balance (db/q '[:find ?balance .
                                         :in $ ?island-id
                                         :where
                                         [?island :island/id ?island-id]
                                         [?island :island/government-money-balance ?balance]]
                                       island-id)
        ;; government redistributes 50% of tax revenues as citizens dividend
        citizens-dividend  (Math/ceil (* 0.5 government-money-balance))
        government-expenses (- citizens-dividend)
        government-revenues (- (apply + (map :resource/money (vals resident-taxes))))
        new-government-balance (+ government-money-balance
                                  government-revenues
                                  government-expenses)
        ;; get improvement offer utilization (based on the tenders)
        offer-id->utilization (->> [:resource/shelter :resource/food :resource/money]
                                   (mapcat (fn [resource]
                                             (->> sim-out
                                                  :sim.out/resources
                                                  resource
                                                  :tenders
                                                  (map (fn [tender]
                                                         [(:tender/offer-id tender)
                                                          (* (:tender/prerequisite-utilization tender)
                                                             (:tender/fill-ratio tender))])))))
                                   (into {}))]
    (db/transact!
      (concat
        (for [[k v] {:island/simulator-stats
                     (assoc sim-out
                       ;; include these also, so front-end reports them
                       :sim.out/government-money-balance new-government-balance)
                     :island/population (:sim.out/population sim-out)
                     :island/joy joy
                     :island/epoch (inc (:sim.in/epoch sim-in))
                     :island/government-money-balance new-government-balance
                     :island/citizen-money-balance (+ (:sim.out/citizen-money-balance sim-out)
                                                      citizens-dividend)
                     :island/citizen-food-balance (:sim.out/citizen-food-balance sim-out)}]
          [:db/add [:island/id island-id] k v])
        (for [[resident-id balance] new-resident-balances]
          [:db/add [:resident/id resident-id]
           :resident/money-balance (:resource/money balance)])
        (for [[offer-id utilization] offer-id->utilization]
          [:db/add [:offer/id offer-id] :offer/utilization utilization])))))

(defn tick-all! []
  (doseq [island-id (db/q '[:find [?island-id ...]
                            :where
                            [?island :island/id ?island-id]])]
    (tick! island-id)))

#_(tick-all!)

#_(balances (:island/id (first (s/all-of-type :island/id '[:island/id]))))

(defonce scheduler (atom nil))

(defn initialize!
  []
  (when @scheduler
    (.close @scheduler))
  (reset! scheduler
            (chime/chime-at
              (chime/periodic-seq (Instant/now)
                                  (Duration/ofSeconds 1))
              (fn [time]
                (tick-all!))
              {:on-finished (fn []
                              (tap> "Schedule finished."))}))
  nil)

#_(initialize!)
#_(.close @scheduler)
