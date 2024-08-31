(ns georgetown.loop
  (:require
    [com.rpl.specter :as x]
    [chime.core :as chime]
    [georgetown.state :as s]
    [georgetown.schema :as schema]
    [georgetown.db :as db]
    [georgetown.market :as m])
  (:import
    [java.time Instant Duration]))

(defn extract-data-for-simulation
  [island-id]
  (let [island (db/q '[:find (pull ?island [:island/population
                                            :island/citizen-money-balance]) .
                       :in $ ?island-id
                       :where
                       [?island :island/id ?island-id]]
                     island-id)]
    {:sim.in/population (:island/population island)
     :sim.in/citizen-money-balance (:island/citizen-money-balance island)
     :sim.in/tenders
     (->> (db/q '[:find [(pull ?offer [*
                                       {:improvement/_offers [:improvement/active?]}]) ...]
                  :in $ ?island-id
                  :where
                  [?island :island/id ?island-id]
                  [?island :island/lots ?lot]
                  [?lot :lot/improvement ?improvement]
                  [?improvement :improvement/offers ?offer]]
                island-id)
          (filter :offer/amount)
          (map (fn [offer]
                 (assoc offer
                   ::resident-id
                   (db/q '[:find ?resident-id .
                           :in $ ?offer-id
                           :where
                           [?offer :offer/id ?offer-id]
                           [?improvement :improvement/offers ?offer]
                           [?lot :lot/improvement ?improvement]
                           [?lot :lot/deed ?deed]
                           [?resident :resident/deeds ?deed]
                           [?resident :resident/id ?resident-id]]
                         (:offer/id offer)))))
          (map (fn [offer]
                 (let [offerable (schema/offerables (:offer/type offer))]
                   {:tender/resident-id (::resident-id offer)
                    ;; only keep offers where the improvement is active?
                    ;; (except ones that are prerequisite)
                    :tender/active? (or (:offerable/prerequisite? (schema/offerables (:offer/type offer)))
                                        (-> offer :improvement/_offers :improvement/active?))
                    :tender/offer-id (:offer/id offer)
                    :tender/supply [(:offerable/supply-unit offerable)
                                    (or (:offerable/supply-amount offerable)
                                        (:offer/amount offer))]
                    :tender/demand [(:offerable/demand-unit offerable)
                                    (or (:offerable/demand-amount offerable)
                                        (:offer/amount offer))]}))))}))

#_(tick-all!)

;; 1 tick ~= 1 week
(def ticks-of-savings 20)
(def food-demand-per-person-per-week 21) ;; 1 food ~= meals
(def shelter-demand-per-person-per-week 1) ;; 1 shelter ~= 1 week of rent
(def max-labour-supply-per-person-per-week (* 7 15)) ;; 1 labour ~= 1 hour

(defn simulate
  [{:sim.in/keys [population tenders citizen-money-balance]}]
  (let [

        ;; POPULATION
        ;; if not enough shelter or food, population decreases to match
        ;; do this before the markets are run
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
        ;; some food supply didn't have enough labour last turn
        active-food-supply (->> tenders
                                (filter :tender/active?)
                                (keep
                                  (fn [tender]
                                    (let [[resource amount] (:tender/supply tender)]
                                      (when (= resource :resource/food)
                                        amount))))
                                (apply +))
        potential-supported-population (Math/floor
                                         (min (/ potential-food-supply
                                                 food-demand-per-person-per-week)
                                              (/ potential-shelter-supply
                                                 shelter-demand-per-person-per-week)))
        supported-population (Math/floor
                               (min (/ active-food-supply
                                       food-demand-per-person-per-week)
                                    (/ potential-shelter-supply
                                       shelter-demand-per-person-per-week)))
        population-emigration (if (< population supported-population)
                                0
                                (- population supported-population))
        ;; emigrating citizens take a fraction of the money with them
        citizen-money-balance (* (- 1 (/ population-emigration population))
                                 citizen-money-balance)
        population (- population population-emigration)
        ;; don't let population go to 0, or there's no going back
        ;; (also, code below explodes)
        population (max 1 population)

        ;; FOOD
        food-demand (* food-demand-per-person-per-week population)
        {food-market-clearing-price :market/clearing-unit-price
         food-supplied :market/amount-supplied
         food-cost :market/total-cost
         food-successful-tenders :market/successful-tenders}
        (m/market :resource/food food-demand :resource/money (filter :tender/active? tenders))

        ;; SHELTER
        shelter-demand (* shelter-demand-per-person-per-week population)
        {shelter-market-clearing-price :market/clearing-unit-price
         shelter-supplied :market/amount-supplied
         shelter-cost :market/total-cost
         shelter-successful-tenders :market/successful-tenders}
        (m/market :resource/shelter shelter-demand :resource/money (filter :tender/active? tenders))

        ;; MONEY
        ;; citizens will work to save up shelter and food
        savings-goal (Math/ceil (* ticks-of-savings (+ shelter-cost food-cost)))
        money-demand (max 0 (- savings-goal citizen-money-balance))
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
         money-successful-tenders :market/successful-tenders}
        (m/market :resource/money money-demand :resource/labour (filter :tender/active? tenders))
        money-cost (Math/ceil money-cost) ;; in terms of labour hours

        ;; LABOUR
        potential-labour-demand (->> tenders
                                     (keep
                                       (fn [tender]
                                         (let [[resource amount] (:tender/demand tender)]
                                           (when (= resource :resource/labour)
                                             amount))))
                                     (apply +))
        potential-labour-supply (* max-labour-supply-per-person-per-week population)
        labour-supplied (min potential-labour-supply
                             money-cost)

        ;; LEISURE
        leisure-percent (/ (- potential-labour-supply
                              labour-supplied)
                           potential-labour-supply)

        ;; POPULATION
        new-citizen-money-balance (+ citizen-money-balance
                                     (- food-cost)
                                     (- shelter-cost)
                                     (+ money-supplied))

        ;; deaths
        population-decrease (->> (repeatedly (fn []
                                               (< (rand) (/ (- 1 leisure-percent)
                                                            ;; divide, just to slow things down
                                                            100))))
                                 (take population)
                                 (filter true?)
                                 count
                                 -)
        ;; for every potential person, there is a leisure%/c% chance they will join
        potential-population-increase (max (- supported-population population) 0)
        population-increase (if (pos? new-citizen-money-balance)
                              (->> (repeatedly (fn []
                                               (< (rand) (/ leisure-percent
                                                            ;; divide, just to slow things down
                                                            40))))
                                 (take potential-population-increase)
                                 (filter true?)
                                 count)
                              0)
        new-population (+ population
                          population-decrease
                          population-increase)]
    ;; transit is struggling with bignums(?)
    ;; for now, just cast all to double
    (x/transform
      (x/walker number?)
      double
      {:sim.out/max-supported-population potential-supported-population
       :sim.out/citizen-money-balance citizen-money-balance
       :sim.out/resources
       {:resource/shelter {:demand shelter-demand
                           :available-supply potential-shelter-supply
                           :supply shelter-supplied
                           :clearing-price shelter-market-clearing-price
                           :cost shelter-cost
                           :tenders
                           (->> tenders
                                (filter
                                  (fn [tender]
                                    (= (first (:tender/supply tender)) :resource/shelter))))
                           :successful-tenders shelter-successful-tenders}
        :resource/food {:demand food-demand
                        :available-supply potential-food-supply
                        :supply food-supplied
                        :clearing-price food-market-clearing-price
                        :cost food-cost
                        :tenders
                        (->> tenders
                             (filter
                               (fn [tender]
                                 (= (first (:tender/supply tender)) :resource/food))))
                        :successful-tenders food-successful-tenders}
        :resource/money {:demand money-demand
                         :available-supply potential-money-supply
                         :supply money-supplied
                         :clearing-price money-market-clearing-price
                         :cost money-cost
                         :tenders
                         (->> tenders
                              (filter
                                (fn [tender]
                                  (= (first (:tender/supply tender)) :resource/money))))
                         :successful-tenders money-successful-tenders}
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
          :successful-tenders
          x/ALL])
       (mapcat (fn [tender]
                 [[(:tender/resident-id tender)
                   (first (:tender/demand tender))
                   (second (:tender/demand tender))]
                  [(:tender/resident-id tender)
                   (first (:tender/supply tender))
                   (- (second (:tender/supply tender)))]]))
       (reduce (fn [memo [resident-id resource amount]]
                 (update-in memo [resident-id resource] (fnil + 0) amount))
               {})))

(defn resident-operations-net-amounts
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
           [?improvement :improvement/active? true]
           [?improvement :improvement/type ?improvement-type]
           [?improvement :improvement/id ?improvement-id]]
         island-id)
       (mapcat (fn [[owner-id improvement-type _]]
                 (->> (schema/blueprints improvement-type)
                      :blueprint/io
                      (map (fn [io]
                             [owner-id io])))))
       (reduce (fn [memo [owner-id {:io/keys [direction resource amount]}]]
                 (update-in memo [owner-id resource] (fnil
                                                    (case direction
                                                      :io.direction/input -
                                                      :io.direction/output +)
                                                    0) amount))
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
  (let [result (simulate (extract-data-for-simulation island-id))
        ;; resident money
        resident-balances (resident-resource-balances island-id)
        resident-market-amounts (resident-market-net-amounts result)
        resident-operations-amounts (resident-operations-net-amounts island-id)
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
        ;; citizen money
        citizen-money-balance (db/q '[:find ?balance .
                                      :in $ ?island-id
                                      :where
                                      [?island :island/id ?island-id]
                                      [?island :island/citizen-money-balance ?balance]]
                                    island-id)
        citizen-expenses (- 0
                            (-> result :sim.out/resources :resource/food :cost)
                            (-> result :sim.out/resources :resource/shelter :cost))
        citizen-revenues (-> result :sim.out/resources :resource/money :supply)
        new-citizen-money-balance (+ citizen-money-balance
                                     citizens-dividend
                                     citizen-revenues
                                     citizen-expenses)

        ;; activate/de-activate improvements
        ;; for each improvement, check if prerequisite offers were successful
        improvement-types-with-prerequisites (->> schema/blueprints
                                                  vals
                                                  (filter (fn [blueprint]
                                                            (some :offerable/prerequisite? (:blueprint/offerables blueprint))))
                                                  (map :blueprint/id))
        improvements-with-prerequisites (db/q '[:find [(pull ?improvement [*]) ...]
                                                :in $ ?island-id ?improvement-types
                                                :where
                                                [(identity ?improvement-types) [?improvement-type ...]]
                                                [?island :island/id ?island-id]
                                                [?island :island/lots ?lot]
                                                [?lot :lot/improvement ?improvement]
                                                [?improvement :improvement/type ?improvement-type]]
                                              island-id
                                              improvement-types-with-prerequisites)
        successful-offers (->> [:resource/shelter :resource/food :resource/money]
                              (mapcat (fn [resource]
                                        (->> result :sim.out/resources resource :successful-tenders (map :tender/offer-id))))
                              set)
        improvements-grouped-by-status (->> improvements-with-prerequisites
                                            (group-by (fn [improvement]
                                                        (if (->> improvement
                                                                 :improvement/offers
                                                                 (filter (fn [offer]
                                                                           (:offerable/prerequisite? (schema/offerables (:offer/type offer)))))
                                                                 (map :offer/id)
                                                                 (every? successful-offers))
                                                          ::active
                                                          ::inactive))))]
    (db/transact!
      (concat
        [[:db/add [:island/id island-id]
          :island/population (:sim.out/population result)]
         [:db/add [:island/id island-id]
          :island/simulator-stats (assoc result
                                    :sim.out/citizen-money-balance citizen-money-balance
                                    :sim.out/government-money-balance government-money-balance)]
         [:db/add [:island/id island-id]
          :island/government-money-balance new-government-balance]
         [:db/add [:island/id island-id]
          :island/citizen-money-balance new-citizen-money-balance]]
        (for [[resident-id balance] new-resident-balances]
          [:db/add [:resident/id resident-id]
           :resident/money-balance (:resource/money balance)])
        (for [improvement (::active improvements-grouped-by-status)]
          [:db/add [:improvement/id (:improvement/id improvement)] :improvement/active? true])
        (for [improvement (::inactive improvements-grouped-by-status)]
          [:db/add [:improvement/id (:improvement/id improvement)] :improvement/active? false])))))

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
