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

(defn government-balance
  [island-id]
  (db/q '[:find ?balance .
          :in $ ?island-id
          :where
          [?island :island/id ?island-id]
          [?island :island/government-money-balance ?balance]]
        island-id))

(defn extract-data-for-simulation
  [island-id]
  (let [island (db/q '[:find (pull ?island [:island/population
                                            :island/government-money-balance]) .
                       :in $ ?island-id
                       :where
                       [?island :island/id ?island-id]]
                     island-id)]
    {:sim.in/population (:island/population island)
     :sim.in/government-money-balance (:island/government-money-balance island)
     :sim.in/tenders
     (let [->offerable (->> schema/blueprints
                            vals
                            (mapcat :blueprint/offerables)
                            (schema/key-by :offerable/id))]
       (->> (db/q '[:find [(pull ?offer [*]) ...]
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
                   (let [offerable (->offerable (:offer/type offer))]
                     {:tender/resident-id (::resident-id offer)
                      :tender/supply [(:offerable/supply-unit offerable)
                                      (or (:offerable/supply-amount offerable)
                                          (:offer/amount offer))]
                      :tender/demand [(:offerable/demand-unit offerable)
                                      (or (:offerable/demand-amount offerable)
                                          (:offer/amount offer))]})))))}))


;; TODO handle oversupply - some suppliers don't get paid (and their stuff won't operate)

#_(tick-all!)

;; 1 tick ~= 1 week
(def food-demand-per-person-per-week 21) ;; 1 food ~= meals
(def shelter-demand-per-person-per-week 1) ;; 1 shelter ~= 1 week of rent
(def max-labour-supply-per-person-per-week (* 7 15)) ;; 1 labour ~= 1 hour

(defn simulate
  [{:sim.in/keys [population tenders government-money-balance]}]
  (let [potential-food-supply (->> tenders
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
        ;; if not enough shelter or food, population decreases to match
        max-supported-population (Math/floor
                                   (min (/ potential-food-supply
                                           food-demand-per-person-per-week)
                                        (/ potential-shelter-supply
                                           shelter-demand-per-person-per-week)))
        population (max (min population max-supported-population) 1)
        ;; FOOD
        food-demand (* 21 population)
        {food-market-price :market/clearing-unit-price
         food-supplied :market/amount-supplied
         food-cost :market/total-cost
         food-succesful-tenders :market/succesful-tenders}
        (m/market :resource/food food-demand :resource/money tenders)
        ;; SHELTER
        shelter-demand population
        {shelter-market-price :market/clearing-unit-price
         shelter-supplied :market/amount-supplied
         shelter-cost :market/total-cost
         shelter-succesful-tenders :market/succesful-tenders}
        (m/market :resource/shelter shelter-demand :resource/money tenders)
        ;; MONEY
        ;; int, b/c of weird transit encoding bug (?)
        raw-money-demand (Math/ceil (+ shelter-cost food-cost))
        ;; government redistributes up to 80% of its tax revenues
        citizens-dividend (min raw-money-demand
                               (Math/ceil (* 0.8 government-money-balance)))
        money-demand (- raw-money-demand citizens-dividend)
        potential-money-supply (->> tenders
                                    (keep
                                      (fn [tender]
                                        (let [[resource amount] (:tender/supply tender)]
                                          (when (= resource :resource/money)
                                            amount))))
                                    (apply +))
        {money-market-price :market/clearing-unit-price
         money-supplied :market/amount-supplied
         money-cost :market/total-cost
         money-succesful-tenders :market/succesful-tenders}
        (m/market :resource/money money-demand :resource/labour tenders)

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
        joy (/ (- potential-labour-supply
                  labour-supplied)
               potential-labour-supply)
        potential-population-increase (max (- max-supported-population population) 0)
        population-decrease (->> (repeatedly (fn []
                                               (< (rand) (/ (- 1 joy)
                                                            ;; divide, just to slow things down
                                                            40))))
                                 (take population)
                                 (filter true?)
                                 count
                                 -)
        ;; for every potential person, there is a joy/c% chance they will join
        population-increase (->> (repeatedly (fn []
                                               (< (rand) (/ joy
                                                            ;; divide, just to slow things down
                                                            40))))
                                 (take potential-population-increase)
                                 (filter true?)
                                 count)
        new-population (+ population population-decrease population-increase)]

    {:sim.out/max-supported-population max-supported-population
     :sim.out/government-money-balance government-money-balance
     :sim.out/citizens-dividend citizens-dividend
     :sim.out/resources
     {:resource/shelter {:demand shelter-demand
                         :available-supply potential-shelter-supply
                         :supply shelter-supplied
                         :price shelter-market-price
                         :tenders
                         (->> tenders
                              (filter
                                (fn [tender]
                                  (= (first (:tender/supply tender)) :resource/shelter))))
                         :succesful-tenders shelter-succesful-tenders}
      :resource/food {:demand food-demand
                      :available-supply potential-food-supply
                      :supply food-supplied
                      :price food-market-price
                      :tenders
                      (->> tenders
                           (filter
                             (fn [tender]
                               (= (first (:tender/supply tender)) :resource/food))))
                      :succesful-tenders food-succesful-tenders}
      :resource/money {:demand money-demand
                       :available-supply potential-money-supply
                       :supply money-supplied
                       :price money-market-price
                       :tenders
                       (->> tenders
                            (filter
                              (fn [tender]
                                (= (first (:tender/supply tender)) :resource/money))))
                       :succesful-tenders money-succesful-tenders}
      :resource/labour {:demand potential-labour-demand
                        :available-supply potential-labour-supply
                        :supply labour-supplied
                        :price nil}}
     :sim.out/joy joy
     :sim.out/population new-population}))

(defn incomes
  [sim-out]
   (->> sim-out
        (x/select
          [:sim.out/resources
           x/MAP-VALS
           :succesful-tenders
           x/ALL
           (fn [tender]
             (= :resource/money (first (:tender/demand tender))))
           (x/collect-one [:tender/resident-id])
           :tender/demand
           1])
        (reduce (fn [memo [resident-id amount]]
                  (update memo resident-id (fnil + 0) amount))
                {})))

(defn taxes
  [island-id]
  (->> (db/q '[:find ?resident-id ?rate ?deed-id
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

(defn balances
  [island-id]
  (->> (db/q '[:find ?resident-id ?balance
               :in $ ?island-id
               :where
               [?island :island/id ?island-id]
               [?island :island/residents ?resident]
               [?resident :resident/money-balance ?balance]
               [?resident :resident/id ?resident-id]]
             island-id)
       (reduce (fn [memo [owner-id rate _]]
                 (update memo owner-id (fnil + 0) rate))
               {})))

(defn tick!
  [island-id]
  (let [result (simulate (extract-data-for-simulation island-id))
        resident-balances (balances island-id)
        resident-incomes (incomes result)
        resident-taxes (taxes island-id)
        new-resident-balances (merge-with (fnil + 0)
                                 resident-balances
                                 resident-incomes
                                 resident-taxes)
        government-balance (government-balance island-id)
        government-expenses (- (:sim.out/citizens-dividend result))
        government-revenues (- (apply + (vals resident-taxes)))
        new-government-balance (+ government-balance
                                  government-revenues
                                  government-expenses)]
    (db/transact!
      (concat
        [[:db/add [:island/id island-id]
          :island/population (:sim.out/population result)]
         [:db/add [:island/id island-id]
          :island/simulator-stats result]
         [:db/add [:island/id island-id]
          :island/government-money-balance new-government-balance]]
        (for [[resident-id balance] new-resident-balances]
          [:db/add [:resident/id resident-id]
           :resident/money-balance balance])))))

(defn tick-all! []
  (doseq [island-id (db/q '[:find [?island-id ...]
                            :where
                            [?island :island/id ?island-id]])]
    (tick! island-id)))

#_(tick-all!)

#_(balances (:island/id (first (s/all-of-type :island/id '[:island/id]))))

(defn demo-tick [s]
  (let [c (simulate s)]
    (-> s
        (assoc :sim/population (:new-population c))
        (update ::history (fnil conj []) (:population-growth c)))))

#_(->> {:sim/population 100
        :sim/tenders
        [;; food for money
         {:tender/supply [:resource/food 750]
          :tender/demand [:resource/money 1000]
          :tender/owner :a
          :tender/id 1}
         {:tender/supply [:resource/food 1500]
          :tender/demand [:resource/money 1500]
          :tender/owner :b
          :tender/id 2}
         {:tender/supply [:resource/food 1000]
          :tender/demand [:resource/money 1750]
          :tender/owner :c
          :tender/id 3}
         ;; shelter for money
         {:tender/supply [:resource/shelter 250]
          :tender/demand [:resource/money 2500]
          :tender/owner :a
          :tender/id 1}
         {:tender/supply [:resource/shelter 250]
          :tender/demand [:resource/money 5000]
          :tender/owner :b
          :tender/id 2}
         {:tender/supply [:resource/shelter 250]
          :tender/demand [:resource/money 7500]
          :tender/owner :c
          :tender/id 3}
         ;; money for labour
         {:tender/supply [:resource/money 10000]
          :tender/demand [:resource/labour 500]
          :tender/owner :a
          :tender/id 1}
         {:tender/supply [:resource/money 10000]
          :tender/demand [:resource/labour 600]
          :tender/owner :a
          :tender/id 2}
         {:tender/supply [:resource/money 10000]
          :tender/demand [:resource/labour 700]
          :tender/owner :b
          :tender/id 3}
         {:tender/supply [:resource/money 10000]
          :tender/demand [:resource/labour 800]
          :tender/owner :c
          :tender/id 4}]}
       demo-tick
       demo-tick
       demo-tick
       demo-tick
       demo-tick)

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
