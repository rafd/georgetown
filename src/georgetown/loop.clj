(ns georgetown.loop
  (:require
    [chime.core :as chime]
    [georgetown.state :as s]
    [georgetown.schema :as schema]
    [georgetown.db :as db]
    [georgetown.market :as m])
  (:import
    [java.time Instant Duration]))

(partition-all 2 2 [10 30 100])

(defn extract-data-for-simulation
  [island-id]
  (let [island (db/q '[:find (pull ?island [*]) .
                       :in $ ?island-id
                       :where
                       [?island :island/id ?island-id]]
                     island-id)]
    {:sim.in/population (:island/population island)
     :sim.in/tenders
     (let [->offerable (->> schema/blueprints
                            vals
                            (mapcat :blueprint/offerables)
                            (schema/key-by :offerable/id))]
       (->> (db/q '[:find [(pull ?offer [*]) ...]
                    :where
                    [?offer :offer/id _]])
            (filter :offer/amount)
            (map (fn [offer]
                   (let [offerable (->offerable (:offer/type offer))]
                     {:tender/supply [(:offerable/supply-unit offerable)
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
  [{:sim.in/keys [population tenders]}]
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
         food-cost :market/total-cost}
        (m/market :resource/food food-demand :resource/money tenders)
        ;; SHELTER
        shelter-demand population
        {shelter-market-price :market/clearing-unit-price
         shelter-supplied :market/amount-supplied
         shelter-cost :market/total-cost}
        (m/market :resource/shelter shelter-demand :resource/money tenders)
        ;; MONEY
        ;; int, b/c of weird transit encoding bug (?)
        money-demand (Math/ceil (+ shelter-cost food-cost))
        potential-money-supply (->> tenders
                                    (keep
                                      (fn [tender]
                                        (let [[resource amount] (:tender/supply tender)]
                                          (when (= resource :resource/money)
                                            amount))))
                                    (apply +))
        {money-market-price :market/clearing-unit-price
         money-supplied :market/amount-supplied
         money-cost :market/total-cost}
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
     :sim.out/shelter {:demand shelter-demand
                       :available-supply potential-shelter-supply
                       :supply shelter-supplied
                       :price shelter-market-price
                       :tenders
                       (->> tenders
                            (filter
                              (fn [tender]
                                (= (first (:tender/supply tender)) :resource/shelter))))}
     :sim.out/food {:demand food-demand
                    :available-supply potential-food-supply
                    :supply food-supplied
                    :price food-market-price
                    :tenders
                    (->> tenders
                         (filter
                           (fn [tender]
                             (= (first (:tender/supply tender)) :resource/food))))}
     :sim.out/money {:demand money-demand
                     :available-supply potential-money-supply
                     :supply money-supplied
                     :price money-market-price
                     :tenders
                     (->> tenders
                          (filter
                            (fn [tender]
                              (= (first (:tender/supply tender)) :resource/money))))}
     :sim.out/labour {:demand potential-labour-demand
                      :available-supply potential-labour-supply
                      :supply labour-supplied
                      :price nil}
     :sim.out/joy joy
     :sim.out/population new-population}))


(defn tick! [island-id]
  (let [result (simulate (extract-data-for-simulation island-id))]
    (prn (:population result))
    (db/transact!
      [[:db/add [:island/id island-id] :island/population (:sim.out/population result)]
       [:db/add [:island/id island-id] :island/simulator-stats result]])))

(defn tick-all! []
  (doseq [island-id (db/q '[:find [?island-id ...]
                            :where
                            [?island :island/id ?island-id]])]
    (tick! island-id)))

#_(tick-all!)

#_(db/q '[:find (pull ?island [*]) .
          :where
          [?island :island/id _]])

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

(defn initialize-loop!
  []
  (when @scheduler
    (.close @scheduler))
  (reset! scheduler
            (chime/chime-at
              (chime/periodic-seq (Instant/now)
                                  (Duration/ofSeconds 1))
              (fn [time]
                (tap> [:tick time])
                (tick-all!))
              {:on-finished (fn []
                              (tap> "Schedule finished."))})))

#_(initialize-loop!)
#_(.close @scheduler)
