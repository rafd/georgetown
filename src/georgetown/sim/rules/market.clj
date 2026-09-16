(ns georgetown.sim.rules.market
  (:require
    [georgetown.sim.blueprints :as blueprints]
    [georgetown.sim.citizen :as citizen]
    [georgetown.sim.constants :as constants]
    [georgetown.sim.market :as m]
    [georgetown.sim.world :as world]))

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
                             capacity (or (:offerable/capacity (blueprints/offerables (:offer/type offer)))
                                          ##Inf)
                             quantity (Math/floor
                                        (min remaining-food
                                             capacity
                                             (world/offer-stock-limited-uses world offer)))]
                         {:remaining-food (- remaining-food quantity)
                          :tenders (conj tenders
                                         {:tender/offer-id (:offer/id offer)
                                          :tender/player-id owner-id
                                          :tender/improvement-id (:offer/improvement-id offer)
                                          :tender/unit-price (:offer/amount offer)
                                          :tender/labour-per-unit labour-needed
                                          :tender/supply [:resource/food quantity]
                                          :tender/demand [:resource/money (* quantity (:offer/amount offer))]})}))
                     {:remaining-food (world/player-stock-amount world owner-id :resource/food)
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
  (let [{:keys [stats-key unserved-ids-key]}
        (case resource
          :resource/food {:stats-key :world/food-stats
                          :unserved-ids-key :world/hungry-citizen-ids}
          :resource/shelter {:stats-key :world/shelter-stats
                             :unserved-ids-key :world/unhoused-citizen-ids})
        tenders (case resource
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
                    (world/update-citizen-savings memo (:citizen/id citizen) (- average-price)))
                  world*
                  served-citizens))
        (as-> world*
          (reduce (fn [memo citizen-id]
                    (update-in memo [:world/citizens citizen-id] citizen/stress-citizen stress-increase))
                  world*
                  unserved-citizens))
        (as-> world*
          (reduce (fn [memo tender]
                    (let [fill-amount (or (:tender/fill-amount tender) 0)
                          revenue (* fill-amount (:tender/unit-price tender))]
                      (-> memo
                          (world/update-player-money (:tender/player-id tender) revenue)
                          (cond->
                            (= :resource/food resource)
                            (-> (world/update-player-stock (:tender/player-id tender) :resource/food - fill-amount)
                                (world/update-improvement-stock (:tender/improvement-id tender) :resource/labour -
                                                                (* fill-amount (:tender/labour-per-unit tender 0.0)))))
                          (assoc-in [:world/utilizations (:tender/offer-id tender)]
                                    (double (or (:tender/fill-ratio tender) 0))))))
                  world*
                  final-tenders))
        (assoc stats-key
               {:demand population
                :affordable-demand (count buyers)
                :available-supply available-supply
                :supply demand-filled
                :clearing-price clearing-price
                :average-price average-price
                :cost supply-consumed
                :unserved-count (count unserved-citizens)})
        (assoc unserved-ids-key (set unserved-citizens)))))

(defn food-market
  {:rule/description "Citizens buy food from player food-sale offers"
   :rule/inputs #{:world/offers :world/citizens :world/players
                  :world/improvements :world/utilizations}
   :rule/outputs #{:world/citizens :world/players :world/improvements :world/utilizations
                   :world/food-stats :world/hungry-citizen-ids}}
  [world]
  (-> world
      (run-goods-market :resource/food)
      (select-keys [:world/citizens
                    :world/players
                    :world/improvements
                    :world/utilizations
                    :world/food-stats
                    :world/hungry-citizen-ids])))

(defn shelter-market
  {:rule/description "Citizens rent shelter from player housing offers"
   :rule/inputs #{:world/offers :world/citizens :world/players :world/utilizations}
   :rule/outputs #{:world/citizens :world/players :world/utilizations
                   :world/shelter-stats :world/unhoused-citizen-ids}}
  [world]
  (-> world
      (run-goods-market :resource/shelter)
      (select-keys [:world/citizens
                    :world/players
                    :world/utilizations
                    :world/shelter-stats
                    :world/unhoused-citizen-ids])))

(def rules
  [#'food-market
   #'shelter-market])
