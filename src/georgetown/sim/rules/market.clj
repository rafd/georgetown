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
                                          :tender/capacity capacity
                                          :tender/labour-per-unit labour-needed
                                          :tender/supply [:resource/food quantity]
                                          :tender/demand [:resource/money (* quantity (:offer/amount offer))]})}))
                     {:remaining-food (world/player-stock-amount world owner-id :resource/food)
                      :tenders []}
                     owner-offers))))))

(defn housing-tenders
  [world]
  (->> (:world/offers world)
       (filter blueprints/shelter-offer?)
       (map (fn [offer]
              (let [capacity (or (:offerable/capacity (blueprints/offerables (:offer/type offer)))
                                 0)]
                {:tender/offer-id (:offer/id offer)
                 :tender/player-id (:offer/owner-id offer)
                 :tender/improvement-id (:offer/improvement-id offer)
                 :tender/unit-price (:offer/amount offer)
                 :tender/supply [:resource/shelter capacity]
                 :tender/demand [:resource/money (* capacity (:offer/amount offer))]})))))

(defn clear-market
  "Two passes: pass 1 sets the clearing price; citizens that cannot afford it
  go without; pass 2, at the reduced demand, determines which suppliers sell."
  [resource citizens tenders]
  (let [population (count citizens)
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
                  tenders)]
    {:clearing-price clearing-price
     :buyers buyers
     :demand-filled demand-filled
     :supply-consumed supply-consumed
     :final-tenders final-tenders
     :available-supply (->> tenders
                            (map (fn [tender]
                                   (second (:tender/supply tender))))
                            (reduce + 0))}))

(defn offer-utilization
  ;; the tender fill ratio is relative to the stock-limited quantity,
  ;; so it reads as full when the player runs out of food or labour
  [fill-amount capacity]
  (double
    (if (= ##Inf capacity)
      (if (pos? fill-amount)
        1
        0)
      (/ fill-amount capacity))))

(defn run-food-market
  [world]
  (let [tenders (food-sale-tenders world)
        citizens (vals (:world/citizens world))
        population (count citizens)
        {:keys [clearing-price buyers demand-filled supply-consumed final-tenders available-supply]}
        (clear-market :resource/food citizens tenders)
        served-count (long demand-filled)
        average-price (if (pos? served-count)
                        (/ supply-consumed demand-filled)
                        0.0)
        served-citizens (take served-count (shuffle buyers))
        unserved-citizens (remove (set (map :citizen/id served-citizens))
                              (map :citizen/id citizens))]
    (-> world
        ;; buyers pay the average unit price, so money exactly matches supplier receipts
        (as-> world*
          (reduce (fn [memo citizen]
                    (world/update-citizen-savings memo (:citizen/id citizen) (- average-price)))
                  world*
                  served-citizens))
        (as-> world*
          (reduce (fn [memo citizen-id]
                    (update-in memo [:world/citizens citizen-id] citizen/stress-citizen constants/hungry-stress-increase))
                  world*
                  unserved-citizens))
        (as-> world*
          (reduce (fn [memo tender]
                    (let [fill-amount (or (:tender/fill-amount tender) 0)
                          revenue (* fill-amount (:tender/unit-price tender))]
                      (-> memo
                          (world/update-player-money (:tender/player-id tender) revenue)
                          (world/update-player-stock (:tender/player-id tender) :resource/food - fill-amount)
                          (world/update-improvement-stock (:tender/improvement-id tender) :resource/labour -
                                                          (* fill-amount (:tender/labour-per-unit tender 0.0)))
                          (assoc-in [:world/utilizations (:tender/offer-id tender)]
                                    (offer-utilization fill-amount (:tender/capacity tender))))))
                  world*
                  final-tenders))
        (assoc :world/food-stats
               {:demand population
                :affordable-demand (count buyers)
                :available-supply available-supply
                :supply demand-filled
                :clearing-price clearing-price
                :average-price average-price
                :cost supply-consumed
                :unserved-count (count unserved-citizens)})
        (assoc :world/hungry-citizen-ids (set unserved-citizens)))))

(defn food-market
  {:rule/description "Citizens buy food from player food-sale offers"
   :rule/inputs #{:world/offers :world/citizens :world/players
                  :world/improvements :world/utilizations}
   :rule/outputs #{:world/citizens :world/players :world/improvements :world/utilizations
                   :world/food-stats :world/hungry-citizen-ids}}
  [world]
  (-> world
      run-food-market
      (select-keys [:world/citizens
                    :world/players
                    :world/improvements
                    :world/utilizations
                    :world/food-stats
                    :world/hungry-citizen-ids])))

(defn shelter-market
  {:rule/description "At night, housing offers set the shelter clearing price; allocation decides who rents"
   :rule/inputs #{:world/shift :world/offers :world/citizens :world/previous-public-stats}
   :rule/outputs #{:world/shelter-stats}}
  [{:world/keys [shift citizens previous-public-stats] :as world}]
  {:world/shelter-stats
   (if (= :time-shift/night shift)
     (let [citizens (vals citizens)
           {:keys [clearing-price buyers available-supply]}
           (clear-market :resource/shelter citizens (housing-tenders world))]
       {:demand (count citizens)
        :affordable-demand (count buyers)
        :available-supply available-supply
        :clearing-price clearing-price})
     ;; shelter is rented once a day, so day shifts report last night
     (get-in previous-public-stats [:sim.out/resources :resource/shelter]))})

(def rules
  [#'food-market
   #'shelter-market])
