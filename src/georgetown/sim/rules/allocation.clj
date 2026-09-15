(ns georgetown.sim.rules.allocation
  (:require
    [georgetown.sim.allocate :as allocate]
    [georgetown.sim.blueprints :as blueprints]
    [georgetown.sim.constants :as constants]
    [georgetown.sim.util.math :as math]
    [georgetown.sim.world :as world]))

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
                      (math/clamp01 (+ level
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
              :resource/money (world/update-citizen-savings world* citizen-id (- amount))
              world*)
            :effect.direction/to-citizen
            (cond
              (= :resource/money target)
              (world/update-citizen-savings world* citizen-id amount)
              (contains? #{:citizen/physical-stress :citizen/mental-stress
                           :citizen/skill.intellect :citizen/skill.fitness :citizen/skill.social} target)
              (update-in world* [:world/citizens citizen-id target]
                         (fn [value] (math/clamp01 (+ value amount))))
              :else
              world*)
            :effect.direction/from-player
            (if (= :resource/money target)
              (world/update-player-money world* (:offer/owner-id offer) (- amount))
              (world/update-player-stock world* (:offer/owner-id offer) target - amount))
            :effect.direction/to-player
            (if (= :resource/money target)
              (world/update-player-money world* (:offer/owner-id offer) amount)
              (world/update-player-stock world* (:offer/owner-id offer) target +
                                         (* amount productivity-factor)))
            :effect.direction/from-self
            (world/update-improvement-stock world* (:offer/improvement-id offer) target - amount)
            :effect.direction/to-self
            (world/update-improvement-stock world* (:offer/improvement-id offer) target +
                                            (* amount productivity-factor)))))
      world
      (:offerable/effects offerable))))

(defn allocation
  {:rule/description "Citizens are assigned to time offers (jobs & leisure) for the shift"
   :rule/inputs #{:world/shift :world/offers :world/citizens :world/players
                  :world/improvements :world/utilizations
                  :world/food-stats :world/shelter-stats}
   :rule/outputs #{:world/citizens :world/players :world/improvements :world/utilizations
                   :world/allocation-stats}}
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
                       :allocate.in/food-price (get-in world [:world/food-stats :clearing-price])
                       :allocate.in/shelter-price (get-in world [:world/shelter-stats :clearing-price])})
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
        (assoc :world/allocation-stats
               {:citizen-activities (->> allocations
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
                                 count)})
        (select-keys [:world/citizens
                      :world/players
                      :world/improvements
                      :world/utilizations
                      :world/allocation-stats]))))

(def rules
  [#'allocation])
