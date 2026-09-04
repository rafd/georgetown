(ns georgetown.sim.allocate
  (:require
    [hyperfiddle.rcf :as rcf]
    [georgetown.sim.blueprints :as blueprints]
    [georgetown.sim.constants :as constants]
    [georgetown.sim.time :as time]
    [georgetown.sim.util.ortools :as ortools]))

(defn citizen-offer-joy
  "Joy a citizen expects from spending the current shift on an offer (nil = idle)."
  [citizen offer {:keys [food-price shelter-price]}]
  (let [weights (:offerable/skill-productivity-weights
                  (blueprints/offerables (:offer/type offer)))
        income (blueprints/effect-sum offer :effect.direction/to-citizen :resource/money)
        money-cost (blueprints/effect-sum offer :effect.direction/from-citizen :resource/money)
        savings-after (max 0.0 (+ (:citizen/savings citizen) (- income money-cost)))
        daily-living-cost (* time/ticks-per-day
                             (+ (or food-price 0.0)
                                (or shelter-price 0.0)))
        days-of-savings (/ savings-after (max daily-living-cost 0.01))
        security-term (* (:citizen/preference.security citizen)
                         (/ days-of-savings
                            (+ days-of-savings constants/security-halfway-days)))
        activity-term (->> {:citizen/skill.fitness :citizen/preference.physical-activity
                            :citizen/skill.intellect :citizen/preference.intellectual-activity
                            :citizen/skill.social :citizen/preference.social-activity}
                           (map (fn [[skill preference]]
                                  (* (get citizen preference)
                                     (get weights skill 0.0))))
                           (reduce + 0.0))
        self-improvement-term (* (:citizen/preference.self-improvement citizen)
                                 (->> weights
                                      (map (fn [[skill weight]]
                                             (* weight
                                                (get citizen (blueprints/skill->talent skill))
                                                (- 1 (get citizen skill)))))
                                      (reduce + 0.0)))
        ;; stress-averse, stressed citizens value stress-reducing offers
        stress-relief-term (->> {:citizen/physical-stress :citizen/preference.physical-stress
                                 :citizen/mental-stress :citizen/preference.mental-stress}
                                (map (fn [[stress-key preference]]
                                       (* (- 1 (get citizen preference))
                                          (get citizen stress-key)
                                          (- (blueprints/effect-sum offer :effect.direction/to-citizen stress-key)))))
                                (reduce + 0.0))]
    (max 0.0
         (+ (* constants/joy-weight-security security-term)
            (* constants/joy-weight-activity activity-term)
            (* constants/joy-weight-self-improvement self-improvement-term)
            (* constants/joy-weight-stress stress-relief-term)))))

(defn allocate-shift
  "Assigns each citizen's current shift to at most one time-offer (nil = idle),
  maximizing the sum over citizens of sqrt(joy of the chosen offer), subject to
  offer capacities and player wage budgets."
  [{:allocate.in/keys [citizens offers player-budgets food-price shelter-price]}]
  (let [prices {:food-price food-price
                :shelter-price shelter-price}
        idle-allocations (->> citizens
                              (map (fn [citizen]
                                     [(:citizen/id citizen) nil]))
                              (into {}))
        pairs (for [citizen citizens
                    offer offers
                    :when (and (<= (:allocate/citizen-money-cost offer)
                                   (:citizen/savings citizen))
                               (<= (:allocate/wage offer)
                                   (get player-budgets (:offer/owner-id offer) 0)))]
                {:pair/id [(:citizen/id citizen) (:offer/id offer)]
                 :pair/citizen citizen
                 :pair/offer offer})]
    (if (empty? pairs)
      idle-allocations
      (let [idle-sqrt-joy-by-citizen-id
            (->> citizens
                 (map (fn [citizen]
                        [(:citizen/id citizen)
                         (Math/sqrt (citizen-offer-joy citizen nil prices))]))
                 (into {}))
            ;; coefficients are net of idling, so leaving a citizen unassigned == idle;
            ;; jitter breaks ties randomly (the solver itself is deterministic)
            coefficient-by-pair-id
            (->> pairs
                 (map (fn [{:pair/keys [id citizen offer]}]
                        [id (+ (Math/round
                                 (* 1000.0
                                    (- (Math/sqrt (citizen-offer-joy citizen offer prices))
                                       (idle-sqrt-joy-by-citizen-id (:citizen/id citizen)))))
                               (- (rand-int 3) 1))]))
                 (into {}))
            citizen-constraints
            (->> pairs
                 (group-by (fn [pair]
                             (first (:pair/id pair))))
                 (map (fn [[_citizen-id citizen-pairs]]
                        [:at-most-one (map :pair/id citizen-pairs)])))
            capacity-constraints
            (->> pairs
                 (group-by (fn [pair]
                             (second (:pair/id pair))))
                 (keep (fn [[_offer-id offer-pairs]]
                         (when-let [capacity (:offerable/capacity
                                               (blueprints/offerables
                                                 (:offer/type (:pair/offer (first offer-pairs)))))]
                           [:<= (->> offer-pairs
                                     (map (fn [pair]
                                            [(:pair/id pair) 1]))
                                     (into {}))
                            capacity]))))
            budget-constraints
            (->> pairs
                 (filter (fn [pair]
                           (pos? (:allocate/wage (:pair/offer pair)))))
                 (group-by (fn [pair]
                             (:offer/owner-id (:pair/offer pair))))
                 (map (fn [[owner-id owner-pairs]]
                        [:<= (->> owner-pairs
                                  (map (fn [pair]
                                         [(:pair/id pair)
                                          (Math/round (double (:allocate/wage (:pair/offer pair))))]))
                                  (into {}))
                         (long (Math/floor (double (get player-budgets owner-id 0))))])))
            solution (ortools/solve
                       {:ortools/bool-vars (set (keys coefficient-by-pair-id))
                        :ortools/constraints (concat citizen-constraints
                                                     capacity-constraints
                                                     budget-constraints)
                        :ortools/maximize coefficient-by-pair-id})]
        (merge idle-allocations
               (into {} (:solve/true-vars solution)))))))

#_(rcf/enable!)

(rcf/tests
  "allocate-shift"
  (let [base-citizen {:citizen/savings 100.0
                      :citizen/preference.security 0.5
                      :citizen/preference.self-improvement 0.5
                      :citizen/preference.physical-stress 0.5
                      :citizen/preference.mental-stress 0.5
                      :citizen/preference.spiritual-activity 0.5
                      :citizen/preference.social-activity 0.5
                      :citizen/preference.physical-activity 0.5
                      :citizen/preference.intellectual-activity 0.5
                      :citizen/talent.intellect 0.5
                      :citizen/talent.fitness 0.5
                      :citizen/talent.social 0.5
                      :citizen/skill.intellect 0.5
                      :citizen/skill.fitness 0.5
                      :citizen/skill.social 0.5
                      :citizen/physical-stress 0.5
                      :citizen/mental-stress 0.5}
        ;; :offer/food-market.job has capacity 1 and social-heavy skill weights
        job-offer {:offer/id :job
                   :offer/type :offer/food-market.job
                   :offer/amount 10
                   :offer/owner-id :player-1
                   :allocate/citizen-money-cost 0
                   :allocate/wage 10}
        prices {:allocate.in/food-price 5.0
                :allocate.in/shelter-price 5.0}]

    "higher-preference citizen wins a capacity-1 job"
    (allocate-shift
      (merge prices
             {:allocate.in/citizens [(merge base-citizen
                                            {:citizen/id :social
                                             :citizen/preference.social-activity 0.9})
                                     (merge base-citizen
                                            {:citizen/id :loner
                                             :citizen/preference.social-activity 0.0})]
              :allocate.in/offers [job-offer]
              :allocate.in/player-budgets {:player-1 1000}}))
    := {:social :job
        :loner nil}

    "player budget limits hires"
    (->> (allocate-shift
           (merge prices
                  {:allocate.in/citizens [(assoc base-citizen :citizen/id :a)
                                          (assoc base-citizen :citizen/id :b)]
                   :allocate.in/offers [{:offer/id :farm-job
                                         :offer/type :offer/farm.job
                                         :offer/amount 10
                                         :offer/owner-id :player-1
                                         :allocate/citizen-money-cost 0
                                         :allocate/wage 10}]
                   :allocate.in/player-budgets {:player-1 10}}))
         vals
         (remove nil?)
         count)
    := 1

    "unaffordable offers are never assigned"
    (allocate-shift
      (merge prices
             {:allocate.in/citizens [(assoc base-citizen :citizen/id :a)]
              :allocate.in/offers [{:offer/id :stroll
                                    :offer/type :offer/park.leisure
                                    :offer/owner-id :player-1
                                    :allocate/citizen-money-cost 500
                                    :allocate/wage 0}]
              :allocate.in/player-budgets {:player-1 1000}}))
    := {:a nil}

    "broke citizen still takes a paying job"
    (allocate-shift
      (merge prices
             {:allocate.in/citizens [(merge base-citizen
                                            {:citizen/id :broke
                                             :citizen/savings 0.0})]
              :allocate.in/offers [{:offer/id :farm-job
                                    :offer/type :offer/farm.job
                                    :offer/amount 100
                                    :offer/owner-id :player-1
                                    :allocate/citizen-money-cost 0
                                    :allocate/wage 100}]
              :allocate.in/player-budgets {:player-1 1000}}))
    := {:broke :farm-job}))
