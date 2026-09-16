(ns georgetown.sim.allocate
  (:require
    [hyperfiddle.rcf :as rcf]
    [georgetown.sim.blueprints :as blueprints]
    [georgetown.sim.constants :as constants]
    [georgetown.sim.time :as time]
    [georgetown.sim.util.ortools :as ortools]))

(defn days-of-savings
  [savings {:keys [food-price shelter-price]}]
  (let [daily-living-cost (* time/ticks-per-day
                             (+ (or food-price 0.0)
                                (or shelter-price 0.0)))]
    (/ savings (max daily-living-cost 0.01))))

(defn job-seeker?
  "Savings cover fewer than security-halfway-days of living costs"
  [citizen prices]
  (< (days-of-savings (:citizen/savings citizen) prices)
     constants/security-halfway-days))

(defn paid-offer?
  [offer]
  (pos? (:allocate/wage offer)))

(defn job-openings
  "Paid slots this shift; per owner, capped by how many wages the budget covers.
  Uses the owner's lowest wage, so mixed-wage owners get an upper bound."
  [offers player-budgets]
  (->> offers
       (filter paid-offer?)
       (group-by :offer/owner-id)
       (map (fn [[owner-id owner-offers]]
              (let [capacity-sum (->> owner-offers
                                      (map (fn [offer]
                                             (or (:allocate/capacity offer)
                                                 ##Inf)))
                                      (reduce + 0.0))
                    budget-slots (Math/floor (/ (get player-budgets owner-id 0)
                                                (->> owner-offers
                                                     (map :allocate/wage)
                                                     (reduce min))))]
                (min capacity-sum budget-slots))))
       (reduce + 0.0)
       long))

(defn citizen-offer-joy
  "Joy a citizen expects from spending the current shift on an offer (nil = idle)."
  [citizen offer prices]
  (let [offerable (blueprints/offerables (:offer/type offer))
        weights (:offerable/skill-productivity-weights offerable)
        income (blueprints/effect-sum offer :effect.direction/to-citizen :resource/money)
        money-cost (blueprints/effect-sum offer :effect.direction/from-citizen :resource/money)
        savings-after (max 0.0 (+ (:citizen/savings citizen) (- income money-cost)))
        days-of-savings (days-of-savings savings-after prices)
        security-term (* (:citizen/preference.security citizen)
                         (/ days-of-savings
                            (+ days-of-savings constants/security-halfway-days)))
        activity-term (->> (:offerable/activity-weights offerable)
                           (map (fn [[preference weight]]
                                  (* (get citizen preference 0.0)
                                     weight)))
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
                         (when-let [capacity (:allocate/capacity (:pair/offer (first offer-pairs)))]
                           [:<= (->> offer-pairs
                                     (map (fn [pair]
                                            [(:pair/id pair) 1]))
                                     (into {}))
                            capacity]))))
            budget-constraints
            (->> pairs
                 (filter (fn [pair]
                           (paid-offer? (:pair/offer pair))))
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
  "job-seeker?"
  (let [prices {:food-price 5.0
                :shelter-price 5.0}]
    ;; daily living cost = 4 ticks * 10 = 40; halfway = 30 days = 1200
    (job-seeker? {:citizen/savings 1160.0} prices) := true
    (job-seeker? {:citizen/savings 1240.0} prices) := false)

  "job-openings"
  (let [job (fn [id owner wage capacity]
              {:offer/id id
               :offer/owner-id owner
               :allocate/wage wage
               :allocate/capacity capacity})]

    "capacity-limited"
    (job-openings [(job :a :player-1 10 3)] {:player-1 1000}) := 3

    "budget-limited"
    (job-openings [(job :a :player-1 10 3)] {:player-1 25}) := 2

    "unlimited capacity counts as budget-limited"
    (job-openings [(job :a :player-1 10 nil)] {:player-1 45}) := 4

    "leisure (wage 0) and broke owners contribute nothing"
    (job-openings [(job :stroll :player-1 0 10)
                   (job :b :player-2 10 5)]
                  {:player-1 1000
                   :player-2 0})
    := 0

    "summed across owners"
    (job-openings [(job :a :player-1 10 3)
                   (job :b :player-2 20 2)]
                  {:player-1 1000
                   :player-2 20})
    := 4)

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
        ;; :offer/food-market.job has social-heavy skill weights
        job-offer {:offer/id :job
                   :offer/type :offer/food-market.job
                   :offer/amount 10
                   :offer/owner-id :player-1
                   :allocate/citizen-money-cost 0
                   :allocate/wage 10
                   :allocate/capacity 1}
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
