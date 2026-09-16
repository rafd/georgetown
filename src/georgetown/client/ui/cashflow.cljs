(ns georgetown.client.ui.cashflow
  (:require
    [georgetown.client.state :as state]
    [georgetown.client.ui.common :as ui]
    [georgetown.sim.blueprints :as blueprints]
    [georgetown.sim.time :as time]))

(defn amount
  [value]
  [:span {:tw (if (< value 0)
                "text-red-600"
                "text-blue-600")}
   (ui/format value 2)])

(defn per-day-total
  [per-tick-values]
  ;; extrapolate to a full day when fewer ticks are cached
  (if (seq per-tick-values)
    (* time/ticks-per-day
       (/ (reduce + per-tick-values)
          (count per-tick-values)))
    0))

(defn offer-tick-utilizations
  [offer]
  (->> @state/offer-utilization-history
       (map (fn [offer-id->utilization]
              (or (offer-id->utilization (:offer/id offer))
                  0)))))

(defn offer-with-net-amount
  [offer direction]
  (let [offerable (blueprints/offerables (:offer/type offer))
        per-unit (blueprints/effect-sum offer direction :resource/money)]
    (when (pos? per-unit)
      ;; the cache spans several days; estimate from the last day only
      (let [tick-utilizations (take time/ticks-per-day (offer-tick-utilizations offer))]
        (assoc offer
          :offer/tick-utilizations tick-utilizations
          :offer/net-amount (* (per-day-total tick-utilizations)
                               (or (:offerable/capacity offerable) 1)
                               per-unit))))))

(defn improvement-id->offers []
  (->> @state/offers
       (group-by (fn [offer]
                   (:improvement/id (:improvement/_offers offer))))))

(defn lot-cashflow-line
  [lot improvement-id->offers]
  (let [deed (:lot/deed lot)
        improvement (:lot/improvement lot)
        offers (improvement-id->offers (:improvement/id improvement))
        revenue-offer (->> offers
                           (keep (fn [offer]
                                   (offer-with-net-amount offer :effect.direction/to-player)))
                           first)
        expense-offer (->> offers
                           (keep (fn [offer]
                                   (offer-with-net-amount offer :effect.direction/from-player)))
                           first)
        ;; deed rate is charged each tick; show per-day
        deed-rate-per-day (- (* time/ticks-per-day (:deed/rate deed)))]
    {:lot lot
     :improvement improvement
     :revenue-offer revenue-offer
     :expense-offer expense-offer
     :deed-rate deed-rate-per-day
     :total (+ (:offer/net-amount revenue-offer)
               (- (:offer/net-amount expense-offer))
               deed-rate-per-day)}))
