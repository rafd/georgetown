(ns georgetown.sim.constants
  (:require
    [georgetown.sim.time :as time]))

;; a tick is one shift; 4 ticks per simulated day
(def shift-order
  [:time-shift/morning
   :time-shift/afternoon
   :time-shift/evening
   :time-shift/night])

(defn epoch->shift [epoch]
  (nth shift-order (mod epoch (count shift-order))))

;; cljs has no ratios, use division

(def hungry-stress-increase 0.03)
(def unhoused-stress-increase 0.10)
(def learn-rate 0.01)
(def stress-amp-base 0.0001)
(def skill-decline-base 0.00005)
(def base-death-chance (/ 1.0 (* 80 time/ticks-per-year)))
(def death-stress-factor 3.0)
(def birth-chance-per-citizen-per-tick (/ 1.0 (* 25 time/ticks-per-year)))
(def citizen-immigration-chance (/ 1 50))
(def max-emigration-chance (/ 1 4))
(def money-supply-target-per-citizen 600)

;; shift-allocation joy weights
(def joy-weight-security 1.0)
(def joy-weight-activity 1.0)
(def joy-weight-self-improvement 1.0)
(def joy-weight-stress 1.0)
;; days of savings at which the security term reaches half its maximum
(def security-halfway-days 30.0)
