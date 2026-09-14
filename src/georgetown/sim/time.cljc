(ns georgetown.sim.time)

(def ticks-per-day 4)
(def ticks-per-year (* 365 ticks-per-day))

(defn ticks->years [ticks]
  (/ ticks ticks-per-year))

;; docs.lot.change-rate - deed rate cannot be lowered (nor lot abandoned) for 1 year
(defn deed-rate-lock [rate-changed-at current-epoch]
  (let [expiry (+ rate-changed-at ticks-per-year)]
    {:locked? (< current-epoch expiry)
     :days-remaining (long (Math/ceil (/ (- expiry current-epoch) ticks-per-day)))}))
