(ns georgetown.sim.time)

(def ticks-per-day 4)
(def ticks-per-year (* 365 ticks-per-day))

(defn ticks->years [ticks]
  (/ ticks ticks-per-year))
