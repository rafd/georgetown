(ns georgetown.server.tada
  (:require
    [tada.events.core :as tada]
    [georgetown.server.api :as server-api]
    [georgetown.sim.api :as sim-api]))

(defonce t (tada/init :malli))

;; temporarily hack around a flaw in tada
(with-redefs [tada.events.core/valid? (fn [_ _] true)]
  (tada/register! t (concat server-api/commands sim-api/commands)))

(defn exec! [k params]
  (tap> ["exec!" k params])
  (try
    (tada/do! t k params)
    (catch clojure.lang.ExceptionInfo e
      (println
        (ex-message e))
      (clojure.pprint/pprint
        (ex-data e))
      (throw e))))
