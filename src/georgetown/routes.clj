(ns georgetown.routes
  (:require
    [tada.events.malli :as tada]))

(defn dispatch-event!
  [event-id event-params]
  (try
    (if-let [return (tada/do! event-id event-params)]
      {:status 200
       :body return}
      {:status 200})
    (catch clojure.lang.ExceptionInfo e
      {:body (.getMessage e)
       :status (case (:anomaly (ex-data e))
                 :incorrect 400
                 :forbidden 403
                 :unsupported 405
                 :not-found 404
                 ;; if no anomaly (usually do to event :effect or :return throwing)
                 ;; rethrow the exception
                 (throw e))})))

(def api
  [[[:post "/api/command"]
    (fn [request]
      (dispatch-event!
        (get-in request [:body-params :command])
        (assoc (get-in request [:body-params :params])
          :user-id #uuid "00000000-0000-0000-0000-000000000000")))]])
