(ns georgetown.server.routes
  (:require
   [dat.api :as dat]
   [tada.events.core :as tada]
   [taoensso.telemere :as t]
   [georgetown.server.tada :as server-tada]
   [georgetown.server.push :as push]
   [georgetown.server.db :as db]))

(defn dispatch-event!
  [event-id event-params]
  (t/trace! {:id :command
             :data {:command event-id}}
    (try
      (if-let [return (dat/with-transaction [tx (db/db)]
                        (t/trace! {:id :command/effect}
                          (tada/do! server-tada/t event-id
                                    (assoc event-params :tx tx))))]
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
                   (throw e))}))))

(def api
  [[[:post "/api/command"]
    (fn [request]
      (dispatch-event!
        (get-in request [:body-params :command])
        (assoc (get-in request [:body-params :params])
          :user-id (get-in request [:session :user-id]))))]

   [[:get "/api/state"]
    (fn [request]
      (push/handler request))]])

