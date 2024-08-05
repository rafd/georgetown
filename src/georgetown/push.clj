(ns georgetown.push
  (:require
    [bloom.commons.muuntaja :as mj]
    [org.httpkit.server :as http]
    [muuntaja.core :as m]
    [georgetown.db :as db]
    [georgetown.state :as s]))

;; map of user-id -> channel
(defonce subscriptions (atom {}))

(defn handler
  [request]
  ;; when force, then immediate reply
  (if (get-in request [:params :force])
    {:status 200
     :body (s/client-state #uuid "614a34a6-4505-40e9-858b-581a0d26602a")}
    (http/as-channel request
      {:on-open (fn [ch]
                  (swap! subscriptions assoc #uuid "614a34a6-4505-40e9-858b-581a0d26602a" ch))
       :on-receive (fn [_ch _msg])
       :on-close (fn [_ch _status]
                   (swap! subscriptions assoc #uuid "614a34a6-4505-40e9-858b-581a0d26602a" nil))})))

(def encoder
  (m/create mj/options))

(defn send-state-update!
  [user-id]
  (when-let [channel (@subscriptions user-id)]
    (http/send! channel
                ;; async channels skip middleware (?)
                ;; so have to reapply encoding
                {:status 200
                 :headers {"Content-Type" "application/transit+json; charset=utf-8"}
                 :body
                 (m/encode encoder "application/transit+json"
                           (s/client-state user-id))})))

(defn initialize!
  []
  (add-watch
    (db/conn)
    ::state-watcher
    (fn [_ _ _ _]
      (doseq [user-id (keys @subscriptions)]
        (send-state-update! user-id)))))

#_(deref subscriptions)

#_(s/client-state
    {:user-id #uuid "00000000-0000-0000-0000-000000000000"})

#_(send-state-update!
    #uuid "614a34a6-4505-40e9-858b-581a0d26602a")
