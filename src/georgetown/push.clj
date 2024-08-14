(ns georgetown.push
  (:require
    [bloom.commons.uuid :as uuid]
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
  (let [user-id (get-in request [:session :user-id])
        session-id (get-in request [:params :session-id])
        island-id (uuid/from-string (get-in request [:params :island-id]))]
    (if (get-in request [:params :force])
      {:status 200
       :body (s/client-state {:user-id user-id
                              :island-id island-id})}
      (http/as-channel request
        {:on-open (fn [ch]
                    (swap! subscriptions assoc session-id {:sub/user-id user-id
                                                           :sub/channel ch
                                                           :sub/island-id island-id}))
         :on-receive (fn [_ch _msg])
         :on-close (fn [_ch _status]
                     (swap! subscriptions dissoc session-id))}))))

(def encoder
  (m/create mj/options))

(defn send-state-update!
  [session-id]
  (when-let [{:sub/keys [channel user-id island-id]} (@subscriptions session-id)]
    (http/send! channel
                ;; async channels skip middleware (?)
                ;; so have to reapply encoding
                {:status 200
                 :headers {"Content-Type" "application/transit+json; charset=utf-8"}
                 :body
                 (m/encode encoder "application/transit+json"
                           (s/client-state {:user-id user-id
                                            :island-id island-id}))})))

(defn initialize!
  []
  (db/watch!
    ::push
    (fn [_ _ _ _]
      (doseq [session-id (keys @subscriptions)]
        (send-state-update! session-id))))
  nil)

#_(deref subscriptions)
