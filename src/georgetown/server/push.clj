(ns georgetown.server.push
  (:require
    [bloom.commons.uuid :as uuid]
    [bloom.commons.muuntaja :as mj]
    [org.httpkit.server :as http]
    [muuntaja.core :as m]
    [georgetown.server.state :as s]
    [georgetown.server.db :as db]))

(defn island-state
  [island-id]
  (db/q '[:find
          (pull ?island
                ;; don't use [*] here, to avoid leaking private information
                [:island/id
                 :island/epoch
                 :island/government-money-balance
                 :island/public-stats
                 :island/joy
                 {:island/players
                  [:player/id]}
                 {:island/citizens [*]}
                 {:island/lots
                  [:lot/id
                   :lot/x
                   :lot/y
                   :lot/elevation
                   :lot/moisture
                   {:lot/deed
                    [:deed/id
                     :deed/rate
                     {:player/_deeds
                      [:player/id
                       {:user/_players
                        [:user/id]}]}]}
                   {:lot/improvement
                    [:improvement/id
                     :improvement/type]}]}]) .
          :in $ ?island-id
          :where
          [?island :island/id ?island-id]]
        island-id))

(defn user-state
  [user-id]
  (when user-id
    (db/q '[:find (pull ?user [:user/id]) .
            :in $ ?user-id
            :where
            [?user :user/id ?user-id]]
          user-id)))

(defn player-state
  [user-id island-id]
  (when user-id
    (db/q '[:find (pull ?player
                        [:player/id
                         :player/money-balance
                         :player/private-stats
                         {:player/stocks
                          [:stock/id
                           :stock/resource
                           :stock/amount]}
                         {:player/loans
                          [:loan/id
                           :loan/amount
                           :loan/daily-payment-amount
                           :loan/minimum-daily-payment-amount
                           :loan/annual-interest-rate]}
                         {:player/deeds
                          [:deed/id
                           :deed/rate-changed-at
                           {:lot/_deed
                            [:lot/id
                             {:lot/improvement
                              [:improvement/id
                               {:improvement/offers
                                [:offer/id
                                 :offer/type
                                 :offer/amount
                                 :offer/utilization
                                 {:improvement/_offers [:improvement/id]}]}]}]}]}]) .
            :in $ ?user-id ?island-id
            :where
            [?user :user/id ?user-id]
            [?island :island/id ?island-id]
            [?island :island/players ?player]
            [?user :user/players ?player]]
          user-id
          island-id)))

;; map of session-id -> {:sub/channel ... :sub/user-id ... :sub/island-id ...}
(defonce subscriptions (atom {}))
#_(reset! subscriptions {})
#_(deref subscriptions)

(defn handler
  [request]
  ;; when force, then immediate reply
  (let [user-id (get-in request [:session :user-id])
        session-id (get-in request [:params :session-id])
        island-id (uuid/from-string (get-in request [:params :island-id]))]
    (if (get-in request [:params :force])
      (if (and island-id (s/exists? :island/id island-id))
        {:status 200
         :body
         {:client-state/island (island-state island-id)
          :client-state/user (user-state user-id)
          :client-state/player (player-state user-id island-id)}}
        {:status 400})
      (http/as-channel request
        {:on-open (fn [ch]
                    (swap! subscriptions assoc session-id {:sub/user-id user-id
                                                           :sub/channel ch
                                                           :sub/island-id island-id}))
         :on-close (fn [_ _]
                     (swap! subscriptions dissoc session-id))}))))

(def encoder
  (m/create mj/options))

(defn on-db-change!
  []
  ;; minimally calculate the various states
  ;; island-state is the same for all watchers of an island
  ;; user-state and player-state would be the same for a user with multiple sessions open
  (let [s @subscriptions ;; deref here and reuse, to avoid race conditions
        island-states (let [island-ids (->> (vals s)
                                            (map :sub/island-id)
                                            set)]
                        (zipmap island-ids
                                (->> island-ids
                                     (map island-state))))
        user-states (let [user-ids (->> (vals s)
                                        (map :sub/user-id)
                                        set)]
                      (zipmap user-ids
                              (map user-state user-ids)))
        player-states (let [user-island-ids (->> (vals s)
                                                   (map (juxt :sub/user-id :sub/island-id))
                                                   set)]
                          (zipmap user-island-ids
                                  (map (fn [[user-id island-id]] (player-state user-id island-id)) user-island-ids)))]
    (->> s
         (pmap (fn [[session-id {:sub/keys [user-id island-id channel]}]]
                 (http/send! channel
                             ;; async channels skip middleware (?)
                             ;; so have to reapply encoding
                             {:status 200
                              :headers {"Content-Type" "application/transit+json; charset=utf-8"}
                              :body
                              (m/encode encoder "application/transit+json"
                                        {:client-state/island (island-states island-id)
                                         :client-state/user (user-states user-id)
                                         :client-state/player (player-states [user-id island-id])})})))
         doall)))

(defn initialize!
  []
  (db/watch!
    ::push
    (fn [_report]
      (on-db-change!)))
  nil)


