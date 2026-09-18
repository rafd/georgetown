(ns georgetown.server.push
  (:require
    [bloom.commons.uuid :as uuid]
    [bloom.commons.muuntaja :as mj]
    [org.httpkit.server :as http]
    [muuntaja.core :as m]
    [georgetown.server.state :as s]
    [georgetown.server.db :as db]
    [georgetown.server.events :as events]))

(defn island-state
  [island-id]
  ;; events are queried separately, to keep limited/system events out of the public pull
  (some->
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
          island-id)
    (assoc :island/events (events/recent-public-events island-id))))

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
    (some->
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
                               {:improvement/stocks
                                [:stock/id
                                 :stock/resource
                                 :stock/amount]}
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
            island-id)
      (as-> player
        (assoc player :player/events
               (events/recent-limited-events island-id (:player/id player)))))))

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

(defn encode-client-state
  [client-state]
  {:status 200
   ;; async channels skip middleware, so encoding is applied here
   :headers {"Content-Type" "application/transit+json; charset=utf-8"}
   ;; muuntaja returns a one-shot input stream; a byte array can be sent to several channels
   :body (.readAllBytes (m/encode encoder "application/transit+json" client-state))})

(defn on-db-change!
  []
  ;; minimally calculate the various states
  ;; island-state is the same for all watchers of an island
  ;; user-state and player-state would be the same for a user with multiple sessions open
  (let [subscriptions-snapshot @subscriptions ;; deref once and reuse, to avoid race conditions
        island-states (let [island-ids (->> (vals subscriptions-snapshot)
                                            (map :sub/island-id)
                                            set)]
                        (zipmap island-ids
                                (map island-state island-ids)))
        user-states (let [user-ids (->> (vals subscriptions-snapshot)
                                        (map :sub/user-id)
                                        set)]
                      (zipmap user-ids
                              (map user-state user-ids)))]
    ;; sessions with the same user and island get identical payloads,
    ;; so encode once per group
    (doseq [[[user-id island-id] group-subscriptions] (group-by (juxt :sub/user-id :sub/island-id)
                                                                (vals subscriptions-snapshot))]
      (let [response (encode-client-state
                       {:client-state/island (island-states island-id)
                        :client-state/user (user-states user-id)
                        :client-state/player (player-state user-id island-id)})]
        (doseq [{:sub/keys [channel]} group-subscriptions]
          (http/send! channel response))))))

(defn initialize!
  []
  (db/watch!
    ::push
    (fn [_report]
      (on-db-change!)))
  nil)


