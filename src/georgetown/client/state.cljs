(ns georgetown.client.state
  (:require
    [bloom.commons.uuid :as uuid]
    [com.rpl.specter :as x]
    [bloom.commons.ajax :as ajax]
    [reagent.core :as r]))

(defn exec!
  [command params]
  (js/Promise.
    (fn [resolve reject]
      (ajax/request {:uri "/api/command"
                     :method :post
                     :params {:command command
                              :params params}
                     :on-success resolve
                     :on-error reject}))))

(defn exec-atom!
  [command params]
  (let [a (r/atom nil)]
    (-> (exec! command params)
        (.then (fn [x]
                 (reset! a x))))
    a))

(defonce session-id (uuid/random))

(defonce island-id (r/atom nil))

(defonce user (r/atom nil))

(defonce island (r/atom nil))

(defonce player (r/atom nil))

(defonce private-stats-history (r/atom '()))
(defonce public-stats-history (r/atom '()))

(defonce money-balance
  (r/reaction (:player/money-balance @player)))

(defonce stocks
  (r/reaction
    (->> (:player/stocks @player)
         (filter (fn [stock]
                   (pos? (:stock/amount stock))))
         (sort-by :stock/resource))))

(defonce improvements
  (r/reaction
    (->> @player
         (x/select
           [:player/deeds
            x/ALL
            :lot/_deed
            x/ALL
            :lot/improvement]))))

(defonce offers (r/reaction
                  (->> @improvements
                       (x/select
                         [x/ALL
                          :improvement/offers
                          x/ALL]))))

(defn subscribe
  [[source & path]]
  (r/reaction
    (x/select-one
      (->> path
           (map (fn [x]
                  (get {:ALL x/ALL} x x))))
      (case source
        :PRIVATE @player
        :PUBLIC @island))))

(defn set-island-id! [id]
  (when (not= id @island-id)
    (reset! island-id id)
    (reset! island nil)
    (reset! player nil)
    (reset! public-stats-history '())
    (reset! private-stats-history '())))

(defn get-island-state []
  (when @island-id
    (ajax/request {:uri "/api/state"
                   :method :get
                   :params (merge {:session-id session-id
                                   :island-id @island-id}
                                  (when (nil? @island)
                                    {:force true}))
                   :on-error (fn [_]
                               (js/setTimeout get-island-state 10000))
                   :on-success (fn [client-state]
                                 (reset! user (:client-state/user client-state))
                                 (reset! player (:client-state/player client-state))
                                 (reset! island (:client-state/island client-state))
                                 (swap! public-stats-history
                                        (fn [prev]
                                          (take 60 (conj prev (:island/public-stats (:client-state/island client-state))))))
                                 (swap! private-stats-history
                                        (fn [prev]
                                          (take 360 (conj prev (:player/private-stats (:client-state/player client-state))))))
                                 (js/setTimeout get-island-state 0))})))

(defonce _watcher
  (add-watch island-id
    ::watcher
    (fn [_ _ old-id new-id]
      ;; start the loop
      (when (nil? old-id)
        (get-island-state)))))
