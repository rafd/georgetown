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

(defonce resident (r/atom nil))

(defonce stats-history (r/atom '()))

(defonce money-balance
  (r/reaction (:resident/money-balance @resident)))

(defonce offers (r/reaction
                  (->> @resident
                       (x/select
                         [:resident/deeds
                          x/ALL
                          :lot/_deed
                          x/ALL
                          :lot/improvement
                          :improvement/offers
                          x/ALL]))))

(defn set-island-id! [id]
  (when (not= id @island-id)
    (reset! island-id id)
    (reset! island nil)
    (reset! resident nil)
    (reset! stats-history '())))

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
                                 (reset! resident (:client-state/resident client-state))
                                 (reset! island (:client-state/island client-state))
                                 (swap! stats-history (fn [prev]
                                                        (take 60 (conj prev (:island/simulator-stats (:client-state/island client-state))))))
                                 (js/setTimeout get-island-state 0))})))

(defonce _watcher
  (add-watch island-id
    ::watcher
    (fn [_ _ old-id new-id]
      ;; start the loop
      (when (nil? old-id)
        (get-island-state)))))
