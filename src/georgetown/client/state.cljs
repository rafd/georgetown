(ns georgetown.client.state
  (:require
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

(defonce stats-history (r/atom '()))

(defonce island (r/atom nil))

(defonce resident (r/atom nil))

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

(defn get-state []
  (ajax/request {:uri "/api/state"
                 :method :get
                 :params (when (nil? @island)
                           {:force true})
                 :on-error (fn [_]
                             (js/setTimeout get-state 1000))
                 :on-success (fn [client-state]
                               (reset! resident (:client-state/resident client-state))
                               (reset! island (:client-state/island client-state))
                               (swap! stats-history (fn [prev]
                                                      (take 60 (conj prev (:island/simulator-stats (:client-state/island client-state))))))
                               (js/setTimeout get-state 0))}))

