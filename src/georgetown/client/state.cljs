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

(defonce user (r/atom nil))

(defonce residency
  (r/reaction (first (:residency/_user @user))))

(defonce money-balance
  (r/reaction (:residency/money-balance @residency)))

(defonce offers (r/reaction
                  (->> @user
                       (x/select
                         [:deed/_owner
                          x/ALL
                          :deed/lot
                          :improvement/_lot
                          x/ALL
                          :offer/_improvement
                          x/ALL]))))

(defn get-state []
  (ajax/request {:uri "/api/state"
                 :method :get
                 :params (when (nil? @island)
                           {:force true})
                 :on-error (fn [_]
                             (js/setTimeout get-state 1000))
                 :on-success (fn [client-state]
                               (reset! user (:client-state/user client-state))
                               (reset! island (:client-state/island client-state))
                               (swap! stats-history (fn [prev]
                                                      (take 60 (conj prev (:island/simulator-stats (:client-state/island client-state))))))
                               (js/setTimeout get-state 0))}))

