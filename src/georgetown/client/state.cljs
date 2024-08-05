(ns georgetown.client.state
  (:require
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

(defonce lots (r/atom nil))

(defonce user (r/atom nil))

(defn get-state []
  (ajax/request {:uri "/api/state"
                 :method :get
                 :params (when (nil? @lots)
                           {:force true})
                 :on-error (fn [_]
                             (js/setTimeout get-state 1000))
                 :on-success (fn [client-state]
                               (reset! user (:client-state/user client-state))
                               (reset! lots (:client-state/lots client-state))
                               (js/setTimeout get-state 0))}))

