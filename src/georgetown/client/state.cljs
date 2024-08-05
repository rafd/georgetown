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

(-> (exec! :query/all {})
    (.then (fn [client-state]
             (reset! user (:client-state/user client-state))
             (reset! lots (:client-state/lots client-state)))))
