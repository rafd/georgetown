(ns georgetown.seed
  (:require
    [georgetown.db :as db]
    [georgetown.state :as s]
    [georgetown.cqrs :refer [exec!]]))

(defn seed! []
  (db/retract-all!)
  (s/initialize!)
  (s/create-island!)
  (let [island (first (s/all-of-type :island/id [:island/id
                                                 {:island/lots [:lot/id]}]))
        island-id (:island/id island)
        lots (vec (:island/lots island))]
    (doseq [[user-index email] [[0 "alice@example.com"]
                                #_[1 "bob@example.com"]]]
      (exec! :command/authenticate-user!
             {:email email})
      (let [user-id (s/email->user-id email)]
        (exec! :command/immigrate!
               {:user-id user-id
                :island-id island-id})
        (doseq [[index [improvement-type offers]]
                (map-indexed vector
                             [;; house
                              [:improvement.type/house {:offer/house.rental 5}]
                              [:improvement.type/house {:offer/house.rental 5}]
                              [:improvement.type/house {:offer/house.rental 6}]
                              [:improvement.type/house {:offer/house.rental 6}]
                              [:improvement.type/house {:offer/house.rental 7}]
                              [:improvement.type/house {:offer/house.rental 7}]
                              [:improvement.type/house {:offer/house.rental 8}]
                              [:improvement.type/house {:offer/house.rental 8}]
                              [:improvement.type/house {:offer/house.rental 9}]
                              [:improvement.type/house {:offer/house.rental 9}]
                              [:improvement.type/house {:offer/house.rental 10}]
                              [:improvement.type/house {:offer/house.rental 10}]
                              ;; farm
                              [:improvement.type/farm {:offer/farm.food 10}]
                              [:improvement.type/farm {:offer/farm.food 11}]
                              [:improvement.type/farm {:offer/farm.food 12}]
                              [:improvement.type/farm {:offer/farm.food 13}]
                              [:improvement.type/farm {:offer/farm.food 14}]
                              [:improvement.type/farm {:offer/farm.food 15}]
                              [:improvement.type/farm {:offer/farm.food 16}]
                              [:improvement.type/farm {:offer/farm.food 17}]
                              [:improvement.type/farm {:offer/farm.food 18}]
                              [:improvement.type/farm {:offer/farm.food 19}]
                              ;; big farm
                              [:improvement.type/big-farm {:offer/big-farm.job 265
                                                           :offer/big-farm.food 10}]
                              ;; empty
                              []])]
          (let [lot-id (get-in lots [(+ (* user-index 20) index) :lot/id])]
            (exec! :command/buy-lot!
                   {:user-id user-id
                    :lot-id lot-id})
            (exec! :command/change-rate!
                   {:user-id user-id
                    :lot-id lot-id
                    :rate 2})
            (when improvement-type
              (exec! :command/build!
                     {:user-id user-id
                      :lot-id lot-id
                      :improvement-type improvement-type})
              (let [improvement-id (:improvement/id (:lot/improvement
                                                      (s/by-id [:lot/id lot-id]
                                                               [{:lot/improvement [:improvement/id]}])))]
                (doseq [[offer-key amount] offers]
                  (exec! :command/set-offer!
                         {:user-id user-id
                          :improvement-id improvement-id
                          :offer-type offer-key
                          :offer-amount amount}))))))))))

#_(seed!)
