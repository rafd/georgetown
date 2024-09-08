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
        (let [resident-id (s/->resident-id user-id [:island/id island-id])]
          (exec! :command/borrow-loan!
                 {:user-id user-id
                  :resident-id resident-id})
          (doseq [[index [improvement-type offers]]
                  (map-indexed vector
                               [;; house
                                [:improvement.type/house {:offer/house.rental 1}]
                                [:improvement.type/house {:offer/house.rental 1}]
                                [:improvement.type/house {:offer/house.rental 1}]
                                [:improvement.type/house {:offer/house.rental 1}]
                                [:improvement.type/house {:offer/house.rental 2}]
                                [:improvement.type/house {:offer/house.rental 2}]
                                [:improvement.type/house {:offer/house.rental 2}]
                                [:improvement.type/house {:offer/house.rental 2}]
                                [:improvement.type/house {:offer/house.rental 3}]
                                [:improvement.type/house {:offer/house.rental 3}]
                                ;; farm
                                [:improvement.type/farm {:offer/farm.food 20
                                                         :offer/farm.job 10}]
                                [:improvement.type/farm {:offer/farm.food 21
                                                         :offer/farm.job 11}]
                                [:improvement.type/farm {:offer/farm.food 22
                                                         :offer/farm.job 12}]
                                [:improvement.type/farm {:offer/farm.food 23
                                                         :offer/farm.job 13}]
                                [:improvement.type/farm {:offer/farm.food 24
                                                         :offer/farm.job 14}]
                                [:improvement.type/farm {:offer/farm.food 25
                                                         :offer/farm.job 15}]
                                [:improvement.type/farm {:offer/farm.food 26
                                                         :offer/farm.job 16}]
                                [:improvement.type/farm {:offer/farm.food 27
                                                         :offer/farm.job 17}]
                                ;; big farm
                                #_[:improvement.type/big-farm {:offer/big-farm.food 30
                                                               :offer/big-farm.job 20}]
                                ;; empty
                                []])]
            (let [lot-id (get-in lots [(+ (* user-index 20) index) :lot/id])]
              (exec! :command/buy-lot!
                     {:user-id user-id
                      :lot-id lot-id})
              (exec! :command/change-rate!
                     {:user-id user-id
                      :lot-id lot-id
                      :rate 1})
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
                            :offer-amount amount})))))))))))

#_(seed!)
