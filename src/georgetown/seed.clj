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
                                [1 "bob@example.com"]]]
      (exec! :command/authenticate-user!
             {:email email})
      (let [user-id (s/email->user-id email)]
        (exec! :command/immigrate!
               {:user-id user-id
                :island-id island-id})
        (doseq [[index improvement-type offers]
                [;; house
                 [0 :improvement.type/house {:offer/house.rental 50}]
                 [1 :improvement.type/house {:offer/house.rental 60}]
                 [2 :improvement.type/house {:offer/house.rental 70}]
                 [3 :improvement.type/house {:offer/house.rental 80}]
                 ;; farm
                 [4 :improvement.type/farm {:offer/farm.food 10}]
                 [5 :improvement.type/farm {:offer/farm.food 11}]
                 [6 :improvement.type/farm {:offer/farm.food 12}]
                 [7 :improvement.type/farm {:offer/farm.food 13}]
                 [8 :improvement.type/farm {:offer/farm.food 14}]
                 [9 :improvement.type/farm {:offer/farm.food 15}]
                 [10 :improvement.type/farm {:offer/farm.food 16}]
                 [11 :improvement.type/farm {:offer/farm.food 17}]
                 [12 :improvement.type/farm {:offer/farm.food 18}]
                 [13 :improvement.type/farm {:offer/farm.food 19}]
                 [14 :improvement.type/farm {:offer/farm.food 20}]
                 ;; big farm
                 [15 :improvement.type/big-farm {:offer/big-farm.job 100
                                                :offer/big-farm.food 105}]
                 [16 :improvement.type/big-farm {:offer/big-farm.job 200
                                                :offer/big-farm.food 205}]
                 [17 :improvement.type/big-farm {:offer/big-farm.job 300
                                                 :offer/big-farm.food 305}]
                 [18 :improvement.type/big-farm {:offer/big-farm.job 400
                                                 :offer/big-farm.food 405}]
                 ;; empty
                 [19]]]
          (let [lot-id (get-in lots [(+ (* user-index 20) index) :lot/id])]
            (exec! :command/buy-lot!
                   {:user-id user-id
                    :lot-id lot-id})
            (exec! :command/change-rate!
                   {:user-id user-id
                    :lot-id lot-id
                    :rate 5})
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
