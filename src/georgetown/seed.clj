(ns georgetown.seed
  (:require
    [georgetown.db :as db]
    [georgetown.state :as s]
    [georgetown.cqrs :refer [exec!]]))

(def primary-user-id #uuid "614a34a6-4505-40e9-858b-581a0d26602a")

(defn seed! []
  (db/retract-all!)
  (s/initialize!)
  (let [island-id (:island/id (first (s/islands [:island/id])))
        lots (s/lots island-id)]
    (doseq [[user-index user-id] [[0 primary-user-id]
                                  [1 #uuid "856f5e27-f1ee-443d-884d-f398afe9b49d"]]]
      (exec! :command/create-user!
             {:id user-id})
      (exec! :command/immigrate!
             {:user-id user-id
              :island-id island-id})
      (doseq [[index improvement-type offers] [[0 :improvement.type/house {:offer/house.rental 100}]
                                               [1 :improvement.type/house {:offer/house.rental 150}]
                                               [2 :improvement.type/house {:offer/house.rental 200}]
                                               [3 :improvement.type/house {:offer/house.rental 250}]
                                               [4 :improvement.type/farm {:offer/farm.job 100
                                                                          :offer/farm.food 10}]
                                               [5 :improvement.type/farm {:offer/farm.job 200
                                                                          :offer/farm.food 20}]
                                               [6 :improvement.type/farm {:offer/farm.job 300
                                                                          :offer/farm.food 30}]
                                               [7 :improvement.type/farm {:offer/farm.job 400
                                                                          :offer/farm.food 40}]
                                               [8]
                                               [9]]]
        (let [lot-id (get-in lots [(+ (* user-index 10) index) :lot/id])]
          (exec! :command/buy-lot!
                 {:user-id user-id
                  :lot-id lot-id})
          (when improvement-type
            (exec! :command/build!
                   {:user-id user-id
                    :lot-id lot-id
                    :improvement-type improvement-type})
            (let [improvement-id (:improvement/id (s/lot-improvement lot-id))]
              (doseq [[offer-key amount] offers]
                (exec! :command/set-offer!
                       {:user-id user-id
                        :improvement-id improvement-id
                        :offer-type offer-key
                        :offer-amount amount})))))))))

#_(seed!)
