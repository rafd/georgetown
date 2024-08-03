(ns georgetown.seed
  (:require
    [georgetown.db :as db]
    [georgetown.state :as s]
    [georgetown.cqrs :refer [exec!]]))

(defn seed! []
  (db/retract-all!)
  (s/initialize!)
  (let [lots (s/lots)]
    (let [user-id #uuid "614a34a6-4505-40e9-858b-581a0d26602a"]
      ;; user 1
      (exec! :command/create-user!
             {:id user-id})
      ;; lot 1.1 - home
      (let [lot-id (get-in lots [0 :lot/id])]
        (exec! :command/buy-lot!
               {:user-id user-id
                :lot-id lot-id
                :rate 1})
        (exec! :command/change-rate!
               {:user-id user-id
                :lot-id lot-id
                :rate 2})
        (exec! :command/build!
               {:user-id user-id
                :lot-id lot-id
                :improvement-type :improvement.type/house})
        (let [house-id (:improvement/id (s/improvement-on-lot lot-id))]
          (exec! :command/set-offer!
                 {:user-id user-id
                  :improvement-id house-id
                  :offer-type :offer/house.rental
                  :offer-amount 1})))
      ;; lot 1.2 - farm
      (let [lot-id (get-in lots [1 :lot/id])]
        (exec! :command/buy-lot!
               {:user-id user-id
                :lot-id lot-id
                :rate 1})
        (exec! :command/build!
               {:user-id user-id
                :lot-id lot-id
                :improvement-type :improvement.type/farm})
        (let [farm-id (:improvement/id (s/improvement-on-lot lot-id))]
          (exec! :command/set-offer!
                 {:user-id user-id
                  :improvement-id farm-id
                  :offer-type :offer/farm.job
                  :offer-amount 1})
          (exec! :command/set-offer!
                 {:user-id user-id
                  :improvement-id farm-id
                  :offer-type :offer/farm.food
                  :offer-amount 1}))))
    (let [user-id #uuid "856f5e27-f1ee-443d-884d-f398afe9b49d"]
      ;; user 2
      (exec! :command/create-user!
             {:id user-id})
      ;; lot 2.1 - home
      (let [lot-id (get-in lots [2 :lot/id])]
        (exec! :command/buy-lot!
               {:user-id user-id
                :lot-id lot-id
                :rate 1})
        (exec! :command/build!
               {:user-id user-id
                :lot-id lot-id
                :improvement-type :improvement.type/house})
        (let [house-id (:improvement/id (s/improvement-on-lot lot-id))]
          (exec! :command/set-offer!
                 {:user-id user-id
                  :improvement-id house-id
                  :offer-type :offer/house.rental
                  :offer-amount 1})))
      ;; lot 2 - farm
      (let [lot-id (get-in lots [3 :lot/id])]
        (exec! :command/buy-lot!
               {:user-id user-id
                :lot-id lot-id
                :rate 1})
        (exec! :command/build!
               {:user-id user-id
                :lot-id lot-id
                :improvement-type :improvement.type/farm})
        (let [farm-id (:improvement/id (s/improvement-on-lot lot-id))]
          (exec! :command/set-offer!
                 {:user-id user-id
                  :improvement-id farm-id
                  :offer-type :offer/farm.job
                  :offer-amount 1})
          (exec! :command/set-offer!
                 {:user-id user-id
                  :improvement-id farm-id
                  :offer-type :offer/farm.food
                  :offer-amount 1}))))))

#_(seed!)
