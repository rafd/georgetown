(ns georgetown.seed
  (:require
    [georgetown.db :as db]
    [georgetown.state :as s]
    [georgetown.cqrs :refer [exec!]]))

(defn seed! []
  (db/retract-all!)
  (s/initialize!)
  (let [island-id (:island/id (first (s/islands [:island/id])))
        lots (s/lots island-id)]
    (doseq [[user-index email] [[0 "alice@example.com"]
                                [1 "bob@example.com"]]]
      (exec! :command/authenticate-user!
             {:email email})
      (let [user-id (s/email->user-id email)]
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
                          :offer-amount amount}))))))))))

#_(seed!)
