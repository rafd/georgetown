(ns georgetown.dev.seed
  (:require
   [georgetown.server.db :as db]
   [georgetown.server.state :as s]
   [georgetown.server.tada :as tada]))

(defn seed! []
  #_(db/retract-all!)
  (db/clear!)
  (db/connect!)
  (s/initialize!)
  (s/create-island!)
  nil)

(defn seed-plus! []
  (let [island (first (s/all-of-type :island/id [:island/id
                                                 {:island/lots [:lot/id]}]))
        island-id (:island/id island)
        lots (vec (:island/lots island))]
    (doseq [[user-index email] [[0 "alice@example.com"]
                                #_[1 "bob@example.com"]]]
      (tada/exec! :command/authenticate-user!
                  {:email email})
      (let [user-id (s/email->user-id email)]
        (tada/exec! :command/immigrate!
                    {:user-id user-id
                     :island-id island-id})
        (let [player-id (s/->player-id user-id [:island/id island-id])]
          ;; 6 loans = 30000; builds below cost 25000
          ;; the 5000 buffer prevents tick-1 bankruptcy (taxes + loan payments)
          (dotimes [_ 6]
            (tada/exec! :command/borrow-loan!
                        {:user-id user-id
                         :player-id player-id}))

          (doseq [[index [improvement-type offers]]
                  (map-indexed vector
                               [;; house
                                [:improvement.type/house {:offer/house.rental {:var/rent-rate 1}}]
                                [:improvement.type/house {:offer/house.rental {:var/rent-rate 1}}]
                                ;; farm
                                [:improvement.type/farm {:offer/farm.job {:var/job-rate 10}}]
                                ;; market
                                [:improvement.type/food-market {:offer/food-market.job {:var/job-rate 3}
                                                                :offer/food-market.offer {:var/food-price 6}}]
                                ;; park
                                [:improvement.type/park {}]
                                ;; empty lot
                                []])]
            (let [lot-id (get-in lots [(+ (* user-index 20) index) :lot/id])]
              (tada/exec! :command/buy-lot!
                          {:user-id user-id
                           :lot-id lot-id})
              (let [deed-id (s/qget [:lot/id lot-id] [:lot/deed :deed/id])]
                (tada/exec! :command/change-rate!
                            {:user-id user-id
                             :deed-id deed-id
                             :rate 1})
                (when improvement-type
                  (tada/exec! :command/build!
                              {:user-id user-id
                               :lot-id lot-id
                               :improvement-type improvement-type})
                  (let [improvement-id (:improvement/id (:lot/improvement
                                                         (s/by-id [:lot/id lot-id]
                                                                  [{:lot/improvement [:improvement/id]}])))]
                    (doseq [[offer-type vars] offers]
                      ;; :command/set-offer! assumes only a single var per offer
                      (tada/exec! :command/set-offer!
                                  {:user-id user-id
                                   :improvement-id improvement-id
                                   :offer-type offer-type
                                   :offer-amount (val (first vars))}))))))))))))

#_(seed!)
#_(seed-plus!)
