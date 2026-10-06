(ns georgetown.dev.seed
  (:require
   [dat.api :as dat]
   [georgetown.server.db :as db]
   [georgetown.server.state :as s]
   [georgetown.server.tada :as tada]
   [georgetown.sim.blueprints :as blueprints]
   [georgetown.sim.citizen :as citizen]
   [georgetown.sim.island :as island]
   [georgetown.sim.schema :as schema]))

(defn seed! []
  #_(db/retract-all! (db/db))
  (db/clear! (db/db))
  (db/connect!)
  (s/initialize! (db/db))
  (s/create-island! (db/db))
  nil)

(defn develop-lot!
  ;; offers: {offer-type amount}
  [{:keys [tx user-id lot-id improvement-type offers]}]
  (tada/exec! :command/buy-lot!
              {:user-id user-id
               :lot-id lot-id
               :tx tx})
  (let [deed-id (s/qget tx [:lot/id lot-id] [:lot/deed :deed/id])]
    (tada/exec! :command/change-rate!
                {:user-id user-id
                 :deed-id deed-id
                 :rate 1
                 :tx tx})
    (when improvement-type
      (tada/exec! :command/build!
                  {:user-id user-id
                   :lot-id lot-id
                   :improvement-type improvement-type
                   :tx tx})
      (let [improvement-id (:improvement/id (:lot/improvement
                                             (s/by-id tx
                                                      [:lot/id lot-id]
                                                      [{:lot/improvement [:improvement/id]}])))]
        (doseq [[offer-type amount] offers]
          (tada/exec! :command/set-offer!
                      {:user-id user-id
                       :improvement-id improvement-id
                       :offer-type offer-type
                       :offer-amount amount
                       :tx tx}))))))

(defn join-island!
  [{:keys [tx island-id email]}]
  (tada/exec! :command/authenticate-user!
              {:email email})
  (let [user-id (s/email->user-id tx email)]
    (tada/exec! :command/immigrate!
                {:user-id user-id
                 :island-id island-id
                 :tx tx})
    {:user-id user-id
     :player-id (s/->player-id tx user-id [:island/id island-id])}))

(defn seed-plus! []
  (dat/with-transaction
   [tx (db/db)]
   (let [island (first (s/all-of-type tx :island/id [:island/id
                                                          {:island/lots [:lot/id]}]))
         island-id (:island/id island)
         lots (vec (:island/lots island))]
     (doseq [[user-index email] [[0 "alice@example.com"]
                                 #_[1 "bob@example.com"]]]
       (let [{:keys [user-id player-id]} (join-island! {:tx tx
                                                        :island-id island-id
                                                        :email email})]
         ;; 6 loans = 30000; builds below cost 25000
         ;; the 5000 buffer prevents tick-1 bankruptcy (taxes + loan payments)
         (dotimes [_ 6]
           (tada/exec! :command/borrow-loan!
                       {:user-id user-id
                        :player-id player-id
                        :tx tx}))
         (doseq [[index [improvement-type offers]]
                 (map-indexed vector
                              [;; house
                               [:improvement.type/house {:offer/house.rental 1}]
                               [:improvement.type/house {:offer/house.rental 1}]
                               ;; farm
                               [:improvement.type/farm {:offer/farm.job 10}]
                               ;; market
                               [:improvement.type/food-market {:offer/food-market.job 3
                                                               :offer/food-market.offer 6}]
                               ;; park
                               [:improvement.type/park {}]
                               ;; empty lot
                               []])]
           (develop-lot! {:tx tx
                          :user-id user-id
                          :lot-id (get-in lots [(+ (* user-index 20) index) :lot/id])
                          :improvement-type improvement-type
                          :offers offers})))))))

;; ---- load testing ----

(def load-citizen-count 500)

(def load-improvement-counts
  {:improvement.type/house 60
   :improvement.type/apartment 10
   :improvement.type/farm 30
   :improvement.type/food-market 10
   :improvement.type/park 5
   :improvement.type/gym 3
   :improvement.type/library 3
   :improvement.type/pub 3})

(def load-var-amounts
  {:var/rent-rate 1
   :var/job-rate 10
   :var/food-price 6
   :var/drink-price 4
   :var/entry-fee 1
   :var/ticket-price 3
   :var/treatment-price 5})

(defn load-offers
  [improvement-type]
  (->> (:blueprint/offerables (blueprints/blueprints improvement-type))
       (keep (fn [offerable]
               ;; offerables have at most one var
               (when-let [var-id (:var/id (first (:offerable/var offerable)))]
                 [(:offerable/id offerable) (load-var-amounts var-id)])))
       (into {})))

(defn seed-load!
  []
  (let [new-island (assoc (island/generate)
                     :island/citizens (vec (repeatedly load-citizen-count
                                                       (fn []
                                                         (citizen/random ::schema/generator-immigrant)))))
        island-id (:island/id new-island)
        lot-ids (->> (:island/lots new-island)
                     (map :lot/id)
                     shuffle)
        improvement-types (->> load-improvement-counts
                               (mapcat (fn [[improvement-type n]]
                                         (repeat n improvement-type))))]
    (db/transact! (db/db) [new-island])
    (dat/with-transaction
     [tx (db/db)]
     (let [{:keys [user-id player-id]} (join-island! {:tx tx
                                                      :island-id island-id
                                                      :email "load@example.com"})]
       (db/transact! tx
         [[:db/add [:player/id player-id] :player/money-balance 100000000]])
       (doseq [[lot-id improvement-type] (map vector lot-ids improvement-types)]
         (develop-lot! {:tx tx
                        :user-id user-id
                        :lot-id lot-id
                        :improvement-type improvement-type
                        :offers (load-offers improvement-type)}))))
    island-id))

#_(seed!)
#_(seed-plus!)
#_(seed-load!)
