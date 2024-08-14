(ns georgetown.state
  (:require
    [bloom.commons.uuid :as uuid]
    [georgetown.db :as db]
    [datalevin.interpret :as di]))

;; register functions
(defn register-functions! []
  #_:clj-kondo/ignore
  (doseq [v [{:db/ident :fn/withdraw
              :db/fn
              (di/inter-fn
                [db resident-id amount]
                (if-let [resident (datalevin.core/entity db [:resident/id resident-id])]
                  (if (<=  amount (:resident/money-balance resident))
                    [[:db/add (:db/id resident) :resident/money-balance
                      (- (:resident/money-balance resident) amount)]]
                    (throw (ex-info "Insuffient funds" {})))
                  (throw (ex-info (str "No resident with id " resident-id) {}))))}
             {:db/ident :fn/deposit
              :db/fn (di/inter-fn
                       [db resident-id amount]
                       (if-let [resident (datalevin.core/entity db [:resident/id resident-id])]
                         [[:db/add (:db/id resident) :resident/money-balance
                           (+ (:resident/money-balance resident) amount)]]
                         (throw (ex-info (str "No resident with id " resident-id) {}))))}]]
    (georgetown.db/transact! [v])))

(defn initialize! []
  (register-functions!)
  (db/transact!
    [{:island/id (uuid/random)
      :island/population 10
      :island/lots
      (for [x (range 10)
            y (range 10)]
        {:lot/id (uuid/random)
         :lot/x x
         :lot/y y})}]))

;; generics ----

(defn exists? [attr value]
  (some?
    (db/q '[:find ?e .
            :in $ ?attr ?value
            :where
            [?e ?attr ?value]]
          attr
          value)))

(defn by-id [[id-attr id] pattern]
  (db/q '[:find (pull ?e ?pattern) .
          :in $ ?attr ?value ?pattern
          :where
          [?e ?attr ?value]]
        id-attr
        id
        pattern))

;; misc helpers ----

(defn email->user-id
  [email]
  (db/q '[:find ?user-id .
          :in $ ?email
          :where
          [?user :user/email ?email]
          [?user :user/id ?user-id]]
        email))

(defn ->resident-id
  [user-id [id-attr id]]
  (case id-attr
    :island/id
    (db/q '[:find ?resident-id .
            :in $ ?user-id ?island-id
            :where
            [?user :user/id ?user-id]
            [?island :island/id ?island-id]
            [?user :user/residents ?resident]
            [?island :island/residents ?resident]
            [?resident :resident/id ?resident-id]]
          user-id
          id)
    :lot/id
    (db/q '[:find ?resident-id .
            :in $ ?user-id ?lot-id
            :where
            [?lot :lot/id ?lot-id]
            [?user :user/id ?user-id]
            [?island :island/lots ?lot]
            [?user :user/residents ?resident]
            [?island :island/residents ?resident]
            [?resident :resident/id ?resident-id]]
          user-id
          id)
    :improvement/id
    (db/q '[:find ?resident-id .
            :in $ ?user-id ?improvement-id
            :where
            [?improvement :improvement/id ?improvement-id]
            [?lot :lot/improvement ?improvement]
            [?lot :lot/id ?lot-id]
            [?user :user/id ?user-id]
            [?island :island/lots ?lot]
            [?user :user/residents ?resident]
            [?island :island/residents ?resident]
            [?resident :resident/id ?resident-id]]
          user-id
          id)))

(defn can-afford?
  [resident-id amount]
  (<= amount
      (db/q '[:find ?balance .
              :in $ ?resident-id
              :where
              [?resident :resident/id ?resident-id]
              [?resident :resident/money-balance ?balance]]
            resident-id)))

(defn lot-deed [lot-id]
  (db/q '[:find (pull ?deed [*]) .
          :in $ ?lot-id
          :where
          [?lot :lot/id ?lot-id]
          [?lot :lot/deed ?deed]]
        lot-id))

(defn lot-improvement [lot-id]
  (db/q '[:find (pull ?improvement [*]) .
          :in $ ?lot-id
          :where
          [?lot :lot/id ?lot-id]
          [?lot :lot/improvement ?improvement]]
        lot-id))

(defn owns? [user-id lot-id]
  (some?
    (db/q '[:find ?deed .
            :in $ ?lot-id ?user-id
            :where
            [?lot :lot/id ?lot-id]
            [?lot :lot/deed ?deed]
            [?resident :resident/deeds ?deed]
            [?user :user/residents ?resident]
            [?user :user/id ?user-id]]
          lot-id
          user-id)))

(defn islands
  ([pattern]
   (db/q '[:find [(pull ?island ?pattern) ...]
           :in $ ?pattern
           :where
           [?island :island/id _]]
         pattern))
  ([]
   (islands '[*])))

(defn lots [island-id]
  (db/q '[:find [(pull ?lot [*]) ...]
          :in $ ?island-id
          :where
          [?island :island/id ?island-id]
          [?island :island/lots ?lot]]
        island-id))

(defn improvement-lot
  [improvement-id]
  (db/q '[:find (pull ?lot [*]) .
          :in $ ?improvement-id
          :where
          [?improvement :improvement/id ?improvement-id]
          [?lot :lot/improvement ?improvement]]
        improvement-id))

;; client state ---

(defn client-state
  [{:keys [user-id island-id]}]
  {;; public
   :client-state/island
   (db/q '[:find
           (pull ?island
                 ;; don't use [*] here, to avoid leaking private information
                 [:island/id
                  :island/population
                  :island/simulator-stats
                  {:island/residents
                   [:resident/id]}
                  {:island/lots
                   [:lot/id
                    :lot/x
                    :lot/y
                    {:lot/deed
                     [:deed/id
                      :deed/rate
                      {:resident/_deeds
                       [:resident/id
                        {:user/_residents
                         [:user/id]}]}]}
                    {:lot/improvement
                     [:improvement/id
                      :improvement/type]}]}]) .
           :in $ ?island-id
           :where
           [?island :island/id ?island-id]]
         island-id)
   ;; private
   :client-state/user
   (when user-id
     (db/q '[:find (pull ?user [:user/id])
             :in $ ?user-id
             :where
             [?user :user/id ?user-id]]
           user-id))
   :client-state/resident
   (when user-id
     (db/q '[:find (pull ?resident
                         [:resident/id
                          :resident/money-balance
                          {:resident/deeds
                           [:deed/id
                            {:lot/_deed
                             [:lot/id
                              {:lot/improvement
                               [:improvement/id
                                {:improvement/offers
                                 [*]}]}]}]}]) .
             :in $ ?user-id ?island-id
             :where
             [?user :user/id ?user-id]
             [?island :island/id ?island-id]
             [?island :island/residents ?resident]
             [?user :user/residents ?resident]]
           user-id
           island-id))})

#_(client-state {:user-id georgetown.seed/primary-user-id
                 :island-id (:island/id (first (islands)))})
