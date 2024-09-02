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
                  (if (<= amount (:resident/money-balance resident))
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

(defn create-island! []
  (db/transact!
    [{:island/id (uuid/random)
      :island/population 10
      :island/government-money-balance 10000
      :island/citizen-money-balance 0
      :island/citizen-food-balance 1000
      :island/lots
      (for [x (range 20)
            y (range 20)]
        {:lot/id (uuid/random)
         :lot/x x
         :lot/y y})}]))

(defn initialize! []
  (register-functions!))

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

(defn all-of-type
  [id-attr pattern]
  (db/q '[:find [(pull ?e ?pattern) ...]
          :in $ ?attr ?pattern
          :where
          [?e ?attr _]]
        id-attr
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

(defn owns?
  [user-id [attr id]]
  (some?
    (case attr
      :lot/id
      (db/q '[:find ?deed .
              :in $ ?user-id ?lot-id
              :where
              [?lot :lot/id ?lot-id]
              [?lot :lot/deed ?deed]
              [?resident :resident/deeds ?deed]
              [?user :user/residents ?resident]
              [?user :user/id ?user-id]]
            user-id
            id)
      :improvement/id
      (db/q '[:find ?deed .
              :in $ ?user-id ?improvement-id
              :where
              [?improvement :improvement/id ?improvement-id]
              [?lot :lot/improvement ?improvement]
              [?lot :lot/deed ?deed]
              [?resident :resident/deeds ?deed]
              [?user :user/residents ?resident]
              [?user :user/id ?user-id]]
            user-id
            id))))

;; client state ---



#_(client-state {:user-id (:user/id (by-id [:user/email "alice@example.com"]
                                           [:user/id]))
                 :island-id (:island/id (first (all-of-type :island/id [:island/id])))})
