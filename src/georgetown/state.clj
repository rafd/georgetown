(ns georgetown.state
  (:require
    [bloom.commons.uuid :as uuid]
    [georgetown.db :as db]))

(defn initialize! []
  (let [island-uuid (uuid/random)]
    ;; create island
    (db/transact!
      [{:island/id island-uuid
        :island/population 10}])
    ;; create lots
    (db/transact!
      (for [x (range 10)
            y (range 10)]
        {:lot/id (uuid/random)
         :lot/island [:island/id island-uuid]
         :lot/x x
         :lot/y y}))))

(defn exists? [attr value]
  (some?
    (db/q '[:find ?e .
            :in $ ?attr ?value
            :where
            [?e ?attr ?value]]
          attr
          value)))

(defn population [s]
  (:state/population s))

(defn resident
  [user-id island-id]
  (db/q '[:find (pull ?resident [*]) .
          :in $ ?user-id ?island-id
          :where
          [?user :user/id ?user]
          [?island :island/id ?island-id]
          [?resident :resident/user ?user]
          [?resident :resident/island ?island]]
        user-id
        island-id))

(defn lot-island [lot-id]
  (db/q '[:find (pull ?island [*]) .
          :in $ ?lot-id
          :where
          [?lot :lot/id ?lot-id]
          [?lot :lot/island ?island]
          [?island :island/id ?island-id]]
        lot-id))

(defn lot-deed [lot-id]
  (db/q '[:find (pull ?deed [*]) .
          :in $ ?lot-id
          :where
          [?lot :lot/id ?lot-id]
          [?deed :deed/lot ?lot]]
        lot-id))

(defn lot-improvement [lot-id]
  (db/q '[:find (pull ?improvement [*]) .
          :in $ ?lot-id
          :where
          [?lot :lot/id ?lot-id]
          [?improvement :improvement/lot ?lot]]
        lot-id))

(defn owns? [user-id lot-id]
  (some?
    (db/q '[:find ?deed .
            :in $ ?lot-id ?user-id
            :where
            [?lot :lot/id ?lot-id]
            [?deed :deed/lot ?lot]
            [?deed :deed/resident ?resident]
            [?resident :resident/user ?user]
            [?user :user/id ?user-id]]
          lot-id
          user-id)))

(defn islands []
  (db/q '[:find [(pull ?island [*]) ...]
          :where
          [?island :island/id _]]))

(defn lots [island-id]
  (db/q '[:find [(pull ?lot [*]) ...]
          :in $ ?island-id
          :where
          [?island :island/id ?island-id]
          [?lot :lot/island ?island]]
        island-id))

(defn improvement-offers
  [improvement-id]
  (db/q '[:find [(pull ?offer [*]) ...]
          :in $ ?improvement-id
          :where
          [?improvement :improvement/id ?improvement-id]
          [?offer :offer/improvement ?improvement]]
        improvement-id))

(defn improvement-lot
  [improvement-id]
  (db/q '[:find (pull ?lot [*]) .
          :in $ ?improvement-id
          :where
          [?improvement :improvement/id ?improvement-id]
          [?improvement :improvement/lot ?lot]]
        improvement-id))

;; ---

(defn client-state
  [user-id]
  ;; TODO support multiple islands
  (let [island-id (:island/id (first (islands)))]
    {;; public
     :client-state/island
     (db/q '[:find
             (pull ?island [*
                            {:lot/_island
                             [*
                              {:deed/_lot [*
                                           {:deed/resident [*
                                                            {:resident/user [:user/id]}]}]}
                              {:improvement/_lot [*]}]}]) .
             :in $ ?island-id
             :where
             [?island :island/id ?island-id]]
           island-id)
     ;; private
     :client-state/resident
     (db/q '[:find (pull ?resident
                         [; resident
                          *
                          {:deed/_resident
                           [; deed
                            {:deed/lot
                             [; lot
                              {:improvement/_lot
                               [; improvement
                                {:offer/_improvement
                                 [; offer
                                  *]}]}]}]
                           }]) .
             :in $ ?user-id ?island-id
             :where
             [?user :user/id ?user-id]
             [?island :island/id ?island-id]
             [?resident :resident/user ?user]
             [?resident :resident/island ?island]]
           user-id
           island-id)}))
