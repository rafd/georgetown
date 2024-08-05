(ns georgetown.state
  (:require
    [bloom.commons.uuid :as uuid]
    [georgetown.db :as db]))

(defn initialize! []
  ;; create lots
  (db/transact!
    (for [x (range 10)
          y (range 10)]
      {:lot/id (uuid/random)
       :lot/x x
       :lot/y y})))

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
            [?deed :deed/owner ?owner]
            [?owner :user/id ?user-id]]
          lot-id
          user-id)))

(defn lots []
  (db/q '[:find [(pull ?lot [*]) ...]
          :where
          [?lot :lot/id _]]))

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
  {;; public
   :client-state/lots
   (db/q '[:find [(pull ?lot [*
                              {:deed/_lot [*
                                           {:deed/owner [*
                                                         :user/id]}]}
                              {:improvement/_lot [*]}])
                  ...]
           :where
           [?lot :lot/id _]])
   ;; private
   :client-state/user
   (db/q '[:find (pull ?user
                       [; user
                        *
                        {:deed/_owner
                         [; deed
                          {:deed/lot
                           [; lot
                            {:improvement/_lot
                             [; improvement
                              {:offer/_improvement
                                 [; offer
                                  *]}]}]}]
                         }]) .
           :in $ ?user-id
           :where
           [?user :user/id ?user-id]]
         user-id)})
