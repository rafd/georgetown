(ns georgetown.state
  (:require
    [bloom.commons.uuid :as uuid]
    [georgetown.db :as db]))

(defn create-user!
  [id]
  (db/transact!
    [{:user/id id}]))

(defn population [s]
  (:state/population s))

(defn initialize! []
  ;; create lots
  (db/transact!
    (for [x (range 10)
          y (range 10)]
      {:lot/id (uuid/random)
       :lot/x x
       :lot/y y})))

(defn deed [lot-id]
  (db/q '[:find (pull ?deed [*]) .
          :in $ ?lot-id
          :where
          [?lot :lot/id ?lot-id]
          [?deed :deed/lot ?lot]]
        lot-id))

(defn exists? [attr value]
  (some?
    (db/q '[:find ?e .
            :in $ ?attr ?value
            :where
            [?e ?attr ?value]]
          attr
          value)))

(defn improvement [lot-id]
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

(defn assign! [user-id lot-id rate]
  (db/transact!
    [{:deed/id (uuid/random)
      :deed/rate rate
      :deed/lot [:lot/id lot-id]
      :deed/owner [:user/id user-id]}]))

(defn set-rate! [lot-id rate]
  (db/transact!
    [[:db/add [:lot/id lot-id] :lot/rate rate]]))

(defn refund! [lot-id]
    ;;TODO

    )

(defn lots []
  (db/q '[:find [(pull ?lot [*]) ...]
          :where
          [?lot :lot/id _]]))

(defn deeds []
  (db/q '[:find [(pull ?deed [*]) ...]
          :where
          [?deed :deed/id _]]))

#_(deeds)

(defn improvement-on-lot
  [lot-id]
  (db/q '[:find (pull ?improvement [*]) .
          :in $ ?lot-id
          :where
          [?lot :lot/id ?lot-id]
          [?improvement :improvement/lot ?lot]]
        lot-id))

(defn build!
  [lot-id improvement-type]
  (db/transact!
    [{:improvement/id (uuid/random)
      :improvement/type improvement-type
      :improvement/lot [:lot/id lot-id]}]))

(defn set-offer!
  [improvement-id offer-type amount]
  (db/transact!
    [{:offer/type offer-type
      :offer/amount amount
      :offer/improvement [:improvement/id improvement-id]}]))

(defn client-state []
  (db/q '[:find [(pull ?lot [*
                             {:deed/_lot [*
                                          {:deed/owner [*
                                                        :user/id]}]}
                             {:improvement/_lot [*]}])
                 ...]
          :where
          [?lot :lot/id _]]))
