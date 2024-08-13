(ns georgetown.state
  (:require
    [bloom.commons.uuid :as uuid]
    [georgetown.db :as db]))

(defn initialize! []
  (db/transact!
    [{:island/id (uuid/random)
      :island/population 10
      :island/lots
      (for [x (range 10)
            y (range 10)]
        {:lot/id (uuid/random)
         :lot/x x
         :lot/y y})}]))

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
          [?user :user/residents ?resident]
          [?island :island/residents ?resident]]
        user-id
        island-id))

(defn lot-island [lot-id]
  (db/q '[:find (pull ?island [*]) .
          :in $ ?lot-id
          :where
          [?lot :lot/id ?lot-id]
          [?island :island/lots ?lot]
          [?island :island/id ?island-id]]
        lot-id))

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

(defn improvement-offers
  [improvement-id]
  (db/q '[:find [(pull ?offer [*]) ...]
          :in $ ?improvement-id
          :where
          [?improvement :improvement/id ?improvement-id]
          [?improvement :improvement/offers ?offer]]
        improvement-id))

(defn improvement-lot
  [improvement-id]
  (db/q '[:find (pull ?lot [*]) .
          :in $ ?improvement-id
          :where
          [?improvement :improvement/id ?improvement-id]
          [?lot :lot/improvement ?improvement]]
        improvement-id))

;; ---

(defn client-state
  [user-id]
  ;; TODO support multiple islands
  (let [island-id (:island/id (first (islands)))]
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
     :client-state/resident
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
                                 [*]}]}]}]
                           }]) .
             :in $ ?user-id ?island-id
             :where
             [?user :user/id ?user-id]
             [?island :island/id ?island-id]
             [?island :island/residents ?resident]
             [?user :user/residents ?resident]]
           user-id
           island-id)}))

#_(client-state georgetown.seed/primary-user-id)
