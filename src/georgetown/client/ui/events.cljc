(ns georgetown.client.ui.events)

(defn citizen-label [data]
  (or (:citizen-name data)
      (subs (str (:citizen-id data)) 24 36)))

(defn player-label [player-id]
  (subs (str player-id) 0 8))

(defn lot-label [data]
  (str "(" (:lot-x data) "," (:lot-y data) ")"))

(defmulti render :event/type)

(defmethod render :event.type/built
  [{:event/keys [data]}]
  [:span "Player " (player-label (:player-id data))
   " built a " (name (:improvement-type data))
   " at " (lot-label data)])

(defmethod render :event.type/lot-purchased
  [{:event/keys [data]}]
  [:span "Player " (player-label (:player-id data))
   " bought the lot at " (lot-label data)
   (when-let [previous-owner-id (:previous-owner-player-id data)]
     (str " from player " (player-label previous-owner-id)))])

(defmethod render :event.type/lot-abandoned
  [{:event/keys [data]}]
  [:span "The lot at " (lot-label data) " was abandoned"])

(defmethod render :event.type/demolished
  [{:event/keys [data]}]
  [:span "The " (name (:improvement-type data))
   " at " (lot-label data) " was demolished"])

(defmethod render :event.type/player-joined
  [{:event/keys [data]}]
  [:span "Player " (player-label (:player-id data)) " joined the island"])

(defmethod render :event.type/player-bankrupt
  [{:event/keys [data]}]
  [:span "Player " (player-label (:player-id data)) " went bankrupt"])

(defmethod render :event.type/loan-borrowed
  [{:event/keys [data]}]
  [:span "You borrowed " (:amount data) " from the bank"])

(defmethod render :event.type/loan-paid-off
  [_event]
  [:span "You paid off a loan"])

(defmethod render :event.type/citizen-born
  [{:event/keys [data]}]
  [:span "Citizen " (citizen-label data) " was born"])

(defmethod render :event.type/citizen-immigrated
  [{:event/keys [data]}]
  [:span "Citizen " (citizen-label data) " immigrated"])

(defmethod render :event.type/citizen-emigrated
  [{:event/keys [data]}]
  [:span "Citizen " (citizen-label data)
   " emigrated at age " (:age-years data)])

(defmethod render :event.type/citizen-died
  [{:event/keys [data]}]
  [:span "Citizen " (citizen-label data)
   " died at age " (:age-years data)])

;; events with retired types survive in the db until pruned
(defmethod render :default
  [{:event/keys [type]}]
  [:span (str type)])
