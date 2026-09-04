(ns georgetown.server.state
  (:require
    [clojure.string :as string]
    [dat.api :as dat]
    [georgetown.server.db :as db]
    [georgetown.sim.island :as island]
    [datalevin.interpret :as di]))

;; register functions
(defn register-functions! []
  #_:clj-kondo/ignore
  (dat/register-fn! (db/db) :fn/withdraw
    (di/inter-fn
      [db player-id amount]
      (if-let [player (datalevin.core/entity db [:player/id player-id])]
        (if (<= amount (:player/money-balance player))
          [[:db/add (:db/id player) :player/money-balance
            (- (:player/money-balance player) amount)]]
          (throw (ex-info "Insuffient funds" {})))
        (throw (ex-info (str "No player with id " player-id) {})))))
  #_:clj-kondo/ignore
  (dat/register-fn! (db/db) :fn/deposit
    (di/inter-fn
      [db player-id amount]
      (if-let [player (datalevin.core/entity db [:player/id player-id])]
        [[:db/add (:db/id player) :player/money-balance
          (+ (:player/money-balance player) amount)]]
        (throw (ex-info (str "No player with id " player-id) {})))))
  #_:clj-kondo/ignore
  (dat/register-fn! (db/db) :fn/transfer-to-government
    (di/inter-fn
      [db island-id amount]
      (if-let [island (datalevin.core/entity db [:island/id island-id])]
        [[:db/add (:db/id island) :island/government-money-balance
          (+ (:island/government-money-balance island) amount)]]
        (throw (ex-info (str "No island with id " island-id) {}))))))

(defn create-island! []
  (db/transact!
    [(island/generate)]))

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

(defn qget
  [[in-k in-v] path]
  (db/q (concat [:find (symbol (str "?" (count path))) '.
                 :in '$ '?input-k '?input-v
                 :where
                 ['?0 '?input-k '?input-v]]
                (doall
                  (for [[i k] (map-indexed vector path)]
                    (if (string/starts-with? (name k) "_")
                      [(symbol (str "?" (inc i)))
                       (keyword
                         (namespace k)
                         (subs (name k) 1))
                       (symbol (str "?" i))]
                      [(symbol (str "?" i))
                       k
                       (symbol (str "?" (inc i)))]))))
        in-k
        in-v))

#_(defn qget
  [[in-k in-v] path]
  (->> (db/q (concat [:find (list 'pull '?e
                              [{:island/_lots [:island/id]}]) '.
                      :in '$ '?input-k '?input-v
                      :where
                      ['?e '?input-k '?input-v]])
             in-k
             in-v)
       :island/_lots
       :island/id))

#_(qget [:lot/id #uuid "0191d86a-6735-714b-934b-f451f14b4cdb"]
        [:island/_lots :island/id])

;; misc helpers ----

(defn email->user-id
  [email]
  (db/q '[:find ?user-id .
          :in $ ?email
          :where
          [?user :user/email ?email]
          [?user :user/id ?user-id]]
        email))

(defn ->player-id
  [user-id [id-attr id]]
  (case id-attr
    :loan/id
    (db/q '[:find ?player-id .
            :in $ ?user-id ?loan-id
            :where
            [?user :user/id ?user-id]
            [?loan :loan/id ?loan-id]
            [?player :player/loans ?loan]
            [?player :player/id ?player-id]]
          user-id
          id)
    :island/id
    (db/q '[:find ?player-id .
            :in $ ?user-id ?island-id
            :where
            [?user :user/id ?user-id]
            [?island :island/id ?island-id]
            [?user :user/players ?player]
            [?island :island/players ?player]
            [?player :player/id ?player-id]]
          user-id
          id)
    :lot/id
    (db/q '[:find ?player-id .
            :in $ ?user-id ?lot-id
            :where
            [?lot :lot/id ?lot-id]
            [?user :user/id ?user-id]
            [?island :island/lots ?lot]
            [?user :user/players ?player]
            [?island :island/players ?player]
            [?player :player/id ?player-id]]
          user-id
          id)
    :improvement/id
    (db/q '[:find ?player-id .
            :in $ ?user-id ?improvement-id
            :where
            [?improvement :improvement/id ?improvement-id]
            [?lot :lot/improvement ?improvement]
            [?lot :lot/id ?lot-id]
            [?user :user/id ?user-id]
            [?island :island/lots ?lot]
            [?user :user/players ?player]
            [?island :island/players ?player]
            [?player :player/id ?player-id]]
          user-id
          id)))

(defn can-afford?
  [player-id amount]
  (<= amount
      (db/q '[:find ?balance .
              :in $ ?player-id
              :where
              [?player :player/id ?player-id]
              [?player :player/money-balance ?balance]]
            player-id)))

(defn owns?
  ;; TODO rename to related-to?
  [user-id [attr id]]
  (some?
    (case attr
      :deed/id
      (db/q '[:find ?deed .
              :in $ ?user-id ?deed-id
              :where
              [?deed :deed/id ?deed-id]
              [?player :player/deeds ?deed]
              [?user :user/players ?player]
              [?user :user/id ?user-id]]
            user-id
            id)

      :player/id
      (db/q '[:find ?player .
              :in $ ?user-id ?player-id
              :where
              [?player :player/id ?player-id]
              [?user :user/players ?player]
              [?user :user/id ?user-id]]
            user-id
            id)
      :loan/id
      (db/q '[:find ?loan .
              :in $ ?user-id ?loan-id
              :where
              [?loan :loan/id ?loan-id]
              [?player :player/loans ?loan]
              [?user :user/players ?player]
              [?user :user/id ?user-id]]
            user-id
            id)
      :lot/id
      (db/q '[:find ?deed .
              :in $ ?user-id ?lot-id
              :where
              [?lot :lot/id ?lot-id]
              [?lot :lot/deed ?deed]
              [?player :player/deeds ?deed]
              [?user :user/players ?player]
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
              [?player :player/deeds ?deed]
              [?user :user/players ?player]
              [?user :user/id ?user-id]]
            user-id
            id))))

;; client state ---



#_(client-state {:user-id (:user/id (by-id [:user/email "alice@example.com"]
                                           [:user/id]))
                 :island-id (:island/id (first (all-of-type :island/id [:island/id])))})
