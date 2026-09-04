(ns georgetown.dev.gen
  (:require
   [georgetown.server.db :as db]
   [georgetown.sim.island :as island]))

(defn replace! [seed]
  (let [new-island (island/generate seed)
        island-id (db/q
                    '[:find ?id .
                      :where
                      [?e :island/id ?id]])
        ->id (->> (db/q
                    '[:find ?id ?x ?y
                      :where
                      [?e :lot/id ?id]
                      [?e :lot/x ?x]
                      [?e :lot/y ?y]])
                  (map (fn [[id x y]]
                         [[x y] id]))
                  (into {}))]
    (db/transact!
      (conj
        (->> (:island/lots new-island)
             (map (fn [lot]
                    (assoc lot :lot/id (->id [(lot :lot/x) (lot :lot/y)])))))
        {:island/id island-id
         :island/seed (:island/seed new-island)}))
    nil))

#_(replace! (rand-int 5000))

;; favorite seeds:
;; 2712
;; 4778
;; 3154
;; 4343

#_(georgetown.server.db/q
    '[:find ?seed .
      :where
      [_ :island/seed ?seed]])

#_(georgetown.dev.seed/seed!)
