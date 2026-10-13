(ns georgetown.dev.profiling
  (:require
   [clj-async-profiler.core :as prof]
   [taoensso.telemere :as t]
   [georgetown.server.db :as db]
   [georgetown.sim.tick :as tick]))

#_(t/remove-handler! :default/console)

(comment
  (let [island-id (first (db/q (db/db)
                               '[:find [?island-id ...]
                                 :where
                                 [?island :island/id ?island-id]]))]
    (tick/tick! (db/db) island-id)
    (prof/profile
     {:event :alloc}
     (dotimes [n 50]
       (println n)
       (tick/tick! (db/db) island-id))
     nil)))

#_(prof/serve-ui 8080)
