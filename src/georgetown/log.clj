(ns georgetown.log)

(defn info [& message]
  (apply println message)
  (tap> message))
