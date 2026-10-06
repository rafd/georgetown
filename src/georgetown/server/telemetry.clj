(ns georgetown.server.telemetry
  (:require
    [taoensso.telemere :as t]))

(def signals-path "data/telemetry/signals.edn")

(defn signal->edn-line
  [signal]
  ;; not t/pr-signal-fn: it keeps :run-val, which would print the full return value of each span
  (str (pr-str (cond-> {:signal/inst (str (:inst signal))
                        :signal/id (:id signal)
                        :signal/uid (:uid signal)
                        :signal/parent-uid (:uid (:parent signal))
                        :signal/root-uid (:uid (:root signal))
                        :signal/run-nsecs (:run-nsecs signal)
                        :signal/data (:data signal)}
                 (:error signal)
                 (assoc :signal/error (ex-message (:error signal)))))
       "\n"))

(defn initialize!
  []
  (t/remove-handler! :default/console)
  (t/add-handler! :default/console
    (t/handler:console)
    {:kind-filter {:disallow #{:trace}}})
  (t/add-handler! ::file
    (t/handler:file {:path signals-path
                     :output-fn signal->edn-line
                     :interval :daily
                     :max-file-size (* 1024 1024 50)
                     :max-num-parts 8
                     :max-num-intervals 14})
    {:kind-filter {:allow #{:trace}}})
  nil)
