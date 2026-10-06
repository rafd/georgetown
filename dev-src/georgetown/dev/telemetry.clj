(ns georgetown.dev.telemetry
  (:require
   [clojure.edn :as edn]
   [clojure.java.io :as io]
   [clojure.string :as string])
  (:import
   [java.time Instant]
   [java.util.zip GZIPInputStream]))

;; reads the span files written by georgetown.server.telemetry
;; usage:
;;   bb -cp dev-src -m georgetown.dev.telemetry [dir] [--since 2026-10-06T12:00:00Z]
;;   (summarize {:dir "data/telemetry"})

(def default-dir "data/telemetry")

(defn signal-files
  [dir]
  (->> (.listFiles (io/file dir))
       (filter (fn [file]
                 (string/starts-with? (.getName file) "signals.edn")))
       (sort-by (fn [file]
                  (.getName file)))))

(defn read-file-signals
  [file]
  (with-open [reader (io/reader (if (string/ends-with? (.getName file) ".gz")
                                  (GZIPInputStream. (io/input-stream file))
                                  (io/input-stream file)))]
    (->> (line-seq reader)
         (remove string/blank?)
         (mapv edn/read-string))))

(defn read-signals
  [{:keys [dir since]}]
  (cond->> (mapcat read-file-signals (signal-files dir))
    since
    (filter (fn [signal]
              (not (.isBefore (Instant/parse (:signal/inst signal))
                              (Instant/parse since)))))))

(defn signal-label
  [signal]
  (case (:signal/id signal)
    :tick/rule (get-in signal [:signal/data :rule/id])
    :command (get-in signal [:signal/data :command])
    (:signal/id signal)))

(defn signal-paths
  "uid -> vector of labels from the root span down to the signal"
  [signals]
  (let [signal-by-uid (->> signals
                           (map (fn [signal]
                                  [(:signal/uid signal) signal]))
                           (into {}))
        path-of (fn path-of [signal]
                  (let [parent (signal-by-uid (:signal/parent-uid signal))]
                    (conj (if parent
                            (path-of parent)
                            [])
                          (signal-label signal))))]
    (->> signals
         (map (fn [signal]
                [(:signal/uid signal) (path-of signal)]))
         (into {}))))

(defn children-nsecs
  [signals]
  (->> signals
       (filter :signal/parent-uid)
       (group-by :signal/parent-uid)
       (map (fn [[parent-uid children]]
              [parent-uid (reduce + (map :signal/run-nsecs children))]))
       (into {})))

(defn percentile
  [sorted-values fraction]
  (nth sorted-values (int (* fraction (dec (count sorted-values))))))

(defn nsecs->ms
  [nsecs]
  (/ nsecs 1e6))

(defn stats
  [values]
  (let [sorted-values (vec (sort values))]
    {:stat/count (count sorted-values)
     :stat/mean (/ (reduce + sorted-values) (count sorted-values))
     :stat/p50 (percentile sorted-values 0.5)
     :stat/p95 (percentile sorted-values 0.95)
     :stat/max (peek sorted-values)}))

(defn step-summaries
  [signals]
  (let [uid->path (signal-paths signals)
        uid->children-nsecs (children-nsecs signals)]
    (->> signals
         (map-indexed (fn [index signal]
                        (let [run-ms (nsecs->ms (:signal/run-nsecs signal))]
                          {:step/path (uid->path (:signal/uid signal))
                           :step/rule? (= :tick/rule (:signal/id signal))
                           :step/index index
                           :step/run-ms run-ms
                           :step/self-ms (- run-ms
                                            (nsecs->ms (get uid->children-nsecs (:signal/uid signal) 0)))
                           :step/error? (some? (:signal/error signal))})))
         (group-by :step/path)
         (map (fn [[path steps]]
                {:step/path path
                 ;; signals are written when spans end (children first), so order by the first seen
                 :step/first-index (apply min (map :step/index steps))
                 :step/rule? (:step/rule? (first steps))
                 :step/errors (count (filter :step/error? steps))
                 :step/run (stats (map :step/run-ms steps))
                 :step/self-mean (:stat/mean (stats (map :step/self-ms steps)))})))))

(defn ordered-steps
  "Depth-first, so each step comes after its parent"
  [steps]
  (let [children-by-parent (group-by (fn [step]
                                       (pop (:step/path step)))
                                     steps)
        sort-siblings (fn [siblings]
                        (if (every? :step/rule? siblings)
                          (sort-by (fn [step]
                                     (- (:stat/mean (:step/run step))))
                                   siblings)
                          (sort-by :step/first-index siblings)))
        walk (fn walk [path]
               (mapcat (fn [step]
                         (cons step (walk (:step/path step))))
                       (sort-siblings (children-by-parent path))))]
    (walk [])))

(defn summarize
  [{:keys [dir since]
    :or {dir default-dir}}]
  (let [signals (read-signals {:dir dir
                               :since since})]
    (->> (ordered-steps (step-summaries signals))
         (group-by (fn [step]
                     (first (:step/path step))))
         (sort-by (fn [[pathway _]]
                    (str pathway))))))

(defn format-ms
  [ms]
  (format "%9.2f" (double ms)))

(defn step-name
  [path]
  (let [label (peek path)]
    (str (apply str (repeat (dec (count path)) "  "))
         (if (keyword? label)
           (if (#{"tick" "push" "command"} (namespace label))
             (str (namespace label) "/" (name label))
             (name label))
           (str label)))))

(defn print-summary!
  [pathway-steps]
  (doseq [[pathway steps] pathway-steps]
    (println)
    (println (str "== " pathway " =="))
    (println (format "%-44s %7s %9s %9s %9s %9s %9s %6s"
                     "step (ms)" "n" "mean" "p50" "p95" "max" "self" "errors"))
    (doseq [{:step/keys [path run self-mean errors]} steps]
      (println (format "%-44s %7d %s %s %s %s %s %6d"
                       (step-name path)
                       (:stat/count run)
                       (format-ms (:stat/mean run))
                       (format-ms (:stat/p50 run))
                       (format-ms (:stat/p95 run))
                       (format-ms (:stat/max run))
                       (format-ms self-mean)
                       errors)))))

(defn -main
  [& args]
  (let [[positional options] (split-with (fn [arg]
                                           (not (string/starts-with? arg "--")))
                                         args)
        options (->> options
                     (partition 2)
                     (map (fn [[k v]]
                            [(keyword (subs k 2)) v]))
                     (into {}))]
    (print-summary! (summarize {:dir (or (first positional) default-dir)
                                :since (:since options)}))))

#_(print-summary! (summarize {}))
