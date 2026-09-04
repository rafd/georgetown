(ns georgetown.sim.util.ortools
  (:require
    [hyperfiddle.rcf :as rcf])
  (:import
    (com.google.ortools Loader)
    (com.google.ortools.sat CpModel
                            CpSolver
                            CpSolverStatus
                            LinearArgument
                            LinearExpr
                            Literal)))

(defonce load-natives!
  (delay (Loader/loadNativeLibraries)))

(defn weighted-sum-expr
  [bool-vars-by-id coefficient-by-var-id]
  ;; cp-sat only accepts integer coefficients
  {:pre [(every? integer? (vals coefficient-by-var-id))]}
  (LinearExpr/weightedSum
    (into-array LinearArgument
                (map bool-vars-by-id (keys coefficient-by-var-id)))
    (long-array (vals coefficient-by-var-id))))

(defn solve
  "Solves a boolean cp-sat problem given as data:
   {:ortools/bool-vars #{var-id ...}
    :ortools/constraints [[:at-most-one [var-id ...]]
                          [:<= {var-id integer-coefficient ...} integer-bound]]
    :ortools/maximize {var-id integer-coefficient ...}}
  Returns {:solve/status ... :solve/objective ... :solve/true-vars #{var-id ...}},
  or nil when infeasible."
  [{:ortools/keys [bool-vars constraints maximize]}]
  @load-natives!
  (let [model (CpModel.)
        bool-vars-by-id (->> bool-vars
                             (map (fn [var-id]
                                    [var-id (.newBoolVar model (pr-str var-id))]))
                             (into {}))]
    (doseq [[constraint-type & args] constraints]
      (case constraint-type
        :at-most-one
        (let [[var-ids] args]
          (.addAtMostOne model (into-array Literal (map bool-vars-by-id var-ids))))
        :<=
        (let [[coefficient-by-var-id bound] args]
          (.addLessOrEqual model
                           (weighted-sum-expr bool-vars-by-id coefficient-by-var-id)
                           (long bound)))))
    (.maximize model (weighted-sum-expr bool-vars-by-id maximize))
    (let [solver (CpSolver.)
          status (.solve solver model)]
      (when (contains? #{CpSolverStatus/OPTIMAL CpSolverStatus/FEASIBLE} status)
        {:solve/status (if (= CpSolverStatus/OPTIMAL status)
                         :optimal
                         :feasible)
         :solve/objective (.objectiveValue solver)
         :solve/true-vars (->> bool-vars-by-id
                               (keep (fn [[var-id bool-var]]
                                       (when (.booleanValue solver ^Literal bool-var)
                                         var-id)))
                               set)}))))

(rcf/tests
  "solve"

  "unconstrained maximize selects only positive-coefficient vars"
  (solve
    {:ortools/bool-vars #{:a :b}
     :ortools/constraints []
     :ortools/maximize {:a 5
                        :b -3}})
  :=
  {:solve/status :optimal
   :solve/objective 5.0
   :solve/true-vars #{:a}}

  "at-most-one keeps the higher-coefficient var"
  (solve
    {:ortools/bool-vars #{:a :b}
     :ortools/constraints [[:at-most-one [:a :b]]]
     :ortools/maximize {:a 2
                        :b 3}})
  :=
  {:solve/status :optimal
   :solve/objective 3.0
   :solve/true-vars #{:b}}

  "weighted-sum bound + at-most-one force a non-greedy pick"
  ;; without at-most-one, {:a :b} would win with objective 5
  (solve
    {:ortools/bool-vars #{:a :b :c}
     :ortools/constraints [[:at-most-one [:a :b]]
                           [:<= {:a 2
                                 :b 2
                                 :c 2} 4]]
     :ortools/maximize {:a 3
                        :b 2
                        :c 1}})
  :=
  {:solve/status :optimal
   :solve/objective 4.0
   :solve/true-vars #{:a :c}}

  "infeasible returns nil"
  (solve
    {:ortools/bool-vars #{:a}
     :ortools/constraints [[:<= {:a 1} -1]]
     :ortools/maximize {:a 1}})
  :=
  nil)
