(ns georgetown.market
  (:require
    [hyperfiddle.rcf :as rcf]))

(defn market
  [demand-resource demand-amount supply-resource tenders]
  (if (= 0 demand-amount)
    {:market/clearing-unit-price nil
     :market/amount-supplied 0
     :market/successful-tenders []
     :market/total-cost 0}
    (->> tenders
         ;; filter, b/c caller of this function may pass in unrelated tenders
         (filter (fn [tender]
                   (and
                     (= (first (tender :tender/supply)) demand-resource)
                     (= (first (tender :tender/demand)) supply-resource))))
         ;; group by price
         (group-by (fn [tender]
                     (/ (get-in tender [:tender/demand 1])
                        (get-in tender [:tender/supply 1]))))
         ;; sort by price
         (sort-by key)
         (reduce (fn [memo [price tenders]]
                   (let [demand-remaining (- demand-amount
                                             (:market/amount-supplied memo))
                         amount-supplied (->> tenders
                                              (map (fn [tender]
                                                     (get-in tender [:tender/supply 1])))
                                              (reduce +))]
                     (if (< amount-supplied demand-remaining)
                       (-> memo
                           (assoc :market/clearing-unit-price price)
                           (update :market/successful-tenders concat tenders)
                           (update :market/amount-supplied + amount-supplied)
                           (update :market/total-cost + (* amount-supplied price)))
                       (reduced
                         {:market/clearing-unit-price price
                          :market/amount-supplied demand-amount
                          :market/successful-tenders
                          (concat (:market/successful-tenders memo)
                                  (let [ratio (/ demand-remaining
                                                 amount-supplied)]
                                    (->> tenders
                                         (map (fn [tender]
                                                (assoc tender
                                                  :tender/amount-supplied (* ratio (get-in tender [:tender/supply 1]))))))))
                          :market/total-cost
                          (+ (:market/total-cost memo)
                             (* demand-remaining price))}))))
                 {:market/amount-supplied 0
                  :market/successful-tenders []
                  :market/total-cost 0}))))

#_(rcf/enable!)
(rcf/tests
  ;; demand == 0
  (market
    :resource/food
    0
    :resource/money
    [{:tender/supply [:resource/food 1000]
      :tender/demand [:resource/money 1000]
      :tender/id 1}
     {:tender/supply [:resource/food 1000]
      :tender/demand [:resource/money 1500]
      :tender/id 2}])
  :=
  #:market{:clearing-unit-price nil
           :amount-supplied 0
           :successful-tenders []
           :total-cost 0}

  ;; demand < supply (exact)
  (market
    :resource/food
    1000
    :resource/money
    [{:tender/supply [:resource/food 1000]
      :tender/demand [:resource/money 1000]
      :tender/id 1}
     {:tender/supply [:resource/food 1000]
      :tender/demand [:resource/money 1500]
      :tender/id 2}])
  :=
  #:market{:clearing-unit-price 1000/1000
           :amount-supplied 1000
           :successful-tenders _
           :total-cost 1000}

  ;; demand < supply (exact, but tie)
  (market
    :resource/food
    1000
    :resource/money
    [{:tender/supply [:resource/food 1000]
      :tender/demand [:resource/money 1000]
      :tender/id 1}
     {:tender/supply [:resource/food 1000]
      :tender/demand [:resource/money 1000]
      :tender/id 2}
     {:tender/supply [:resource/food 1000]
      :tender/demand [:resource/money 2000]
      :tender/id 3}
     ])
  :=
  #:market{:clearing-unit-price 1000/1000
           :amount-supplied 1000
           :total-cost 1000
           :successful-tenders
           [{:tender/supply [:resource/food 1000]
             :tender/demand [:resource/money 1000]
             :tender/id 1
             :tender/amount-supplied 500N}
            {:tender/supply [:resource/food 1000]
             :tender/demand [:resource/money 1000]
             :tender/id 2
             :tender/amount-supplied 500N}]}

  ;; demand < supply (partial)
  #_(market
      :resource/food
      500
      :resource/money
      [{:tender/supply [:resource/food 1000]
        :tender/demand [:resource/money 1000]
        :tender/id 1}])
  :=
  #:market{:clearing-unit-price 1
           :amount-supplied 500
           :total-cost 500
           :successful-tenders
           [{:tender/supply [:resource/food 1000]
             :tender/demand [:resource/money 1000]
             :tender/id 1
             :tender/amount-supplied 500N}]}

  ;; demand < supply (partial, tie)
  (market
    :resource/food
    1500
    :resource/money
    [{:tender/supply [:resource/food 1000]
      :tender/demand [:resource/money 1000]
      :tender/id 1}
     {:tender/supply [:resource/food 1000]
      :tender/demand [:resource/money 1000]
      :tender/id 2}])
  :=
  #:market{:clearing-unit-price 1
           :amount-supplied 1500
           :total-cost 1500
           :successful-tenders
           [{:tender/supply [:resource/food 1000]
             :tender/demand [:resource/money 1000]
             :tender/id 1
             :tender/amount-supplied 750N}
            {:tender/supply [:resource/food 1000]
             :tender/demand [:resource/money 1000]
             :tender/id 2
             :tender/amount-supplied 750N}]}

  ;; demand == supply
  (market
    :resource/food
    2000
    :resource/money
    [{:tender/supply [:resource/food 1000]
      :tender/demand [:resource/money 1000]
      :tender/id 1}
     {:tender/supply [:resource/food 1000]
      :tender/demand [:resource/money 1500]
      :tender/id 2}])
  :=
  #:market{:clearing-unit-price 1500/1000
           :amount-supplied 2000
           :total-cost 2500
           :successful-tenders _}

  ;; demand > supply
  (market
    :resource/food
    2500
    :resource/money
    [{:tender/supply [:resource/food 1000]
      :tender/demand [:resource/money 1000]
      :tender/id 1}
     {:tender/supply [:resource/food 1000]
      :tender/demand [:resource/money 1500]
      :tender/id 2}])
  :=
  #:market{:clearing-unit-price 1500/1000
           :amount-supplied 2000
           :total-cost 2500
           :successful-tenders _}
  nil)
