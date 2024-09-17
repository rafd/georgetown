(ns georgetown.market
  (:require
    [hyperfiddle.rcf :as rcf]))

(defn market
  [demand-resource demand-amount supply-resource max-supply-amount tenders]
  (let [tenders (->> tenders
                     ;; filter, b/c caller of this function may pass in unrelated tenders
                     (filter (fn [tender]
                               (and
                                 (= (first (tender :tender/supply)) demand-resource)
                                 (= (first (tender :tender/demand)) supply-resource)))))]
    (if (= 0 demand-amount)
      {:market/clearing-unit-price nil
       :market/demand-filled 0
       :market/tenders
       (->> tenders
            (map (fn [tender]
                   (assoc tender
                     :tender/fill-ratio 0
                     :tender/fill-amount 0))))
       :market/supply-consumed 0}
      (->> tenders
           ;; group by price
           (group-by (fn [tender]
                       ;; supply amount possibly 0 due to upstream utilization set to 0
                       (when-not (zero? (second (tender :tender/supply)))
                         (/ (get-in tender [:tender/demand 1])
                            (get-in tender [:tender/supply 1])))))
           ;; sort by price
           (sort-by key)
           (reduce (fn [memo [price tenders]]
                     (let [demand-remaining (- demand-amount
                                               (:market/demand-filled memo))
                           demand-filled (->> tenders
                                              (map (fn [tender]
                                                     (get-in tender [:tender/supply 1])))
                                              (reduce +))
                           supply-remaining (- max-supply-amount
                                               (:market/supply-consumed memo))
                           supply-consumed (->> tenders
                                                (map (fn [tender]
                                                       (get-in tender [:tender/demand 1])))
                                                (reduce +))]
                       (cond
                         (or
                           ;; may be nil (see note above)
                           (nil? price)
                           ;; may have filled demand, but keep going to return rest of tenders
                           (memo ::done?))
                         (-> memo
                             (update :market/tenders
                                     concat
                                     (->> tenders
                                          (map (fn [tender]
                                                 (assoc tender
                                                   :tender/fill-ratio 0
                                                   :tender/fill-amount 0))))))

                         ;; filled, but maybe partially
                         (or (<= demand-remaining demand-filled)
                             (<= supply-remaining supply-consumed))
                         (let [ratio (min (/ demand-remaining
                                             demand-filled)
                                          (/ supply-remaining
                                             supply-consumed))
                               ;; requiring integer amounts of non-money
                               ratio (if (zero? ratio)
                                       0
                                       (* ratio
                                          (cond
                                            (= :resource/money supply-resource)
                                            (/ (quot (* ratio demand-filled) 1)
                                               (* ratio demand-filled))
                                            (= :resource/money demand-resource)
                                            (/ (quot (* ratio supply-consumed) 1)
                                               (* ratio supply-consumed))
                                            :else
                                            1)))]
                           (-> memo
                               (assoc ::done? true)
                               (assoc :market/clearing-unit-price price)
                               (update :market/demand-filled + (* ratio demand-filled))
                               (update :market/supply-consumed + (* ratio supply-consumed))
                               (update :market/tenders concat
                                       (->> tenders
                                            (map (fn [tender]
                                                   (assoc tender
                                                     :tender/fill-ratio ratio
                                                     :tender/fill-amount (* ratio (get-in tender [:tender/supply 1])))))))))

                         ;; filled completely, move on
                         :else
                         (-> memo
                             (assoc :market/clearing-unit-price price)
                             (update :market/tenders
                                     concat
                                     (->> tenders
                                          (map (fn [tender]
                                                 (assoc tender
                                                   :tender/fill-ratio 1
                                                   :tender/fill-amount
                                                   (get-in tender [:tender/supply 1]))))))
                             (update :market/demand-filled + demand-filled)
                             (update :market/supply-consumed + supply-consumed)))))
                   {:market/demand-filled 0
                    :market/supply-consumed 0
                    :market/tenders []})
           (#(dissoc % ::done?))))))

#_(rcf/enable!)

(rcf/tests
  "market"
  "demand == 0"
  (market
    :resource/food
    0
    :resource/money
    ##Inf
    [{:tender/supply [:resource/food 1000]
      :tender/demand [:resource/money 1000]
      :tender/id 1}
     {:tender/supply [:resource/food 1000]
      :tender/demand [:resource/money 1500]
      :tender/id 2}])
  :=
  #:market{:clearing-unit-price nil
           :demand-filled 0
           :tenders [{:tender/supply [:resource/food 1000]
                      :tender/demand [:resource/money 1000]
                      :tender/id 1
                      :tender/fill-ratio 0
                      :tender/fill-amount 0}
                     {:tender/supply [:resource/food 1000]
                      :tender/demand [:resource/money 1500]
                      :tender/id 2
                      :tender/fill-ratio 0
                      :tender/fill-amount 0}]
           :supply-consumed 0}

  "demand < supply (exact)"
  (market
    :resource/food
    1000
    :resource/money
    ##Inf
    [{:tender/supply [:resource/food 1000]
      :tender/demand [:resource/money 1000]
      :tender/id 1}
     {:tender/supply [:resource/food 1000]
      :tender/demand [:resource/money 1500]
      :tender/id 2}])
  :=
  #:market{:clearing-unit-price 1000/1000
           :demand-filled 1000
           :tenders _
           :supply-consumed 1000}

  "demand < supply (exact, but tie)"
  (market
    :resource/food
    1000
    :resource/money
    ##Inf
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
           :demand-filled 1000
           :supply-consumed 1000
           :tenders
           [{:tender/supply [:resource/food 1000]
             :tender/demand [:resource/money 1000]
             :tender/id 1
             :tender/fill-ratio 500/1000
             :tender/fill-amount 500N}
            {:tender/supply [:resource/food 1000]
             :tender/demand [:resource/money 1000]
             :tender/id 2
             :tender/fill-ratio 500/1000
             :tender/fill-amount 500N}
            {:tender/supply [:resource/food 1000]
             :tender/demand [:resource/money 2000]
             :tender/id 3
             :tender/fill-ratio 0
             :tender/fill-amount 0}]}

  "demand < supply (partial)"
  (market
    :resource/food
    500
    :resource/money
    ##Inf
    [{:tender/supply [:resource/food 1000]
      :tender/demand [:resource/money 1000]
      :tender/id 1}])
  :=
  #:market{:clearing-unit-price 1
           :demand-filled 500
           :supply-consumed 500
           :tenders
           [{:tender/supply [:resource/food 1000]
             :tender/demand [:resource/money 1000]
             :tender/id 1
             :tender/fill-ratio 1/2
             :tender/fill-amount 500N}]}

  "demand < supply (partial, tie)"
  (market
    :resource/food
    1500
    :resource/money
    ##Inf
    [{:tender/supply [:resource/food 1000]
      :tender/demand [:resource/money 1000]
      :tender/id 1}
     {:tender/supply [:resource/food 1000]
      :tender/demand [:resource/money 1000]
      :tender/id 2}])
  :=
  #:market{:clearing-unit-price 1
           :demand-filled 1500
           :supply-consumed 1500
           :tenders
           [{:tender/supply [:resource/food 1000]
             :tender/demand [:resource/money 1000]
             :tender/id 1
             :tender/fill-ratio 750/1000
             :tender/fill-amount 750N}
            {:tender/supply [:resource/food 1000]
             :tender/demand [:resource/money 1000]
             :tender/id 2
             :tender/fill-ratio 750/1000
             :tender/fill-amount 750N}]}

  "demand == supply"
  (market
    :resource/food
    2000
    :resource/money
    ##Inf
    [{:tender/supply [:resource/food 1000]
      :tender/demand [:resource/money 1000]
      :tender/id 1}
     {:tender/supply [:resource/food 1000]
      :tender/demand [:resource/money 1500]
      :tender/id 2}])
  :=
  #:market{:clearing-unit-price 1500/1000
           :demand-filled 2000
           :supply-consumed 2500
           :tenders _}

  "demand > supply"
  (market
    :resource/food
    2500
    :resource/money
    ##Inf
    [{:tender/supply [:resource/food 1000]
      :tender/demand [:resource/money 1000]
      :tender/id 1}
     {:tender/supply [:resource/food 1000]
      :tender/demand [:resource/money 1500]
      :tender/id 2}])
  :=
  #:market{:clearing-unit-price 1500/1000
           :demand-filled 2000
           :supply-consumed 2500
           :tenders _}


  "limited $"
  "exact"
  (market
    :resource/food
    ##Inf
    :resource/money
    1000
    [{:tender/supply [:resource/food 1000]
      :tender/demand [:resource/money 1000]
      :tender/id 1}
     {:tender/supply [:resource/food 2000]
      :tender/demand [:resource/money 3000]
      :tender/id 2}])
  :=
  #:market{:clearing-unit-price 1000/1000
           :demand-filled 1000
           :supply-consumed 1000
           :tenders [{:tender/supply [:resource/food 1000]
                      :tender/demand [:resource/money 1000]
                      :tender/id 1
                      :tender/fill-amount 1000
                      :tender/fill-ratio 1}
                     {:tender/supply [:resource/food 2000]
                      :tender/demand [:resource/money 3000]
                      :tender/id 2
                      :tender/fill-amount 0
                      :tender/fill-ratio 0}]}

  "partway - one"
  (market
    :resource/food
    ##Inf
    :resource/money
    1500
    [{:tender/supply [:resource/food 1000]
      :tender/demand [:resource/money 1000]
      :tender/id 1}
     {:tender/supply [:resource/food 2000]
      :tender/demand [:resource/money 4000]
      :tender/id 2}])
  :=
  #:market{:clearing-unit-price 4000/2000
           :demand-filled (+ 1000 (* 500/4000 2000))
           :supply-consumed 1500
           :tenders [{:tender/supply [:resource/food 1000]
                      :tender/demand [:resource/money 1000]
                      :tender/id 1
                      :tender/fill-amount 1000
                      :tender/fill-ratio 1}
                     {:tender/supply [:resource/food 2000]
                      :tender/demand [:resource/money 4000]
                      :tender/id 2
                      :tender/fill-amount (* 500/4000 2000)
                      :tender/fill-ratio (/ 500 4000)}]}

  "partway - tied"
  (market
    :resource/food
    ##Inf
    :resource/money
    600
    [{:tender/supply [:resource/food 2000]
      :tender/demand [:resource/money 4000]
      :tender/id 2}
     {:tender/supply [:resource/food 1000]
      :tender/demand [:resource/money 2000]
      :tender/id 3}])
  :=
  #:market{:clearing-unit-price 4000/2000
           :demand-filled (* 600/4000 2000)
           :supply-consumed 600
           :tenders [{:tender/supply [:resource/food 2000]
                      :tender/demand [:resource/money 4000]
                      :tender/id 2
                      :tender/fill-amount (* 2000 (/ 600 6000))
                      :tender/fill-ratio (/ 600 6000)}
                     {:tender/supply [:resource/food 1000]
                      :tender/demand [:resource/money 2000]
                      :tender/id 3
                      :tender/fill-amount (* 1000 (/ 600 6000))
                      :tender/fill-ratio (/ 600 6000)}
                     ]}

  nil)
