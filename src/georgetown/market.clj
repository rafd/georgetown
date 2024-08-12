(ns georgetown.market
  (:require
    [hyperfiddle.rcf :as rcf]))

(defn market
  [demand-resource demand-amount supply-resource tenders]
  (let [tenders
        (->> tenders
             (filter (fn [tender]
                       (and
                         (= (first (tender :tender/supply)) demand-resource)
                         (= (first (tender :tender/demand)) supply-resource))))
             (map (fn [tender]
                    (assoc tender ::price
                      (/ (get-in tender [:tender/demand 1])
                         (get-in tender [:tender/supply 1])))))
             (sort-by ::price))
        total-supply-available (->> tenders
                                    (map (fn [tender]
                                           (get-in tender [:tender/supply 1])))
                                    (reduce +))]
    (if (<= total-supply-available demand-amount)
      {:market/clearing-unit-price (::price (last tenders))
       :market/amount-supplied total-supply-available
       :market/total-cost (->> tenders
                               (map (fn [tender]
                                      (* (get-in tender [:tender/supply 1])
                                         (::price tender))))
                               (reduce +))}
      (->> tenders
           (reduce (fn [memo tender]
                     (let [remaining (- demand-amount
                                        (+ (::amount-supplied memo)
                                           (get-in tender [:tender/supply 1])))]
                       (cond
                         (or (zero? remaining)
                             (neg? remaining))
                         (reduced {:market/clearing-unit-price (::price tender)
                                   :market/amount-supplied demand-amount
                                   :market/total-cost (+ (::total-cost memo)
                                                         (* (- demand-amount (::amount-supplied memo))
                                                            (::price tender)))})
                         (pos? remaining)
                         (-> memo
                             (update ::amount-supplied
                                     + (get-in tender [:tender/supply 1]))
                             (update ::total-cost
                                     + (* (get-in tender [:tender/supply 1])
                                          (::price tender)))))))
                   {::amount-supplied 0
                    ::total-cost 0})))))

(defn market-price
  [demand-resource demand-amount supply-resource tenders]
  (let [tenders
        (->> tenders
             (filter (fn [tender]
                       (and
                         (= (first (tender :tender/supply)) demand-resource)
                         (= (first (tender :tender/demand)) supply-resource))))
             (map (fn [tender]
                    (assoc tender ::price
                      (/ (get-in tender [:tender/demand 1])
                         (get-in tender [:tender/supply 1])))))
             (sort-by ::price))
        total-supply-available (->> tenders
                                    (map (fn [tender]
                                           (get-in tender [:tender/supply 1])))
                                    (reduce +))]
    (if (<= total-supply-available demand-amount)
      (::price (last tenders))
      (->> tenders
           (reduce (fn [amount-supplied tender]
                     (if (<= demand-amount (+ amount-supplied
                                              (get-in tender [:tender/supply 1])))
                       (reduced (::price tender))
                       (+ amount-supplied
                          (get-in tender [:tender/supply 1]))))
                   0))))

  #_(->> (tenders-supplying demand-resource tenders)
         (map (fn [tender]
                {:shares (second (tender :tender/supply))
                 :cash (second (tender :tender/demand))
                 :data tender}))
         (auction demand-amount)
         :purchase-cash-per-share))

(rcf/tests
  ;; demand < supply
  (market-price
    :resource/food
    1000
    :resource/money
    {:state/tenders
     [{:tender/supply [:resource/food 1000]
       :tender/demand [:resource/money 1000]
       :tender/owner :a
       :tender/id 1}
      {:tender/supply [:resource/food 1000]
       :tender/demand [:resource/money 1500]
       :tender/owner :b
       :tender/id 2}]})
  := 1000/1000

  ;; demand == supply
  (market-price
    :resource/food
    2000
    :resource/money
    {:state/tenders
     [{:tender/supply [:resource/food 1000]
       :tender/demand [:resource/money 1000]
       :tender/owner :a
       :tender/id 1}
      {:tender/supply [:resource/food 1000]
       :tender/demand [:resource/money 1500]
       :tender/owner :b
       :tender/id 2}]})
  := 1500/1000

  ;; demand > supply
  (market-price
    :resource/food
    2000
    :resource/money
    {:state/tenders
     [{:tender/supply [:resource/food 1000]
       :tender/demand [:resource/money 1000]
       :tender/owner :a
       :tender/id 1}
      {:tender/supply [:resource/food 1000]
       :tender/demand [:resource/money 1500]
       :tender/owner :b
       :tender/id 2}]})
  := 1500/1000)
