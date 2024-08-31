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
                               (reduce +))
       :market/successful-tenders (map (fn [tender] (dissoc tender ::price)) tenders)}
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
                                   ;; TODO handle partial tender success
                                   :market/successful-tenders (conj (::successful-tenders memo)
                                                                   (dissoc tender ::price))
                                   :market/total-cost (+ (::total-cost memo)
                                                         (* (- demand-amount (::amount-supplied memo))
                                                            (::price tender)))})
                         (pos? remaining)
                         (-> memo
                             (update ::successful-tenders conj (dissoc tender ::price))
                             (update ::amount-supplied
                                     + (get-in tender [:tender/supply 1]))
                             (update ::total-cost
                                     + (* (get-in tender [:tender/supply 1])
                                          (::price tender)))))))
                   {::amount-supplied 0
                    ::successful-tenders []
                    ::total-cost 0})))))

#_(rcf/enable!)
(rcf/tests
  ;; demand = 0
  (market
    :resource/food
    0
    :resource/money
    [{:tender/supply [:resource/food 1000]
      :tender/demand [:resource/money 1000]
      :tender/owner :a
      :tender/id 1}
     {:tender/supply [:resource/food 1000]
      :tender/demand [:resource/money 1500]
      :tender/owner :b
      :tender/id 2}])
  :=
  #:market{:clearing-unit-price nil
           :amount-supplied 0
           :successful-tenders []
           :total-cost 0}

  ;; demand < supply
  (market
    :resource/food
    1000
    :resource/money
    [{:tender/supply [:resource/food 1000]
      :tender/demand [:resource/money 1000]
      :tender/owner :a
      :tender/id 1}
     {:tender/supply [:resource/food 1000]
      :tender/demand [:resource/money 1500]
      :tender/owner :b
      :tender/id 2}])
  :=
  #:market{:clearing-unit-price 1000/1000
           :amount-supplied 1000
           :successful-tenders _
           :total-cost 1000}

  ;; demand == supply
  (market
    :resource/food
    2000
    :resource/money
    [{:tender/supply [:resource/food 1000]
      :tender/demand [:resource/money 1000]
      :tender/owner :a
      :tender/id 1}
     {:tender/supply [:resource/food 1000]
      :tender/demand [:resource/money 1500]
      :tender/owner :b
      :tender/id 2}])
  :=
  #:market{:clearing-unit-price 1500/1000
           :amount-supplied 2000
           :total-cost 2500
           :successful-tenders _}

  ;; demand > supply
  (market
    :resource/food
    2000
    :resource/money
    [{:tender/supply [:resource/food 1000]
      :tender/demand [:resource/money 1000]
      :tender/owner :a
      :tender/id 1}
     {:tender/supply [:resource/food 1000]
      :tender/demand [:resource/money 1500]
      :tender/owner :b
      :tender/id 2}])
  :=
  #:market{:clearing-unit-price 1500/1000
           :amount-supplied 2000
           :total-cost 2500
           :successful-tenders _}
  nil)
