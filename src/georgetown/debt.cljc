(ns georgetown.debt)

(defn loan-effective-interest [amount daily-payment-amount]
  (/ (* 365 daily-payment-amount) amount))

(defn remaining-payments
  [{:loan/keys [amount daily-payment-amount annual-interest-rate]}]
  (let [daily-interest-rate (/ annual-interest-rate 365.0)]
    (/ (Math/log (- (/ daily-payment-amount
                       (- (* daily-interest-rate amount)
                          daily-payment-amount))))
       (Math/log (+ 1 daily-interest-rate)))))

(defn remaining-amount
  [loan]
  (* (remaining-payments loan)
     (:loan/daily-payment-amount loan)))

(defn new-amount
  [{:loan/keys [amount daily-payment-amount annual-interest-rate]}]
  (- (* amount (+ 1 (/ annual-interest-rate 365.0)))
     daily-payment-amount))

#_(remaining-payments
    {:loan/amount 5000
     :loan/annual-interest-rate
     (loan-effective-interest 5000 3)
     :loan/minimum-daily-payment-amount 3
     :loan/daily-payment-amount 4})

(defn next-potential-loan [loan-count]
  ;; in real world, business loans are typically 5.5% to 29%
  ;; here, we want to peg to integer payment rates
  ;; 5000 at various rates ends up pretty nice:
  ;; 0.07
  ;; 0.15
  ;; 0.22
  ;; 0.29
  ;; 0.37
  {:loan/amount 5000
   :loan/annual-interest-rate
   (- (loan-effective-interest 5000 (inc loan-count))
      ;; decrease slightly, so that the minimum payment pays off a bit more than the interest
      0.01)
   :loan/minimum-daily-payment-amount (inc loan-count)
   :loan/daily-payment-amount (inc loan-count)})

#_(->> [0 1 2 3 4 5 6]
       (map next-loan-offer))

#_(double (loan-effective-interest 5000 1))



