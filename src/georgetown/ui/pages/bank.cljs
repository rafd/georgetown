(ns georgetown.ui.pages.bank
  (:require
    [applied-science.js-interop :as j]
    [bloom.commons.pages :as pages]
    [georgetown.ui.common :as ui]
    [georgetown.debt :as debt]
    [georgetown.client.state :as state]
    [georgetown.ui.map :as map]))

(defn loan-view
  [loan]
  [:table {:tw "border rounded"
           :style {:border-collapse "separate"
                   :border-spacing "0.5em"}}
   [:tbody
    [:tr
     [:td {:tw "text-sm"} "Amount"]
     [:td {:tw "text-right"} [ui/resource-amount (:loan/amount loan) 0 :resource/money]]]
    [:tr
     [:td {:tw "text-sm"} "Annual Interest Rate"]
     [:td {:tw "text-right"}
      [ui/value-with-icon
       (ui/format (* 100 (:loan/annual-interest-rate loan)) 2) "%"]]]
    [:tr
     [:td {:tw "text-sm"} "Minimum Daily Payment"]
     [:td {:tw "text-right"} [ui/resource-amount (:loan/minimum-daily-payment-amount loan) 0 :resource/money]]]
    (when (:loan/id loan)
      [:tr
       [:td {:tw "text-sm"} "Daily Payment"]
       [:td {:tw "text-right"}
        [:input {:tw "border px-1"
                 :type "number"
                 :name "amount"
                 :min (:loan/minimum-daily-payment-amount loan)
                 :max (:loan/amount loan)
                 :default-value (:loan/daily-payment-amount loan)
                 :on-change (fn [e]
                              (state/exec!
                                :command/set-loan-daily-payment-amount!
                                {:loan-id (:loan/id loan)
                                 :daily-payment-amount (js/parseInt (.. e -target -value))}))}]]])
    (when (:loan/id loan)
      [:<>
       [:tr
        [:td {:tw "text-sm"} "Remaining Amount"]
        [:td {:tw "text-right"}
         [ui/resource-amount (debt/remaining-amount loan) 0 :resource/money]]]
       [:tr
        [:td {:tw "text-sm"} "Remaining Payments"]
        [:td {:tw "text-right"}
         [ui/value-with-icon (ui/format (debt/remaining-payments loan) 0) "🗓️"]]]])
    [:tr
     [:td {:col-span 2}
      (if (:loan/id loan)
        [:form {:on-submit (fn [e]
                             (.preventDefault e)
                             (state/exec!
                               :command/repay-loan!
                               {:loan-id (:loan/id loan)
                                :amount (js/parseInt (j/get-in e [.-target "amount" .-value]))}))}
         [:input {:tw "border px-1"
                  :type "number"
                  :name "amount"
                  :min 1
                  :default-value (min
                                   @state/money-balance
                                   (:loan/amount loan))}]
         [ui/button {} "Repay"]]
        [:div
         [ui/button {:on-click
                     (fn [e]
                       (state/exec!
                         :command/borrow-loan!
                         {:resident-id
                          (:resident/id @state/resident)}))}
          "Borrow"]])]]]])

(defn bank-view []
  [:section
   [:h1 "Bank"]
   [:h2 "Outstanding Loans"]
   [:div {:tw "space-y-2"}
    (for [loan (->> @state/resident
                    :resident/loans)]
      ^{:key (:loan/id loan)}
      [loan-view loan])]

   [:h2 "New Loan Offer"]
   (if-let [offer (debt/next-potential-loan
                    (->> @state/resident
                         :resident/loans
                         count))]
     [loan-view offer]
     "None")])

(defn page []
  [map/page-wrapper
   [bank-view]])

(pages/register-page!
  {:page/id :page/bank
   :page/view #'page
   :page/path "/island/:island-id/bank"
   :page/parameters {:island-id :uuid}
   :page/on-enter! (fn [[_ {:keys [island-id]}]]
                     (state/set-island-id! island-id))})
