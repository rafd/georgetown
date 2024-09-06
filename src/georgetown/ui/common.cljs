(ns georgetown.ui.common
  (:require
    [georgetown.schema :as schema]
    [georgetown.client.state :as state]))

(defn pie [opts percent]
  [:svg (assoc opts :height 20 :width 20 :view-box "0 0 20 20")
   [:circle {:cx 10
             :cy 10
             :r 10
             :fill (or (:bg-color opts) "black")}]
   [:circle {:cx 10
             :cy 10
             :r 5
             :fill "transparent"
             :stroke (or (:fg-color opts) "white")
             :stroke-width 10
             :transform "rotate(-90) translate(-20)"
             :stroke-dasharray (str (* 31.4 percent) " 31.4")}]])

(defn format [n fraction-digits]
  (when n
    (.toLocaleString
      n
      "en-US"
      #js {:minimumFractionDigits fraction-digits
           :maximumFractionDigits fraction-digits})))

(defn resource-icon [resource-id]
  (when resource-id
    (let [resource (schema/resources resource-id)]
      [:span {:title (name resource-id)}
       (:resource/icon resource)])))

(defn value-with-icon
  [value icon]
  [:div {:tw "inline-flex items-center gap-1"}
   [:span {:tw "tabular-nums"
           :style {:font-size "0.65em"}}
    value]
   [:span {:style {:font-size "0.65em"}}
    icon]])

(defn resource-amount [amount sig-figs & resource-ids]
  [:div {:tw "inline-flex items-center gap-1"}
   [:span {:tw "tabular-nums"
           :style {:font-size "0.65em"}}
    (if amount
      (format amount sig-figs)
      "?")]
   (into [:<>]
         (interpose [:span {:style {:font-size "0.6em"}} " / "]
                    (for [resource-id resource-ids]
                      ^{:key resource-id}
                      [:span {:style {:font-size "0.65em"}}
                       [resource-icon resource-id]])))])

(defn button [opts & content]
  [:button (assoc opts
             :tw "bg-gray-500 text-white px-1")
   (into [:<>]
         content)])

(defn login-button []
  [button {:on-click (fn []
                       ;; temporary authentication method
                       (let [email (js/prompt "Enter a long random string (but save it somewhere if you want to log back in).")]
                         (-> (state/exec! :command/alpha.authenticate-user!
                                          {:email (str email "@example.com")
                                           :url js/location.pathname})
                             (.then
                               (fn [response]
                                 (set! js/window.location (:url response))))
                             (.catch
                               (fn []
                                 (js/alert "Something went wrong.")))))

                       #_(let [email (js/prompt "Enter your email:")]
                         (-> (state/exec! :command/authenticate-user!
                                          {:email email
                                           :url js/location.pathname})
                             (.then
                               (fn []
                                 (js/alert "Check your email for a login link.")))
                             (.catch
                               (fn []
                                 (js/alert "Something went wrong."))))))}
   "Log In to Play"])

(defn join-island-button
  [island-id]
  [button {:on-click (fn []
                       (state/exec! :command/immigrate!
                                    {:island-id island-id}))}
   "Join this Island"])
