(ns georgetown.ui.common
  (:require
    [georgetown.schema :as schema]
    [georgetown.client.state :as state]))

(defn format [n fraction-digits]
  (when n
    (.toLocaleString
      n
      "en-US"
      #js {:minimumFractionDigits fraction-digits
           :maximumFractionDigits fraction-digits})))

(defn resource-icon [resource-id]
  (let [resource (schema/resources resource-id)]
    [:span {:title (name resource-id)}
     (:resource/icon resource)]))

(defn resource-amount [amount & resource-ids]
  [:div {:tw "inline-flex items-center gap-1"}
   [:span {:tw "tabular-nums"
           :style {:font-size "0.65em"}}
    (if amount
      (format amount 0)
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
