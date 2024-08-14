(ns georgetown.ui.common
  (:require
    [georgetown.schema :as schema]
    [georgetown.client.state :as state]))

(defn resource-icon [resource-id]
  (let [resource (schema/resources resource-id)]
    [:span {:title (name resource-id)}
     (:resource/icon resource)]))

(defn resource-amount [amount & resource-ids]
  [:div {:tw "flex items-center gap-1"}
   [:span {:tw "tabular-nums"
           :style {:font-size "0.65em"}}
    (if amount
      (.toLocaleString amount)
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
                       (let [email (js/prompt "Enter your email:")]
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
