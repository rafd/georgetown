(ns georgetown.client.ui.events
  (:require
    [bloom.commons.pages :as pages]
    [event.render :as-alias render]
    [georgetown.client.state :as state]
    [georgetown.client.ui.common :as ui]
    [georgetown.sim.blueprints :as blueprints]))

(defn player-view
  [{:keys [player-id]}]
  ;; no player page exists, so this is not a link
  [:span {:title (str player-id)}
   (subs (str player-id) 0 8)])

(defn citizen-view
  [{:keys [citizen-id citizen-name]}]
  [:a {:tw "underline"
       :href (pages/path-for [:page/citizen {:island-id @state/island-id
                                             :citizen-id citizen-id}])}
   (ui/citizen-display-name {:citizen/id citizen-id
                             :citizen/name citizen-name})])

(defn lot-view
  [{:keys [lot-id lot-x lot-y]}]
  [:a {:tw "underline"
       :href (pages/path-for [:page/lot {:island-id @state/island-id
                                         :lot-id lot-id}])}
   (str "(" lot-x "," lot-y ")")])

(defn improvement-view
  [{:keys [improvement-type]}]
  (if-let [blueprint (blueprints/blueprints improvement-type)]
    [:span {:tw "whitespace-nowrap"}
     (:blueprint/icon blueprint) " " (:blueprint/label blueprint)]
    ;; retired improvement types survive in stored events
    [:span (name improvement-type)]))

(defn money-view
  [{:keys [amount]}]
  ;; not ui/resource-amount: it emits a div, and event rows render inside a span
  [:span {:tw "whitespace-nowrap"}
   (ui/format amount 0) " " [ui/resource-icon :resource/money]])

(def node-views
  {::render/player #'player-view
   ::render/citizen #'citizen-view
   ::render/lot #'lot-view
   ::render/improvement #'improvement-view
   ::render/money #'money-view})

(defn render
  [event]
  (if-let [nodes (seq (:event/render event))]
    (into [:<>]
          (map (fn [node]
                 (if (vector? node)
                   (let [[node-type attributes] node]
                     (if-let [node-view (node-views node-type)]
                       [node-view attributes]
                       [:span {:tw "text-red-600"} (str node-type)]))
                   node)))
          nodes)
    ;; events stored before :event/render existed, and retired types
    [:span (str (:event/type event))]))
