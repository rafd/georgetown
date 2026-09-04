(ns georgetown.client.ui.pages.home
  (:require
    [reagent.core :as r]
    [bloom.commons.pages :as pages]
    [georgetown.client.ui.common :as ui]
    [georgetown.client.state :as state]
    [georgetown.client.ui.iso :as iso]))

(defn islands-view []
  (r/with-let
   [islands (state/exec-atom! :query/islands {})]
   [:div {:tw "flex flex-wrap gap-4"}
     (for [island @islands]
       ^{:key (:island/id island)}
       [:a
        {:tw "block relative"
         :href (pages/path-for [:page/island {:island-id (:island/id island)}])}
        [iso/iso-view island]
        [:div {:tw "absolute top-0 right-0"}
         (count (:island/players island))
         "👑"]
        [:div {:tw "absolute bottom-0 right-0"}
         (apply str (drop 30 (str (:island/id island))))]])]))

(defn view []
  [:div
   [:p "Welcome to Georgetown!"]
   [:p "Georgetown is a multiplayer persistent town building game. (Think: SimCity, but, multiplayer and always on)."]
   [:p
    "It's barely playable yet, but in the spirit of 'release early; release often', it's deployed here."]
   [:p "Warning: game state may be erased occasionally."]
   [:p "How does it work? " [:a {:tw "underline"
                                 :href "https://github.com/rafd/georgetown/blob/master/README.md"} "Read the 'rules' here."]]
   [:p "Tell me what I should fix or build or whatever " [:a {:tw "underline"
                                                              :href "https://discord.gg/FdPus82t4b"} "on the Discord"]]
   [:p "If you're curious, check out " [:a {:tw "underline"
                                            :href "https://github.com/rafd/georgetown"} "the source"] "."]
   [:p "Click on an island below to play:"]
   [islands-view]

   [ui/button
    {:on-click (fn []
                 (-> (state/exec! :command/create-island! {})
                     (.then (fn []
                              (js/window.location.reload)))))}
    "🌋 Spawn a new island"]])

(pages/register-page!
  {:page/id :page/home
   :page/view #'view
   :page/path "/"
   :page/on-enter! (fn [_]
                     (state/set-island-id! nil))})
