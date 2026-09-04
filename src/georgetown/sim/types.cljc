(ns georgetown.sim.types)

(defn key-by [f coll]
  (into {} (map (juxt f identity) coll)))

(def resources
  (->> [{:resource/id :resource/citizen
         :resource/icon "👤"
         :resource/label "citizen"
         :resource/unit-label "citizen"
         :resource/description "The island's citizens"}
        {:resource/id :resource/joy
         :resource/icon "😀"
         :resource/label "joy"
         :resource/unit-label "joy"
         :resource/description "The island's citizens' happiness"}
        {:resource/id :resource/time
         :resource/icon "⏱️"
         :resource/label "time"
         :resource/unit-label "shift"
         :resource/description "A shift; 4 shifts in a day (morning, afternoon, evening, night)"}

        {:resource/id :resource/food
         :resource/icon "🥕"
         :resource/label "food"
         :resource/unit-label "meal"
         :resource/description "1 meal for 1 person"}
        {:resource/id :resource/shelter
         :resource/icon "🛌"
         :resource/label "shelter"
         :resource/unit-label "night"
         :resource/description "1 day of shelter for 1 person"}
        {:resource/id :resource/money
         :resource/icon "💰"
         :resource/label "money"
         :resource/unit-label "dollar"
         :resource/description "currency, exchanged for other goods"}
        {:resource/id :resource/labour
         :resource/icon "👷"
         :resource/label "labour"
         :resource/unit-label "hour"
         :resource/description "1 hour of work by 1 person"}
         ]
       (key-by :resource/id)))

(def Resource
  (into [:enum] (keys resources)))

(def CitizenAttribute
  [:enum
   :citizen/physical-stress
   :citizen/mental-stress
   :citizen/skill.intellect
   :citizen/skill.fitness
   :citizen/skill.social])

(def VarId [:qualified-keyword {:namespace :var}])

(def PosInt pos-int?)

(def Blueprint
  [:map {:closed true}
   [:blueprint/id [:qualified-keyword {:namespace :improvement.type}]]
   [:blueprint/label :string]
   [:blueprint/icon :string]
   [:blueprint/description :string]
   [:blueprint/player-buildable? :boolean]
   [:blueprint/price PosInt]
   [:blueprint/stocks {:optional true}
    [:vector
     [:map {:closed true}
      [:stock/resource Resource]]]]
   [:blueprint/offerables
    [:vector
     [:and
      [:map {:closed true}
       [:offerable/id [:qualified-keyword {:namespace :offer}]]
       [:offerable/label :string]
       [:offerable/icon :string]
       [:offerable/capacity {:optional true} PosInt]
       [:offerable/time-shifts [:set [:enum
                                      :time-shift/morning
                                      :time-shift/afternoon
                                      :time-shift/evening
                                      :time-shift/night]]]
       ;; jobs have weights per skill
       ;; when combined with the skills of a specific citizen, they determine the productivity
       ;; also affect the rate at which citizen skills are improved (along with citizen talent for that skill)
       [:offerable/skill-productivity-weights {:optional true}
        [:map-of
         [:enum :citizen/skill.intellect :citizen/skill.social :citizen/skill.fitness]
         [:double {:min 0 :max 1}]]]
       [:offerable/var
        [:vector
         [:map {:closed true}
          [:var/id VarId]
          [:var/label :string]
          [:var/unit [:tuple [:enum :/] Resource Resource]]]]]
       [:offerable/effects
        [:vector
         [:tuple
          [:enum
           :effect.direction/from-citizen
           :effect.direction/to-citizen
           :effect.direction/from-player
           :effect.direction/to-player
           :effect.direction/from-self
           :effect.direction/to-self]
          [:or Resource CitizenAttribute]
          [:or number? VarId]]]]]
      [:fn {:error/message "effect refers to a var not declared in :offerable/var"}
       (fn [{:offerable/keys [var effects]}]
         (let [declared-var-ids (set (map :var/id var))]
           (->> effects
                (map (fn [[_direction _target amount]] amount))
                (filter keyword?)
                (every? declared-var-ids))))]]]]])
