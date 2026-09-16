(ns georgetown.sim.rules.citizens
  (:require
    [event.render :as-alias render]
    [georgetown.sim.blueprints :as blueprints]
    [georgetown.sim.citizen :as citizen]
    [georgetown.sim.constants :as constants]
    [georgetown.sim.schema :as schema]
    [georgetown.sim.util.math :as math]))

(defn amp-stress [citizen]
  (let [age-factor (+ 0.5 (/ (citizen/age-in-years citizen) 100))]
    (-> citizen
        (update :citizen/physical-stress
                (fn [stress]
                  (math/clamp01 (+ stress (* constants/stress-amp-base age-factor (+ 0.5 stress))))))
        (update :citizen/mental-stress
                (fn [stress]
                  (math/clamp01 (+ stress (* constants/stress-amp-base age-factor (+ 0.5 stress)))))))))

(defn decline-skills [citizen]
  (let [decline-factor (* constants/skill-decline-base
                          (+ 0.5 (/ (citizen/age-in-years citizen) 100))
                          (+ 0.5 (citizen/mean-stress citizen)))]
    (reduce (fn [citizen* skill]
              (update citizen* skill (fn [level] (math/clamp01 (* level (- 1 decline-factor))))))
            citizen
            (keys blueprints/skill->talent))))

(defn citizen-maintenance
  {:rule/description "Citizens age, accumulate stress, and lose unused skills"
   :rule/inputs #{:world/citizens}
   :rule/outputs #{:world/citizens}}
  [{:world/keys [citizens]}]
  {:world/citizens (->> citizens
                        (map (fn [[citizen-id citizen]]
                               [citizen-id (-> citizen
                                               (update :citizen/age-ticks inc)
                                               (update :citizen/residency-ticks inc)
                                               amp-stress
                                               decline-skills)]))
                        (into {}))})

(defn death-chance [citizen]
  (* constants/base-death-chance
     (+ 1 (* constants/death-stress-factor (citizen/mean-stress citizen)))
     (Math/pow (/ (+ (citizen/age-in-years citizen) 1) 40) 2)))

(defn deaths
  {:rule/description "Citizens may die, by age and stress"
   :rule/inputs #{:world/citizens}
   :rule/outputs #{:world/citizens :world/dead-citizen-ids :world/txs :world/events}}
  [{:world/keys [citizens]}]
  (let [dead-citizen-ids (->> citizens
                              vals
                              (filter (fn [citizen]
                                        (< (rand) (death-chance citizen))))
                              (map :citizen/id)
                              set)
        dead-citizens (map citizens dead-citizen-ids)]
    {:world/citizens (apply dissoc citizens dead-citizen-ids)
     :world/dead-citizen-ids dead-citizen-ids
     :world/txs (->> dead-citizen-ids
                     (mapv (fn [citizen-id]
                             [:db/retractEntity [:citizen/id citizen-id]])))
     :world/events (->> dead-citizens
                        (mapv (fn [citizen]
                                {:event/type :event.type/citizen-died
                                 :event/source :source/simulation
                                 :event/render [[::render/citizen {:citizen-id (:citizen/id citizen)
                                                                   :citizen-name (:citizen/name citizen)}]
                                                " died at age " (int (citizen/age-in-years citizen))]})))}))

(defn emigration-chance
  "Very stressed citizens are likely to leave the island."
  [citizen]
  (* constants/max-emigration-chance
     (Math/pow (citizen/mean-stress citizen) 4)))

(defn emigration
  {:rule/description "Very stressed citizens may leave the island"
   :rule/inputs #{:world/citizens}
   :rule/outputs #{:world/citizens :world/emigrant-citizen-ids :world/txs :world/events}}
  [{:world/keys [citizens]}]
  (let [emigrant-citizen-ids (->> citizens
                                  vals
                                  (filter (fn [citizen]
                                            (< (rand) (emigration-chance citizen))))
                                  (map :citizen/id)
                                  set)
        emigrant-citizens (map citizens emigrant-citizen-ids)]
    {:world/citizens (apply dissoc citizens emigrant-citizen-ids)
     :world/emigrant-citizen-ids emigrant-citizen-ids
     :world/txs (->> emigrant-citizen-ids
                     (mapv (fn [citizen-id]
                             [:db/retractEntity [:citizen/id citizen-id]])))
     :world/events (->> emigrant-citizens
                        (mapv (fn [citizen]
                                {:event/type :event.type/citizen-emigrated
                                 :event/source :source/simulation
                                 :event/render [[::render/citizen {:citizen-id (:citizen/id citizen)
                                                                   :citizen-name (:citizen/name citizen)}]
                                                " emigrated after " (int (citizen/residency-in-years citizen))
                                                " years on the island"]})))}))

(defn randomize [n odds]
  (->> (repeatedly (fn [] (< (rand) odds)))
       (take n)
       (filter true?)
       count))

(defn births
  {:rule/description "Citizens may be born, in proportion to the population"
   :rule/inputs #{:world/citizens}
   :rule/outputs #{:world/new-citizens :world/events}}
  [{:world/keys [citizens]}]
  (let [newborns (->> (repeatedly (fn []
                                    (citizen/random ::schema/generator-baby)))
                      (take (randomize (count citizens) constants/birth-chance-per-citizen-per-tick))
                      vec)]
    {:world/new-citizens newborns
     :world/events (->> newborns
                        (mapv (fn [citizen]
                                {:event/type :event.type/citizen-born
                                 :event/source :source/simulation
                                 :event/render [[::render/citizen {:citizen-id (:citizen/id citizen)
                                                                   :citizen-name (:citizen/name citizen)}]
                                                " was born"]})))}))

(defn immigration
  {:rule/description "Occasionally, a citizen immigrates to the island"
   :rule/inputs #{}
   :rule/outputs #{:world/new-citizens :world/events}}
  [_]
  (let [immigrants (if (< (rand) constants/citizen-immigration-chance)
                     [(citizen/random ::schema/generator-immigrant)]
                     [])]
    {:world/new-citizens immigrants
     :world/events (->> immigrants
                        (mapv (fn [citizen]
                                {:event/type :event.type/citizen-immigrated
                                 :event/source :source/simulation
                                 :event/render [[::render/citizen {:citizen-id (:citizen/id citizen)
                                                                   :citizen-name (:citizen/name citizen)}]
                                                 " immigrated"]})))}))

(def rules
  [#'citizen-maintenance
   #'deaths
   #'emigration])

;; declared after the money & stats rules, matching the original tick order
;; (immigration has no inputs; its position relies on declaration-order tie-breaking)
(def population-rules
  [#'births
   #'immigration])
