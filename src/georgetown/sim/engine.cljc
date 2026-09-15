(ns georgetown.sim.engine)

;; a rule is a var whose function carries rule metadata:
;; (defn demurrage
;;   {:rule/description "..."
;;    :rule/inputs #{:world/players ...}
;;    :rule/outputs #{:world/players ...}}
;;   [inputs-map]
;;   outputs-map)
;; passed as #'demurrage (fn values lose defn metadata);
;; a map {:rule/id ... :rule/inputs ... :rule/outputs ... :rule/fn ...}
;; is also accepted
;;
;; execution order derives from data dependencies (topological sort);
;; when two rules read-modify-write the same key, declaration order breaks
;; the tie (such pairs are reported in the plan)
;;
;; a key output by 2+ rules and input by none is an "accumulator":
;; contributions are merged with `into`, in any order (eg. :world/txs)

(defn normalize-rule
  [rule]
  (if (map? rule)
    rule
    (let [{rule-ns :ns
           rule-name :name
           :rule/keys [inputs outputs description]} (meta rule)]
      (when (or (nil? inputs)
                (nil? outputs))
        (throw (ex-info "Rule is missing :rule/inputs or :rule/outputs metadata (pass a var, eg. #'my-rule)"
                        {:rule rule
                         :meta (meta rule)})))
      {:rule/id (keyword (str rule-ns) (str rule-name))
       :rule/description description
       :rule/inputs inputs
       :rule/outputs outputs
       :rule/fn rule})))

(defn key-roles
  [rules]
  (reduce
    (fn [memo [rule-index rule]]
      (let [{:rule/keys [inputs outputs]} rule]
        (reduce
          (fn [memo state-key]
            (let [role (cond
                         (and (contains? inputs state-key)
                              (contains? outputs state-key))
                         :read-modify-writers
                         (contains? outputs state-key)
                         :producers
                         :else
                         :readers)]
              (update-in memo [state-key role] (fnil conj []) rule-index)))
          memo
          (into inputs outputs))))
    {}
    (map-indexed vector rules)))

(defn accumulator-keys
  [roles]
  (->> roles
       (keep (fn [[state-key {:keys [producers read-modify-writers readers]}]]
               (when (and (<= 2 (count producers))
                          (empty? read-modify-writers)
                          (empty? readers))
                 state-key)))
       set))

(defn validate-rules!
  [rules roles]
  (let [rule-ids (map :rule/id rules)]
    (when (not= (count rule-ids) (count (set rule-ids)))
      (throw (ex-info "Duplicate rule ids"
                      {:rule-ids rule-ids}))))
  (doseq [[state-key {:keys [producers read-modify-writers readers]}] roles]
    (when (and (<= 2 (count producers))
               (or (seq read-modify-writers)
                   (seq readers)))
      (throw (ex-info "State key has multiple producers but is also read; accumulator keys must not be read"
                      {:state-key state-key
                       :producers producers
                       :read-modify-writers read-modify-writers
                       :readers readers})))))

(defn key-edges
  "For one non-accumulator key: the sole producer (if any) runs first,
   then read-modify-writers and readers serialize in declaration order
   (only reader-reader pairs stay unordered)."
  [{:keys [producers read-modify-writers readers]}]
  (let [writers (set (concat producers read-modify-writers))
        ordered (concat producers
                        (sort (concat read-modify-writers readers)))]
    (for [[position-a rule-a] (map-indexed vector ordered)
          [position-b rule-b] (map-indexed vector ordered)
          :when (and (< position-a position-b)
                     (or (contains? writers rule-a)
                         (contains? writers rule-b)))]
      [rule-a rule-b])))

(defn declaration-ordered-pairs
  "Pairs whose relative order comes from declaration order, not dataflow:
   write-after-write and write-after-read on a shared key."
  [roles accumulators]
  (->> roles
       (remove (fn [[state-key _]]
                 (contains? accumulators state-key)))
       (mapcat (fn [[state-key {:keys [producers read-modify-writers readers]}]]
                 (let [writers (set (concat producers read-modify-writers))
                       ordered (sort (concat read-modify-writers readers))]
                   (for [[position-a rule-a] (map-indexed vector ordered)
                         [position-b rule-b] (map-indexed vector ordered)
                         :when (and (< position-a position-b)
                                    (contains? writers rule-b))]
                     {:state-key state-key
                      :before rule-a
                      :after rule-b}))))))

(defn topological-order
  [rule-count edges]
  (loop [remaining (set (range rule-count))
         order []]
    (if (empty? remaining)
      order
      (let [ready (->> remaining
                       (filter (fn [rule-index]
                                 (not-any? (fn [[from-index to-index]]
                                             (and (= to-index rule-index)
                                                  (contains? remaining from-index)))
                                           edges)))
                       sort)]
        (when (empty? ready)
          (throw (ex-info "Cycle between rules"
                          {:remaining-rule-indices remaining})))
        (recur (disj remaining (first ready))
               (conj order (first ready)))))))

(defn plan
  [rules]
  (let [rules (mapv normalize-rule rules)
        roles (key-roles rules)
        accumulators (accumulator-keys roles)
        _ (validate-rules! rules roles)
        edges (->> roles
                   (remove (fn [[state-key _]]
                             (contains? accumulators state-key)))
                   (mapcat (fn [[_state-key key-role]]
                             (key-edges key-role)))
                   set)
        order (topological-order (count rules) edges)
        rule-id-at (fn [rule-index]
                     (:rule/id (nth rules rule-index)))]
    {:engine/order-indices order
     :engine/order (mapv rule-id-at order)
     :engine/accumulator-keys accumulators
     :engine/external-input-keys (->> roles
                                      (keep (fn [[state-key {:keys [producers]}]]
                                              (when (empty? producers)
                                                state-key)))
                                      set)
     :engine/declaration-ordered-pairs (->> (declaration-ordered-pairs roles accumulators)
                                            (map (fn [pair]
                                                   (-> pair
                                                       (update :before rule-id-at)
                                                       (update :after rule-id-at))))
                                            set)}))

(defn run-rule
  [state accumulators {:rule/keys [id inputs outputs] :as rule}]
  (let [result ((:rule/fn rule) (select-keys state inputs))]
    (when (not= (set (keys result)) outputs)
      (throw (ex-info "Rule returned keys that do not match its declared outputs"
                      {:rule/id id
                       :missing (remove (set (keys result)) outputs)
                       :extra (remove outputs (keys result))})))
    (reduce
      (fn [memo [state-key value]]
        (if (contains? accumulators state-key)
          (update memo state-key (fnil into []) value)
          (assoc memo state-key value)))
      state
      result)))

(defn run-debug
  [rules state]
  (let [rules (mapv normalize-rule rules)
        {:engine/keys [order-indices accumulator-keys external-input-keys]
         :as engine-plan} (plan rules)
        missing-keys (remove (fn [state-key]
                               (contains? state state-key))
                             external-input-keys)]
    (when (seq missing-keys)
      (throw (ex-info "Initial state is missing keys that no rule produces"
                      {:missing-keys missing-keys})))
    (reduce
      (fn [memo rule-index]
        (let [rule (nth rules rule-index)
              state-before (:engine/state memo)
              state-after (run-rule state-before accumulator-keys rule)]
          (-> memo
              (assoc :engine/state state-after)
              (update :engine/steps conj
                      {:rule/id (:rule/id rule)
                       :engine/inputs (select-keys state-before (:rule/inputs rule))
                       :engine/changed-keys (->> (:rule/outputs rule)
                                                 (filter (fn [state-key]
                                                           (not= (get state-before state-key)
                                                                 (get state-after state-key))))
                                                 set)}))))
      {:engine/plan engine-plan
       :engine/state state
       :engine/steps []}
      order-indices)))

(defn run
  [rules state]
  (:engine/state (run-debug rules state)))
