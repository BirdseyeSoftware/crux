(ns crux.domain
  (:require [slingshot.slingshot :refer (throw+)])
  (:require [crux.util
             :refer (unquoted?
                     defrecord-dynamically
                     defrecord-keep-meta)]))

(defprotocol ICruxSpec
  (-validate-spec [this]))

(defrecord-keep-meta CruxDefaults
  [^{:type map.of.Symbol->AbstractFieldType} field-types
   ^{:type list.of.Fn->AbstractFieldType} type-inference-rules])

(defrecord-keep-meta EventReductionSpec
  [^{:type :Keyword} name
   ^{:type :variable} args])

(defrecord-keep-meta EventSpec
  [^{:type Symbol} name
   ^{:type list.of.Symbol} fields
   ^{:type list.of.EventReductionSpec} reducer-specs
   ^{:type CruxDefaults} defaults
   ])

(defrecord-keep-meta CommandSpec
  [^{:type Symbol} name
   ^{:type list.of.Symbol} fields
   ^{:type CruxDefaults} defaults
   ])

(defn -validate-entity-spec [entity-spec]
  (when-not (and (vector? (:fields entity-spec))
                   (every? symbol? (:fields entity-spec)))
      (throw+ {:type :crux/invalid-entity-fields
               :msg (str entity-spec)}))
    (doseq [[event-symbol event-map] (:events entity-spec)]
      (when-not (and (vector? (:fields event-map))
                     (every? symbol? (:fields event-map)))
        (throw+ {:type :crux/invalid-event-fields
                 :msg (str event-map)})))
    entity-spec)

(defrecord-keep-meta EntitySpec
  [^{:type Symbol} name
   ^{:type Symbol} plural-name
   ^{:type CruxDefaults} defaults
   ^{:type Map} id-field
   ^{:type list.of.Symbol} fields
   ^{:type list.of.EntityProperty} properties
   ^{:type map.of.Symbol->EventSpec} events
   ^{:type map.of.Symbol->CommandSpec} commands]

  ICruxSpec
  (-validate-spec [this] (-validate-entity-spec this)))

(defn -validate-domain-spec [domain-spec]
  ;; it's better to do this in each of the macros so the error
  ;; reporting is closer to the original source of the error
  (doseq [[entity-symbol entity-spec] (:entities domain-spec)]
    (-validate-spec entity-spec))
  domain-spec)

(defrecord-keep-meta DomainSpec
    [^{:type Symbol} name
     ^{:type CruxDefaults} defaults
     ^{:type map.of.Symbol->EntitySpec} entities]

  ICruxSpec
  (-validate-spec [this] (-validate-domain-spec this)))

;;;;
;;; Crux DDL: minor fns/macros

(defmacro create-domain [initial-spec & body]
  `(do
     (assert (instance? DomainSpec ~initial-spec))
     (-> ~initial-spec ~@body -validate-domain-spec)))

(defmacro defdomain [domain-name & body]
  `(def ~domain-name
     (create-domain (map->DomainSpec {:name '~domain-name
                                      :defaults (map->CruxDefaults {})})
       ~@body)))

(defn -quote-or-unquote-fields-form [fields owner]
  (cond
    (vector? fields) `'[~@fields]
    (unquoted? fields) (second fields)
    :else (throw+
           (java.lang.IllegalArgumentException.
            (format "Crux: Illegal fields def '%s' for %s"
                    fields owner)))))

(defmacro entity [domain-spec
                  entity-name entity-fields & body]
  (let [fields (-quote-or-unquote-fields-form entity-fields entity-name)]
    `(let [entity-spec# (map->EntitySpec {:defaults (:defaults ~domain-spec)
                                         :name ~(name entity-name)
                                         :fields ~fields})]
       (update-in ~domain-spec [:entities] assoc '~entity-name
                  (-> entity-spec# ~@body -validate-spec)))))

(defn- throw-aready-declared-error [declaration-type sym owner-type owner]
  (throw+
   (java.lang.IllegalArgumentException.
    (format "Crux: %s '%s' already declared for %s '%s'"
            declaration-type sym owner-type owner))))

(def event-reduce-strategy-validators
  {:inc (fn not-using [entity-spec event-symbol event-fields args]
          (assert (= 1 (count args)) args)
          (let [field-name (symbol (name (first args)))]
            (assert (not (keyword? field-name)))
            (assert (some #{field-name}
                          (:fields entity-spec))
                    (str (:fields entity-spec) " - " field-name)))
          true),
   :merge (constantly true)
   :conj (constantly true)
   :assoc (constantly true)
   :dissoc (constantly true)
   })

(defn validate-event-reducer-spec
  [entity-spec event-symbol event-fields {:keys [name args]}]
  (assert (keyword? name))
  (let [validator (event-reduce-strategy-validators name)]
    (assert validator (str name))
    (assert (validator entity-spec event-symbol event-fields
                       args)
            (str name " validation failed"))))

(defn -norm-event-reduce-spec [spec]
  (let [spec (cond (keyword? spec) [spec]
                   (map? spec) [:merge spec]
                   :else spec)
        [spec-name & spec-args] spec]
    (map->EventReductionSpec {:name spec-name
                              :args spec-args})))

(defn event* [entity-spec
              [event-symbol command-symbol event-fields reduce-specs]]
  (when (get-in entity-spec [:events event-symbol])
    (throw-aready-declared-error
     'event event-symbol 'entity (:name entity-spec)))
  (when (get-in entity-spec [:commands command-symbol])
    (throw-aready-declared-error
     'command command-symbol 'entity (:name entity-spec)))

  (let [event-spec (map->EventSpec
                    {:defaults (:defaults entity-spec)
                     :name event-symbol
                     :fields event-fields})
        reduce-specs (map -norm-event-reduce-spec reduce-specs)
        command-spec (assoc (map->CommandSpec event-spec)
                       :name command-symbol)
        event-spec (assoc event-spec :reducer-specs reduce-specs)]
    (doseq [spec reduce-specs]
      (validate-event-reducer-spec entity-spec event-symbol
                                   event-fields spec))
    (-> entity-spec
        (update-in [:events] assoc event-symbol event-spec)
        (update-in [:commands] assoc command-symbol command-spec))))

(defn- quote-event-spec-form
  [[event-symbol command-symbol event-fields & reduce-spec]]
  (let [event-fields (-quote-or-unquote-fields-form event-fields event-symbol)]
    `['~event-symbol '~command-symbol ~event-fields '~reduce-spec]))

(defmacro events [entity-spec & event-specs]
  (let [event-specs (into [] (map quote-event-spec-form event-specs))]
    `(doall (reduce event* ~entity-spec ~event-specs))))

;;;;;;;;;
;;; Crux DDL: minor fns/macros

(defn field-types* [m field-types-map]
  (update-in m [:defaults :field-types] merge field-types-map))

(defmacro field-types
  [m field-types-map]
  (cond
    (unquoted? field-types-map) `(field-types* ~m ~(second field-types-map))
    (map? field-types-map) `(field-types* ~m '~field-types-map)
    :else (throw+
           (java.lang.IllegalArgumentException.
            (format "Crux: Illegal field-types '%s'"
                    field-types-map)))))

(defn plural* [entity-spec plural-symbol]
  (assoc entity-spec :plural plural-symbol))

(defmacro plural [entity-spec plural-symbol]
  `(plural* ~entity-spec '~plural-symbol))

(defn type-infer-rule [entity-spec regex abstract-type]
  (update-in
   entity-spec [:defaults :type-inference-rules]
   conj [regex abstract-type]))

(defn properties [entity-spec properties-map]
  (update-in entity-spec [:properties] merge properties-map))

(defn id-field* [entity-spec id-field-name & [id-type]]
  (assoc entity-spec :id-field {:name id-field-name
                               :type (or id-type :guid)}))
(defmacro id-field [entity-spec id-field-name & [id-type]]
  `(id-field* ~entity-spec '~id-field-name ~id-type))

(defmacro command-validators [entity-spec & actions] entity-spec)
