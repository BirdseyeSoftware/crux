(ns crux.domain
  (:require [crux.internal.keys :refer :all])
  (:require [slingshot.slingshot :refer (throw+)])
  (:require [crux.util
             :refer [eval-with-meta
                     unquoted?
                     map-over-keys
                     defrecord-dynamically
                     defrecord-keep-meta]]))

(defn check [{:keys [entity event user]}
             validation-value error-msg]
  (when (not validation-value)
    error-msg))

(defn crux-properties-from-meta [entity]
  (CRUX-PROPERTIES (meta entity)))

;; TODO
(declare add-map-of-command->events-to-domain-spec)

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
     ^{:type list.of.EventReductionSpec} reduce-forms
     ^{:type Fn } reducer
     ^{:type CruxDefaults} defaults
     ])

(defrecord-keep-meta CommandSpec
    [^{:type Symbol} name
     ^{:type list.of.Symbol} fields
     ^{:type CruxDefaults} defaults
     ])

(defn -validate-entity-spec [entity-spec]
  (when-not (and (vector? (FIELDS entity-spec))
                 (every? symbol? (FIELDS entity-spec)))
    (throw+ {:type :crux/invalid-entity-fields
             :msg (str entity-spec)}))
  (doseq [[event-symbol event-map] (EVENTS entity-spec)]
    (when-not (and (vector? (FIELDS event-map))
                   (every? symbol? (FIELDS event-map)))
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
  (doseq [[entity-symbol entity-spec] (ENTITIES domain-spec)]
    (-validate-spec entity-spec))
  domain-spec)

(defrecord DomainSpec
    [^{:type Symbol} name
     ^{:type CruxDefaults} defaults
     ^{:type map.of.Symbol->EntitySpec} entities
     ;; event-records: crux.reify/blah
     ;; event-store: crux.reify.savant
     ^{:type '{Symbol Symbol}} reifications]

  ICruxSpec
  (-validate-spec [this] (-validate-domain-spec this)))

;;;;
;;; Crux DDL: minor fns/macros

(defn -ensure-is-crux-domain-spec [m]
  (if (instance? DomainSpec m)
    m
    (map->DomainSpec m)))

(defn -seed-domain-with-crux-defaults [domain-spec]
  (if-let [provided-defaults (DEFAULTS domain-spec)]
    (if-not (instance? CruxDefaults provided-defaults)
      (assoc domain-spec DEFAULTS (map->CruxDefaults provided-defaults))
      domain-spec)
    (assoc domain-spec DEFAULTS (map->CruxDefaults {}))))

(defmacro create-domain [initial-spec & body]
  `(do
     (assert (:name ~initial-spec))
     (-> ~initial-spec
         -ensure-is-crux-domain-spec
         -seed-domain-with-crux-defaults
         ~@body
         -validate-domain-spec)))

(defmacro defdomain [domain-name & body]
  `(def ~domain-name
     (create-domain {:name ~domain-name}
       ~@body)))

(defn -quote-or-unquote-fields-form [fields owner]
  (cond
    (vector? fields) `'[~@fields]
    (unquoted? fields) (second fields)
    (= fields 'entity-fields) 'entity-fields
    :else (throw+
           (java.lang.IllegalArgumentException.
            (format "Crux: Illegal fields def '%s' for %s"
                    fields owner)))))



(defmacro entity [domain-spec
                  entity-name entity-fields & body]
  (let [fields (-quote-or-unquote-fields-form entity-fields entity-name)]
    `(let [entity-spec# (map->EntitySpec {DEFAULTS (DEFAULTS ~domain-spec)
                                          :name ~(name entity-name)
                                          ENTITY-SYMBOL '~(symbol entity-name)
                                          FIELDS ~fields})]
       (-> ~domain-spec
           (update-in [ENTITIES] assoc '~entity-name
                      (-> entity-spec# ~@body -validate-spec))))))

(defn- throw-aready-declared-error [declaration-type sym owner-type owner]
  (throw+
   (java.lang.IllegalArgumentException.
    (format "Crux: %s '%s' already declared for %s '%s'"
            declaration-type sym owner-type owner))))

(defn- throw-reduce-forms-required-error [event-symbol]
  (throw+
   (java.lang.IllegalArgumentException.
    (format "Crux: %s '%s' requires a reduce form"
            event-symbol))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; entity/event/command properties (unary functions)

(defn gen-entity-property-fn
  [entity-spec property-symbol entity-properties form]
  ;; NOTE: Allow the entity# record to have a CRUX-PROPERTIES and that
  ;; would be a map of {Sym Fn}
  (let [property-symbols (keys entity-properties)
        entity-symbol (ENTITY-SYMBOL entity-spec)]
      (eval-with-meta
        `(fn ~property-symbol [entity#]
           (let [{:keys [~@(FIELDS entity-spec)]} entity#
                 ~(symbol "entity") entity#

                 {:keys [~@property-symbols]}
                 (map-over-keys
                  keyword (crux-properties-from-meta
                           ~(symbol "entity")))
                 
                 ]
             ~form))
        {:doc (format "Property %s generated by crux for %s"
                      property-symbol entity-symbol)
         :crux/generated-by 'crux.domain/gen-entity-property-fn})))

(defn gen-event-property-fn
  [event-spec property-symbol event-properties form]
  ;; NOTE: Allow the event# record to have a CRUX-PROPERTIES and that
  ;; would be a map of {Sym Fn}
  (let [event-symbol (EVENT-SYMBOL event-spec)
        property-symbols (keys event-properties)]
    (eval-with-meta
      `(fn ~property-symbol [event#]
         (let [{:keys [~@(FIELDS event-spec)]} event#
               ~(symbol "event") event#

               {:keys [~@property-symbols]}
               (map-over-keys
                keyword (crux-properties-from-meta
                         ~(symbol "event")))]
           ~form))
      {:doc (format "Property %s generated by crux for %s"
                    property-symbol event-symbol)
       :crux/generated-by 'crux.domain/gen-event-property-fn})))

(defmulti gen-property-fn (fn [spec sym properties-map form] (type spec)))
(defmethod gen-property-fn EntitySpec [spec sym properties-map form]
  (gen-entity-property-fn spec sym properties-map form))
(defmethod gen-property-fn EventSpec [spec sym properties-map form]
  (gen-event-property-fn spec sym properties-map form))

(defn properties* [spec properties-map]
  (-> spec
      (update-in [PROPERTY-FORMS] merge properties-map)
      ;; first declare them so props can use other props
      (update-in [PROPERTIES] merge properties-map)
      ;; then define them
      (update-in [PROPERTIES] merge 
                 (into {}
                       (for [[sym form] properties-map]
                         [sym (gen-property-fn
                                spec
                                sym properties-map form)])))))

(defmacro properties [spec & property-pairs]
  ;;TODO: normalize property-pairs
  `(let [properties-map# (into {} (map #(into [] %)
                                       (partition 2 '~property-pairs)))]
     (properties* ~spec properties-map#)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn gen-event-reducer [entity-spec event-symbol event-fields forms]
  (let [entity-symbol (ENTITY-SYMBOL entity-spec)
        entity-property-symbols (keys (PROPERTIES entity-spec))
        event-reducer-symbol (symbol (format "%s-%s-reduce"
                                             entity-symbol
                                             event-symbol))]
    (eval-with-meta
      `(fn ~event-reducer-symbol [entity#
                                  {:keys [~@event-fields] :as event#}]
         (let [~(symbol "entity") entity#
               ~(symbol "event") (vary-meta event#
                                            assoc
                                            CRUX-PROPERTIES
                                            (CRUX-PROPERTIES (meta entity#)))
               {~(symbol "entity-id") :oid} entity#
               {~(symbol "event-id") :oid} event#
               {:keys [~@entity-property-symbols]}
               (map-over-keys
                keyword (crux-properties-from-meta
                         ~(symbol "entity")))
               ]
           (-> entity#
               ~@forms)))
      {:crux/generated-from 'crux.domain/gen-event-reducer
       :doc (format
             "Event reducer generated by crux for `%s%s`"
             entity-symbol
             event-symbol)})))

(defn add-reducer-to-event-spec [entity-spec event-symbol]
  
  (let [event-spec (get-in entity-spec [EVENTS event-symbol])
        entity-symbol (ENTITY-SYMBOL entity-spec)
        reducer (gen-event-reducer
                 entity-spec event-symbol
                 (FIELDS event-spec)
                 (REDUCE-FORMS event-spec))]
      (update-in entity-spec [EVENTS event-symbol] assoc REDUCER reducer)))

(defn gen-single-command-validator [entity-spec event-spec form]
  (let [property-symbols (keys (PROPERTIES entity-spec))
        event-fields (FIELDS event-spec)
        validator-fn-symbol (symbol (format "%s-%s-validate"
                                            (:name event-spec)
                                            (:name entity-spec)))
        fn-form `(fn ~validator-fn-symbol
                   [validator-args#]
                   (let [~(symbol "check") crux.domain/check
                         {:keys [~(symbol "event")
                                 ~(symbol "entity")
                                 ~(symbol "user")]} validator-args#

                                 {:keys [~@property-symbols]}
                                 (map-over-keys
                                  keyword (crux-properties-from-meta
                                           ~(symbol "entity")))
                                 
                                 {:keys [~@event-fields]} ~(symbol "event")]
                     (-> validator-args#
                         ~form)))]

    (eval-with-meta fn-form
      {:doc (format
             "Command Validator generated by crux for command `%s`: `%s`"
             (:name event-spec)
             form)
       :crux/generated-from 'crux.domain/gen-single-command-validator})))

(defn gen-single-constraint-checker [entity-spec form]
  (let [property-symbols (keys (PROPERTIES entity-spec))
        fn-form `(fn constraint-checker [entity#]
                   (let [~(symbol "entity") entity#
                         entity-properties# (crux-properties-from-meta entity#)
                         {:keys [~@property-symbols]}
                         (into {} (for [[k# v#] entity-properties#]
                                    [(keyword k#) v#]))]
                     (-> entity#
                         ~form)))]

    (eval-with-meta fn-form
      {:doc (format
             "Function generated by crux for entity `%s`: constraint `%s`"
             (:name entity-spec)
             form)
       :crux/generated-from 'crux.domain/gen-single-constraint-checker})))



(defn add-constraints-to-event-spec [event-entity event-symbol]
  (let [event-spec (get-in event-entity [EVENTS event-symbol])
        properties-map (get-in event-spec [ADDITIONAL-EVENT-ATTRS PROPERTIES])]
    (if properties-map
      (properties* event-spec properties-map)
      event-spec)))

(defn add-full-names-for-event-and-command-on-event-spec
  [entity-spec event-symbol]
  (let [event-spec (get-in entity-spec [EVENTS event-symbol])
        entity-symbol (ENTITY-SYMBOL entity-spec)]
    (update-in entity-spec [EVENTS event-symbol]
               merge
               {FULL-COMMAND-SYMBOL
                (symbol (format "%s%s"
                                (COMMAND-SYMBOL event-spec)
                                entity-symbol))
                FULL-EVENT-SYMBOL
                (symbol (format "%s%s"
                                (ENTITY-SYMBOL entity-spec)
                                event-symbol))})))


(defn add-command-to-event-spec [entity-spec event-symbol]
  (let [entity-symbol (ENTITY-SYMBOL entity-spec)
        event-spec (get-in entity-spec [EVENTS event-symbol])
        ;; 
        command-constraint-forms (get-in event-spec
                                         [ADDITIONAL-EVENT-ATTRS CONSTRAINTS])
        ;; Generate function bindings from the forms given
        command-constraints (into (array-map)
                                  (for [form command-constraint-forms]
                                    [form
                                     (gen-single-constraint-checker entity-spec
                                                                    form)]))
        command-validation-forms (get-in
                                  event-spec
                                  [ADDITIONAL-EVENT-ATTRS VALIDATIONS])]

    (update-in entity-spec [EVENTS event-symbol]
               assoc
               COMMAND-VALIDATIONS
               (into (array-map)
                     (for [form command-validation-forms]
                       [form
                        (gen-single-command-validator entity-spec
                                                      event-spec
                                                      form)])))))

(defn event* [entity-spec
              {:keys [event-symbol command-symbol event-fields
                      additional-event-attrs reduce-forms]
               :as event-spec}]

  (let [entity-symbol (:name entity-spec)]

    (when (get-in entity-spec [EVENTS event-symbol])
      (throw-aready-declared-error
       'event event-symbol 'entity entity-symbol))

    (when-not reduce-forms
      (throw-reduce-forms-required-error event-symbol))

    (let [
          
          event-spec (map->EventSpec
                      {DEFAULTS (DEFAULTS entity-spec)
                       :name event-symbol
                       ENTITY-SYMBOL entity-symbol
                       EVENT-SYMBOL event-symbol
                       COMMAND-SYMBOL command-symbol
                       FIELDS event-fields
                       ADDITIONAL-EVENT-ATTRS additional-event-attrs
                       REDUCE-FORMS reduce-forms})
          
          ;; add properties to event-spec
          properties-map (PROPERTIES additional-event-attrs)
          event-spec (if properties-map
                       (properties* event-spec properties-map)
                       event-spec)
          
          ]
      (-> entity-spec
          (update-in [EVENTS] assoc event-symbol event-spec)
          ;;(add-constraints-to-event-spec event-symbol)
          (add-full-names-for-event-and-command-on-event-spec event-symbol)
          (add-reducer-to-event-spec event-symbol)
          (add-command-to-event-spec event-symbol)
          ;
          ))))

(defn- quote-event-spec-form
  [[event-symbol command-symbol event-fields & rem]]
  (let [event-fields (-quote-or-unquote-fields-form
                      event-fields event-symbol)
        [additional-event-attrs
         reduce-forms] (if (map? (first rem))
                         [(first rem) (rest rem)]
                         [nil rem])]
    (when-not reduce-forms
      (throw-reduce-forms-required-error event-symbol))

    `{EVENT-SYMBOL '~event-symbol
      COMMAND-SYMBOL '~command-symbol
      :event-fields ~event-fields
      ADDITIONAL-EVENT-ATTRS '~additional-event-attrs
      REDUCE-FORMS '~reduce-forms}))

(defmacro events [entity-spec & event-specs]
  (let [event-specs (into [] (map quote-event-spec-form event-specs))]
    `(let [~(symbol "entity-fields") (FIELDS ~entity-spec)]
       (doall (reduce event* ~entity-spec ~event-specs)))))

;;;;;;;;;
;;; Crux DDL: minor fns/macros

(defn field-types* [m field-types-map]
  (update-in m [DEFAULTS :field-types] merge field-types-map))

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
   entity-spec [DEFAULTS :type-inference-rules]
   conj [regex abstract-type]))

(defn id-field* [entity-spec id-field-name & [id-type]]
  (assoc entity-spec ID-FIELD {:name id-field-name
                                :type (or id-type :guid)}))
(defmacro id-field [entity-spec id-field-name & [id-type]]
  `(id-field* ~entity-spec '~id-field-name ~id-type))

(defn first-unmet-constraint [entity constraint-forms+preds]
  (some (fn -or* [[form pred]] (if-not (pred entity) form))
        constraint-forms+preds))

(defn unmet-constraints [entity constraint-forms+preds]
  (for [[form pred] constraint-forms+preds
        :when (not (pred entity))]
    form))

(defn unmet-validations [validation-args validation-forms+vfns]
  (for [[form vfn] validation-forms+vfns
        [error-message] [[(vfn validation-args)]]
        :when (not (nil? error-message))]
    {:error-message error-message
     :validation-form form}))

(defn first-unmet-validation [validation-args validation-forms+vfns]
  (first (unmet-validations validation-args validation-forms+vfns)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

