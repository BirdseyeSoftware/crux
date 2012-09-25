(ns crux.domain
  (:require [clojure.set :as set])

  (:require [slingshot.slingshot :refer (throw+)])

  (:require [crux.internal.keys :refer :all]
            [crux.util
             :refer [throw-already-declared-error
                     throw-reduce-forms-required-error
                     eval-with-meta
                     unquoted?
                     -quote-or-unquote-fields-form
                     map-over-keys
                     create-multi
                     add-method-to-multi
                     defrecord-dynamically
                     defrecord-keep-meta]]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn check [{:keys [entity event user]}
             validation-value error-msg]
  (when (not validation-value)
    error-msg))

(defn crux-properties-from-meta [entity]
  (CRUX-PROPERTIES (meta entity)))

;; TODO
(declare add-map-of-command->events-to-domain-spec)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Core DDL Record types

;;; NOTE! The :type meta declared below on the defrecord-meta
;;; declarations is for documentation / UI building purposes. It has
;;; nothing to do with java type hinting.  Also note, we haven't
;;; started using them yet and they have probably gotten out of sync
;;; with the rest of the code.

(defprotocol ICruxSpec
  (get-name [this])
  (get-symbol [this])
  (get-full-symbol [this])
  (-validate-spec [this]))

(defrecord-keep-meta CruxDefaults
    [^{:type {Symbol AbstractFieldTypeAbstractFieldType}} field-types
     ^{:type [[Fn, AbstractFieldType]]} type-inference-rules])

(defrecord-keep-meta EventReductionSpec
    [^{:type :Keyword} name
     ^{:type :variable} args])

(defrecord-keep-meta EventSpec
    [^{:type Symbol} name
     ^{:type [Symbol]} fields
     ^{:type [EventReductionSpec]} reduce-forms
     ^{:type Fn } reducer
     ^{:type CruxDefaults} defaults]

  ICruxSpec
  (get-name [this] (EVENT-NAME this))
  (get-symbol [this] (EVENT-SYMBOL this))
  (get-full-symbol [this] (FULL-EVENT-SYMBOL this))
  (-validate-spec [this] this))

(defrecord-keep-meta CommandSpec
    [^{:type Symbol} name
     ^{:type [Symbol]} fields
     ^{:type CruxDefaults} defaults]

  ICruxSpec
  (get-name [this] (COMMAND-NAME this))
  (get-symbol [this] (COMMAND-SYMBOL this))
  (get-full-symbol [this] (FULL-COMMAND-SYMBOL this))
  (-validate-spec [this] this))

(defrecord-keep-meta EntitySpec
    [^{:type Symbol} name
     ^{:type Symbol} plural-name
     ^{:type CruxDefaults} defaults
     ^{:type Map} id-field
     ^{:type [Symbol]} fields
     ^{:type [EntityProperty]} properties
     ^{:type {Symbol EventSpec}} events
     ^{:type {Symbol CommandSpec}} commands]

  ICruxSpec
  (get-name [this] (ENTITY-NAME this))
  (get-symbol [this] (ENTITY-SYMBOL this))
  (get-full-symbol [this] (ENTITY-SYMBOL this))
  (-validate-spec [this]
    ;; when entity `fields` is not a vector of symbols
    (when-not (and (vector? (FIELDS this))
                   (every? symbol? (FIELDS this)))
        (throw+ {:type :crux/invalid-entity-fields
                 :msg (str this)}))

   ;; when each event `fields` is not a vector of symbols
   (doseq [[event-symbol event-map] (EVENTS this)]
    (when-not (and (vector? (FIELDS event-map))
                   (every? symbol? (FIELDS event-map)))
      (throw+ {:type :crux/invalid-event-fields
               :msg (str event-map)})))

    this))

(defn -validate-domain-spec [domain-spec]
  ;; NOTE: it's better to do this in each of the macros so the error
  ;; reporting is closer to the original source of the error

  (doseq [[entity-symbol entity-spec] (ENTITIES domain-spec)]
    (-validate-spec entity-spec))
  domain-spec)

(defrecord-keep-meta DomainSpec
    [^{:type Symbol} name
     ^{:type CruxDefaults} defaults
     ^{:type {Symbol EntitySpec}} entities
     ^{:type {Symbol Symbol}} reifications]

  ICruxSpec
  (get-name [this] (:name this))
  (get-symbol [this] (symbol (get-name this)))
  (get-full-symbol [this] (get-symbol this))
  (-validate-spec
   [this]
   ;; For some reason the compiler is throwing an error
   ;; that says that -validate-spec is not defined
   ;; on DomainSpec, even though it has a definition
   ;; of the protocol ICruxSpec
   (-validate-domain-spec this)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Crux DDL: minor fns/macros

(defn -ensure-is-crux-domain-spec [m]
  (if (instance? DomainSpec m) m (map->DomainSpec m)))

(defn -seed-domain-spec-with-crux-defaults [domain-spec]
  (if-let [provided-defaults (DEFAULTS domain-spec)]
    (if-not (instance? CruxDefaults provided-defaults)
      (assoc domain-spec DEFAULTS (map->CruxDefaults provided-defaults))
      domain-spec)
    (assoc domain-spec DEFAULTS (map->CruxDefaults {}))))

(defmacro create-domain [initial-spec & body]
  `(do
     (assert (:name ~initial-spec))
     ;; the -fns used below must be public as this is expanded in 3rd
     ;; party ns's
     (-> ~initial-spec
         -ensure-is-crux-domain-spec
         -seed-domain-spec-with-crux-defaults
         ~@body
         -validate-domain-spec)))

(defmacro defdomain [domain-name & body]
  `(def ~domain-name
     (create-domain {:name '~domain-name}
       ~@body)))

(defmacro entity [domain-spec entity-name entity-fields & body]
  (let [fields (-quote-or-unquote-fields-form entity-fields entity-name)]
    `(let [entity-spec# (map->EntitySpec {DEFAULTS (DEFAULTS ~domain-spec)
                                          ENTITY-NAME ~(name entity-name)
                                          ENTITY-SYMBOL '~(symbol entity-name)
                                          FIELDS ~fields})]
       (-> ~domain-spec
           (update-in [ENTITIES] assoc '~entity-name
                      (-> entity-spec# ~@body -validate-spec))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; entity/event/command properties (unary functions)

(defn- -gen-single-property-fn
  [name-of-local-symbol fields property-sym all-property-symbols form]
  (eval-with-meta
    `(fn ~property-sym [entity-or-event#]
       (let [{:keys [~@fields]} entity-or-event#
             ~(symbol name-of-local-symbol) entity-or-event#

             {:keys [~@all-property-symbols]}
             (map-over-keys keyword (crux-properties-from-meta
                                     ~(symbol name-of-local-symbol)))]
         ~form))
    {:doc (format "Property %s generated by crux" property-sym )
     :crux/generated-by 'crux.domain/gen-property-fn}))

(defn- -property-dispatch-function [entity-or-event]
  (symbol (.getSimpleName (type entity-or-event))))

(defn- -create-property-multimethods
  [existing-multifns-map new-property-forms-map]
  (let [new-keys (set/difference (set (keys new-property-forms-map))
                                 (set (keys existing-multifns-map)))
        new-multifns-map (into {}
                           (for [k new-keys]
                             [k
                              (create-multi
                               k -property-dispatch-function)]))]
    (merge existing-multifns-map new-multifns-map)))

(defn- -add-multimethods-to-properties
  [entity-spec
   dispatch-value
   name-of-local-symbol ; "event" or "entity"
   fields
   property-forms-map]
  (let [existing-property-multifns (PROPERTIES entity-spec)
        property-fns-map
        (into {}
              (for [[property-sym form] property-forms-map]
                [property-sym
                 (-gen-single-property-fn
                  name-of-local-symbol
                  fields
                  property-sym
                  (keys (PROPERTIES entity-spec))
                  form)]))]
    (doseq [[property-sym pfn] property-fns-map]
      (add-method-to-multi (existing-property-multifns property-sym)
                          dispatch-value pfn)))
  entity-spec)

(defn- -add-property-forms
  "The property forms are always added to the entity-spec record."
  [entity-spec property-forms-map & [event-spec]]
  (if event-spec
    (update-in entity-spec [EVENTS (EVENT-SYMBOL event-spec)
                            PROPERTY-FORMS]
               merge property-forms-map)
    (update-in entity-spec [PROPERTY-FORMS] merge property-forms-map)))


(defn properties*
  ([entity-spec property-forms-map & [event-spec]]
     (let [name-of-local-symbol (if event-spec "event" "entity")
           fields (FIELDS (or event-spec entity-spec))
           dispatch-value (if event-spec
                            (FULL-EVENT-SYMBOL event-spec)
                            (ENTITY-SYMBOL entity-spec))]
       ;; TODO add the FULL-COMMAND-SYMBOL dispatch val also
       (-> entity-spec
           (-add-property-forms property-forms-map event-spec)
           (update-in [PROPERTIES]
                      -create-property-multimethods
                      property-forms-map)
           (-add-multimethods-to-properties
            dispatch-value name-of-local-symbol fields property-forms-map)))))

(defmacro properties [entity-spec property-forms-map]
  ;; TODO record what *ns* the forms were defined in and set it when doing
  ;; the eval for each form
  `(properties* ~entity-spec '~property-forms-map))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- -gen-single-event-reducer [entity-spec event-symbol event-fields forms]
  (let [entity-symbol (ENTITY-SYMBOL entity-spec)
        entity-property-symbols (keys (PROPERTIES entity-spec))
        event-reducer-symbol
        (symbol (format "%s-%s-reduce" entity-symbol event-symbol))]
    (eval-with-meta
      `(fn ~event-reducer-symbol [entity#
                                  {:keys [~@event-fields] :as event#}]
         (let [~(symbol "entity") entity#
               ~(symbol "event") (vary-meta
                                  event# assoc
                                  CRUX-PROPERTIES
                                  (CRUX-PROPERTIES (meta entity#)))
               {~(symbol "entity-id") :oid} entity#
               {~(symbol "event-id") :oid} event#
               {:keys [~@entity-property-symbols]}
               (map-over-keys keyword (crux-properties-from-meta
                                       ~(symbol "entity")))]
           (-> entity#
               ~@forms)))
      {:crux/generated-from 'crux.domain/gen-event-reducer
       :doc (format "Event reducer generated by crux for `%s%s`"
                    entity-symbol event-symbol)})))

(defn- -add-reducer-to-event-spec [entity-spec event-symbol]
  (let [event-spec (get-in entity-spec [EVENTS event-symbol])
        entity-symbol (ENTITY-SYMBOL entity-spec)
        reducer (-gen-single-event-reducer
                 entity-spec event-symbol
                 (FIELDS event-spec)
                 (REDUCE-FORMS event-spec))]
      (update-in entity-spec [EVENTS event-symbol] assoc REDUCER reducer)))

(defn- -gen-single-command-validator [entity-spec event-spec form]
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

(defn- -gen-single-constraint-checker [entity-spec form]
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

(defn- -add-properties-from-event-spec [entity-spec event-symbol]
  (let [event-spec (get-in entity-spec [EVENTS event-symbol])
        property-forms-map (get-in event-spec
                                   [ADDITIONAL-EVENT-ATTRS PROPERTIES])]
    (if property-forms-map
      (properties* entity-spec property-forms-map event-spec)
      entity-spec)))

(defn- -gen-constraint-checker-fns-map [entity-spec command-constraint-forms]
  (into (array-map)
        (for [form command-constraint-forms]
          [form (-gen-single-constraint-checker entity-spec form)])))

(defn- -gen-command-validation-fns-map
  [entity-spec event-spec command-validation-forms]
  (into (array-map)
        (for [form command-validation-forms]
          [form
           (-gen-single-command-validator entity-spec event-spec form)])))

(defn- -add-command-to-event-spec [entity-spec event-symbol]
  (let [entity-symbol (ENTITY-SYMBOL entity-spec)
        event-spec (get-in entity-spec [EVENTS event-symbol])
        additional-event-attrs (ADDITIONAL-EVENT-ATTRS event-spec)
        command-constraint-forms (CONSTRAINTS additional-event-attrs)
        command-validation-forms (VALIDATIONS additional-event-attrs)]
    (update-in entity-spec [EVENTS event-symbol]
               merge
               {COMMAND-CONSTRAINT-FORMS command-constraint-forms
                COMMAND-CONSTRAINTS (-gen-constraint-checker-fns-map
                                     entity-spec command-constraint-forms)

                COMMAND-VALIDATION-FORMS command-validation-forms
                COMMAND-VALIDATIONS (-gen-command-validation-fns-map
                                     entity-spec event-spec
                                     command-validation-forms)})))

(defn event* [entity-spec
              {:keys [event-symbol command-symbol fields
                      additional-event-attrs reduce-forms]
               :as event-spec}]

  (let [entity-symbol (ENTITY-SYMBOL entity-spec)]

    (when (get-in entity-spec [EVENTS event-symbol])
      (throw-already-declared-error
       'event event-symbol 'entity entity-symbol))

    (when-not reduce-forms
      (throw-reduce-forms-required-error event-symbol))

    (let [event-spec
          (map->EventSpec
           {DEFAULTS (DEFAULTS entity-spec)
            ENTITY-SYMBOL entity-symbol
            EVENT-SYMBOL event-symbol
            COMMAND-SYMBOL command-symbol
            FULL-COMMAND-SYMBOL (symbol
                                 (format "%s%s"
                                         (COMMAND-SYMBOL event-spec)
                                         entity-symbol))
            FULL-EVENT-SYMBOL (symbol
                               (format "%s%s"
                                       (ENTITY-SYMBOL entity-spec)
                                       event-symbol))

            FIELDS fields
            ADDITIONAL-EVENT-ATTRS additional-event-attrs
            REDUCE-FORMS reduce-forms})]
      (-> entity-spec
          (update-in [EVENTS] assoc event-symbol event-spec)
          (-add-properties-from-event-spec event-symbol)
          (-add-reducer-to-event-spec event-symbol)
          (-add-command-to-event-spec event-symbol)))))

(defn- -quote-event-spec-form
  [[event-symbol command-symbol event-fields & rem]]
  (let [;;captured-in-ns *ns* ; TODO use this in the evals
        event-fields (-quote-or-unquote-fields-form
                      event-fields event-symbol)
        [additional-event-attrs
         reduce-forms] (if (map? (first rem))
                         [(first rem) (rest rem)]
                         [nil rem])]
    (when-not reduce-forms
      (throw-reduce-forms-required-error event-symbol))

    `{EVENT-SYMBOL '~event-symbol
      COMMAND-SYMBOL '~command-symbol
      FIELDS ~event-fields
      ADDITIONAL-EVENT-ATTRS '~additional-event-attrs
      REDUCE-FORMS '~reduce-forms}))

(defmacro events [entity-spec & event-specs]
  (let [event-specs (into [] (map -quote-event-spec-form event-specs))]
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
