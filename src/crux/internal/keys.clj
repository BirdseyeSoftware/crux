(ns crux.internal.keys)


;;; keywords used on entity meta at runtime
(defonce CRUX-PROPERTIES :crux/properties)

;;;;; keywords used at build time from entity|event-spec

(defonce COMMAND-CONSTRAINT-FORMS :command-constraint-forms)
(defonce COMMAND-VALIDATION-FORMS :command-validation-forms)
(defonce COMMAND-VALIDATIONS :command-validations)

(defonce REDUCER :reducer)
(defonce REDUCE-FORMS :reduce-forms)
(defonce ADDITIONAL-EVENT-ATTRS :additional-event-attrs)
(defonce CONSTRAINTS :entity-constraints)
(defonce VALIDATIONS :validations)
(defonce FIELDS :fields)

(defonce ENTITIES :entities)
(defonce ENTITY-SYMBOL :entity-symbol)
(defonce ENTITY-NAME :entity-name)

(defonce EVENTS :events)
(defonce EVENT-NAME :event-name)
(defonce EVENT-SYMBOL :event-symbol)
(defonce FULL-EVENT-SYMBOL :full-event-symbol)

(defonce COMMAND-NAME :event-name)
(defonce COMMAND-SYMBOL :command-symbol)
(defonce FULL-COMMAND-SYMBOL :full-command-symbol)

(defonce DEFAULTS :defaults)

(defonce PROPERTIES :properties)
(defonce PROPERTY-FORMS :property-forms)
(defonce COMMAND-CONSTRAINTS :command-constraints)
(defonce COMMAND-VALIDATORS :command-validators)

(defonce ID-FIELD :id-field)
