(ns crux.reify
  "crux.reify transforms abstract domain specs defined via the DSL in
  crux.domain into concrete executable code:

  - Each Entity type is reified with defrecord
  - Each Event/Command pair is reified as a pair of record types
  - An event reduction multimethod is created for each Entity and its
    Events.
  ...
  "
  (:require [clojure.string :as str])

  (:require [slingshot.slingshot :refer [throw+]])

  (:require [crux.internal.keys :refer :all]
            [crux.reify.records  :as records]
            [crux.reify.entity-finder :as finder]
            [crux.reify.store :as store]
            [crux.reify.reducers :as reducers]
            [crux.reify.commands :as commands]
            [crux.util :refer
             [map-over-keys read-forms-from-file]]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn reify-domain-spec! [domain-spec]
  (-> domain-spec
      records/-reify-domain-records!
      finder/-reify-get+set-entity-functions
      reducers/-reify-all-entity-event-reducers!
      commands/-reify-check-command-constraints-function
      commands/-reify-validate-command-function
      commands/-reify-command->event-fn-to-domain-spec
      (store/-add-event-store {:memory {:name (:name domain-spec)}})
      store/-reify-store-events-function
      ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; tools for using the records reified here

(defn get-domain-data-readers [domain-spec & [prefix]]
  (let [prefix (or prefix (symbol (:name domain-spec)))]
    (map-over-keys
     #(symbol (format "%s/%s" prefix %))
     (:crux.reify/constructors domain-spec))))

(defn read-domain-data-from-file
  "Returns a seq of top level clojure data forms from the file. If the
  data includes any (def)records that were declared on the
  domain-spec (commands, events, entities, etc.) they should be in
  clojure `data-readers` format with the `reader-prefix` as the
  namespace on the reader tag and the symbol of the record type on the
  second part of the reader tag.

  e.g. #tickets/Ticket{}, #tickets/TicketAssigned{}"

  [domain-spec file-path & [reader-prefix]]
  (let [reader-prefix (or reader-prefix (symbol (:name domain-spec)))]
    (binding [*data-readers* (get-domain-data-readers
                              domain-spec reader-prefix)]
      (doall (read-forms-from-file file-path)))))

(defn read-domain-event-log [domain-spec file-path & [domain-prefix]]
  (read-domain-data-from-file domain-spec file-path domain-prefix))

(defn -resolve-entity-symbol [domain-spec domain-symbol-or-class]
  (cond (symbol? domain-symbol-or-class)
        (if (get-in domain-spec [ENTITIES domain-symbol-or-class])
          domain-symbol-or-class
          (get-in domain-spec
                  [:crux.reify/event+command-symbols-to-entity-symbols
                   domain-symbol-or-class]))

        :else
        (get-in domain-spec [:crux.reify/records-to-entity-symbols
                             domain-symbol-or-class])))

(defn get-reducer [domain-spec domain-symbol-or-class]
  (get-in domain-spec [:crux.reify/reducers
                       (-resolve-entity-symbol
                        domain-spec
                        domain-symbol-or-class)]))

(defn get-entity-ctor [domain-spec domain-symbol-or-class]
  (get-in domain-spec [:crux.reify/constructors
                       (-resolve-entity-symbol
                        domain-spec
                        domain-symbol-or-class)]))

(defn -reduce-events [clj-reduce-fn domain-spec events & [entity0]]
  (let [ev1-type (type (first events))
        entity-ctor (get-entity-ctor domain-spec ev1-type)
        reducer (get-reducer domain-spec ev1-type)
        entity0 (entity-ctor (or entity0 {}))]
    (clj-reduce-fn reducer entity0 events)))

(defn reduce-events [domain-spec events & [entity0]]
  (-reduce-events reduce domain-spec events entity0))

(defn reduction-of-events [domain-spec events & [entity0]]
  (-reduce-events reductions domain-spec events entity0))
