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
  (:require [crux.internal.keys :refer :all])
  (:require [crux.util :refer
             [defrecord-dynamically
              addmethod-to-multi
              map-over-keys
              map-over-values
              read-forms-from-file]])
  (:require [crux.command-handling :refer
             [unmet-validations
              unmet-constraints]]))

;;; Crux Reification code:
;;; abstract-spec -> real records, event reduction multifns,
;;; data-readers, etc.

(defn -add-constructor-and-records-to-domain-spec
  [domain-spec record-symbol record-map]
  (-> domain-spec
      (update-in [:crux.reify/records]
                 assoc record-symbol record-map)
      (update-in [:crux.reify/constructors]
                 assoc record-symbol (:record-ctor record-map))))

(defn -add-command-to-event-mapping [domain-spec command-symbol event-symbol]
  (update-in domain-spec [:crux.reify/commands-to-events]
             assoc command-symbol event-symbol))

(defn -add-event-records-to-entity-mapping [domain-spec event-spec]
  (let [entity-symbol (ENTITY-SYMBOL event-spec)
        reified-records (:crux.reify/records domain-spec)
        ev-sym (FULL-EVENT-SYMBOL event-spec)
        com-sym (FULL-COMMAND-SYMBOL event-spec)]
    (-> domain-spec
     (update-in [:crux.reify/records-to-entity-symbols]
               merge
               {(get-in reified-records [ev-sym :record-class]) entity-symbol
                (get-in reified-records [com-sym :record-class])
                entity-symbol})
     (update-in [:crux.reify/event+command-symbols-to-entity-symbols]
               merge
               {ev-sym entity-symbol
                com-sym entity-symbol}))))

(defn -reify-event-and-command!
  [domain-spec event-spec]
  (let [{:keys [entity-symbol event-symbol]} event-spec
        fields (FIELDS event-spec)

        full-command-symbol (FULL-COMMAND-SYMBOL event-spec)
        command-record-map (defrecord-dynamically
                             full-command-symbol fields)

        full-event-symbol (FULL-EVENT-SYMBOL event-spec)
        event-record-map (defrecord-dynamically
                           full-event-symbol fields)]
    (-> domain-spec
        (-add-command-to-event-mapping (COMMAND-SYMBOL event-spec)
                                       event-symbol)
        (-add-command-to-event-mapping FULL-COMMAND-SYMBOL FULL-EVENT-SYMBOL)

        (-add-constructor-and-records-to-domain-spec
         full-command-symbol command-record-map)

        (-add-constructor-and-records-to-domain-spec
         full-event-symbol event-record-map)

        (-add-event-records-to-entity-mapping event-spec)
        )))

(defn -reify-events-and-commands-for-each-entity! [domain-spec]
  (reduce -reify-event-and-command!
          domain-spec
          (flatten (map #(vals (EVENTS %))
                        (vals (ENTITIES domain-spec))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn -reify-entity-record! [domain-spec entity-symbol]
  (let [entity-spec (get-in domain-spec [ENTITIES entity-symbol])
        fields (FIELDS entity-spec)
        properties (PROPERTIES entity-spec)
        defrecord-map (defrecord-dynamically entity-symbol fields)
        orig-ctor (:record-ctor defrecord-map)
        crux-meta-fields {CRUX-PROPERTIES properties}
        defrecord-map (assoc defrecord-map
                        :record-ctor
                        (fn [m] (vary-meta
                                 (orig-ctor m)
                                 merge crux-meta-fields)))]
    (-> domain-spec
        (update-in [ENTITIES entity-symbol] merge defrecord-map)
        (-add-constructor-and-records-to-domain-spec
            entity-symbol defrecord-map))))

(defn -reify-all-entity-records! [domain-spec]
  (reduce -reify-entity-record! domain-spec (keys (ENTITIES domain-spec))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; get-entity / set-entity

;; {
;;  ['Entity] (atom {id (atom {})})
;; }

(defn -reify-get+set-entity-functions [domain-spec]
  (let [entity-caches-map (map-over-values (constantly (atom {}))
                                           (ENTITIES domain-spec))]
    (-> domain-spec
        (assoc :crux.reify/entity-caches-map entity-caches-map)
        (assoc :crux.reify/set-entity
          (fn set-entity-fn [entity-symbol entity-id entity]
            ;; This version assumes all entities are in memory. Later
            ;; this will be called after commit-events! on an
            ;; event-store (savant etc.)
            (swap! (get entity-caches-map entity-symbol)
                   ;; ^ and \/ the atom for the entity's own cache
                   (fn [entity-cache-map]
                     (let [entity-atom (get entity-cache-map entity-id)
                           created-new-atom? (nil? entity-atom)
                           entity-atom (or entity-atom ; may be nil
                                           (atom nil))]
                       (swap! entity-atom (constantly entity))
                       (if created-new-atom?
                         (assoc entity-cache-map entity-id entity-atom)
                         entity-cache-map))))))
        (assoc :crux.reify/get-entity
          (fn get-entity-fn [entity-symbol entity-id #_rev]
            ;; This version assumes all entities are in memory. Later
            ;; this will look for anything not in memory in an
            ;; event-store (savant etc.)
            (let [entity-cache-atom (get entity-caches-map entity-symbol)
                  entity-atom (get @entity-cache-atom entity-id)]
              (if entity-atom
                @entity-atom
                nil)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn -reify-domain-records! [domain-spec]
  (-> domain-spec
      -reify-all-entity-records!
      -reify-events-and-commands-for-each-entity!))


(defn -reify-entity-event-reducer! [domain-spec entity-symbol]
  (let [multi-name (symbol (format "%s-event-reducer" entity-symbol))
        events (get-in domain-spec  [ENTITIES entity-symbol EVENTS])
        multifn-var (eval `(defmulti ~multi-name
                             (fn [entity# event#] (type event#))))
        multifn (eval `~multi-name)]

    (doseq [[event-symbol event-spec] events]
      (let [event-rec-symbol
            (symbol (format "%s%s" entity-symbol event-symbol))
            reducer-fn (REDUCER event-spec)
            record-map (get-in domain-spec
                               [:crux.reify/records event-rec-symbol])]
        (when-not reducer-fn
          (throw+ {:type :library/specify-reducer-error
                   :msg ""}))
        (addmethod-to-multi
         multifn (:record-class record-map) reducer-fn)))
    (update-in domain-spec [:crux.reify/reducers]
               assoc entity-symbol multifn)))

(defn -reify-all-entity-event-reducers! [domain-spec]
  (reduce -reify-entity-event-reducer!
          domain-spec (keys (ENTITIES domain-spec))) )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Main public interface

(defn reify-domain-spec! [domain-spec]
  (-> domain-spec
      -reify-domain-records!
      -reify-get+set-entity-functions
      -reify-all-entity-event-reducers!))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Command handling
#_(defn get-spec-for-command [command-record]
  nil
  )

#_(defn handle-command-pseudo-code [domain-spec command]
  (let [event-spec (get-spec-for-command command)
        entity-symbol (ENTITY-SYMBOL event-spec)
        {entity-id :id
         entity-rev :rev} (:crux/target command)
        ;; rest of this atomically
        get-entity (:crux.reify/get-entity domain-spec)
        set-entity (:crux.reify/set-entity domain-spec)

        entity-from-cache (get-entity entity-symbol entity-id)
        entity-current-tip (:crux/rev entity-from-cache)
        conflict-detector (:conflict-detector event-spec)
        conflict (conflict-detector
                  entity-id entity-rev
                  entity-from-cache
                  entity-current-tip)]
    (if-not conflict
      (let [constraints (COMMAND-CONSTRAINTS event-spec)
            constraint-result (unmet-constraints
                               entity-from-cache constraints)
            validations (COMMAND-VALIDATIONS event-spec)
            validation-result (unmet-validations
                               {:entity entity-from-cache
                                :event command
                                :user nil}
                               validations)

            reducer (REDUCER event-spec)]
        (when-not (empty? constraint-result)
          (throw+
           (format "Entity doesn't meet constraints: %s"
                   (str/join ", " constraint-result))))
        (when-not (empty? validation-result)
          (throw+ validation-result))
        ;; (commit-events! )
        )
      ;; (bitch-about conflict)
      )))
