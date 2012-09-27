(ns crux.reify
  "crux.reify transforms abstract domain specs defined via the DSL in
  crux.domain into concrete executable code:

  - Each Entity type is reified with defrecord
  - Each Event/Command pair is reified as a pair of record types
  - An event reduction multimethod is created for each Entity and its
    Events.
  ...
  "
  (:require [clojure.pprint :refer [pprint]])
  (:require [clojure.string :as str])
  (:require [clojure.set :as set])
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
      (store/-reify-event-store {:memory {:name (:name domain-spec)}})))

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


(defn zip-reductions [events event-reductions & [begin & [end]]]
  (let [begin (or begin 0)
        zipped (into [] (map vector events
                             event-reductions ; input
                             (rest event-reductions) ; output
                             ))
        end (or end (count zipped))]
    (subvec zipped begin end)))

(defn print-event-reductions [domain-spec events
                              & {:keys [initial begin end highlight]}]
  (let [event-reductions (reduction-of-events domain-spec events initial)
        reductions (zip-reductions events event-reductions)
        domain-prefix (symbol (:name domain-spec))
        type-name #(.getSimpleName (type %))]
    (doseq [[i [event input red]] (map vector
                                       (range (count reductions))
                                       reductions)]
      (let [diff (into {} (set/difference (set red) (set input)))]
        (println "")
        (print (format "#%s/%s" domain-prefix (type-name event)))
        (pprint (into {} (filter second event)))
        (if (= highlight i)
          (print "   #_result->>> ")
          (print "   #_result-> "))
        ;; (print (format "#%s/%s" domain-prefix (type-name red)) )
        (pprint (into {} (filter second red)))
        (print "   #_diff-> ")
        (pprint diff)))))

(defn event-log-to-reductions-report [domain-spec file-path]
  (print-event-reductions
   domain-spec
   (read-domain-event-log domain-spec file-path)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Command handling


;;; Utility functions to transform command-symbol to
;;; event/entity

(defn get-entity-symbol-from-command-symbol
  [domain-spec command-symbol]
  (get (:crux.reify/event+command-symbols-to-entity-symbols domain-spec)
       command-symbol))

(defn get-event-symbol-from-command-symbol
  [domain-spec command-symbol]
  (get (:crux.reify/commands-to-events domain-spec)
       command-symbol))

(defn get-command-constraints
  [domain-spec command-symbol]
  (let [entity-symbol (get-entity-symbol-from-command-symbol
                       domain-spec command-symbol)]
    (get-in domain-spec
            [ENTITIES entity-symbol COMMAND-CONSTRAINTS])))

(defn get-command-validations
  [domain-spec command-symbol]
  (let [entity-symbol (get-entity-symbol-from-command-symbol
                       domain-spec command-symbol)]
    (get-in domain-spec
            [ENTITIES entity-symbol COMMAND-VALIDATIONS])))


(defn -reify-command->event-fn [domain-spec]
    (fn command-handler [command]
      (let [;;; get meta data needed in order to process the command
            {entity-oid :oid
             entity-rev :rev
             user-oid :user-oid} (:crux/target (meta command))

             ;;; get-command+event+entity-symbols-from-domain-spec-and-command
             command-symbol   (type-symbol command)
             entity-symbol    (get-entity-symbol-from-command-symbol
                               domain-spec command-symbol)

             check-command-constraints (get-check-command-constraints-fn
                                         domain-spec)
             validate-command          (get-validate-command-fn
                                         domain-spec)

             ;;; entity of the command
             get-entity       (:crux.reify/get-entity domain-spec)
             entity           (get-entity entity-symbol
                                          entity-oid
                                          #_entity-rev)

             ;; TODO: set a variable that has the 'User entity.
             user             (get-entity 'User user-oid)
             ]
        (do
          (check-command-constraints command entity)
          (validate-command command entity user)
          (-build-event-from-command domain-spec command)))))

(defn -reify-command->event-fn-to-domain-spec
  [domain-spec]
  (assoc domain-spec
    :crux.reify/command->event-fn (-reify-command->event-fn domain-spec)))


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
