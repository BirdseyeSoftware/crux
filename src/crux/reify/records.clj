(ns crux.reify.records
  (:require [crux.internal.keys :refer :all]
            [crux.util :refer
             [defrecord-dynamically]]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Crux Reification code:

;;; abstract-spec -> real records, event reduction multifns,
;;; data-readers, etc.


;;;;;;;;;;;;;;;;;;;;

(defn -add-constructor-and-records-to-domain-spec
  [domain-spec record-symbol record-map]
  (-> domain-spec
      (update-in [:crux.reify/records]
                 assoc record-symbol record-map)
      (update-in [:crux.reify/constructors]
                 assoc record-symbol (:record-ctor record-map))))

(defn -add-command-to-event-mapping
  [domain-spec command-symbol event-symbol]
  (update-in domain-spec [:crux.reify/commands-to-events]
             assoc command-symbol event-symbol))

(defn -add-event-records-to-entity-mapping
  [domain-spec event-spec]
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
        fields              (FIELDS event-spec)
        full-command-symbol (FULL-COMMAND-SYMBOL event-spec)
        command-record-map  (defrecord-dynamically full-command-symbol fields)
        full-event-symbol   (FULL-EVENT-SYMBOL event-spec)
        event-record-map    (defrecord-dynamically full-event-symbol fields)]

    (-> domain-spec
        (-add-command-to-event-mapping
         (COMMAND-SYMBOL event-spec) event-symbol)

        (-add-command-to-event-mapping
         (FULL-COMMAND-SYMBOL event-spec) (FULL-EVENT-SYMBOL event-spec))

        (-add-constructor-and-records-to-domain-spec
         full-command-symbol command-record-map)

        (-add-constructor-and-records-to-domain-spec
         full-event-symbol event-record-map)

        (-add-event-records-to-entity-mapping event-spec))))


(defn -reify-events-and-commands-for-each-entity! [domain-spec]
  (reduce -reify-event-and-command!
          domain-spec
          (flatten (map #(vals (EVENTS %))
                        (vals (ENTITIES domain-spec))))))

;;;;;;;;;;;;;;;;;;;;

(defn -bind-properties-to-entity-record-ctor
  [entity-spec record-map]
  (let [properties  (get-in entity-spec [PROPERTIES])
        orig-ctor   (get-in record-map  [:record-ctor])]
    (assoc record-map :record-ctor
           #(vary-meta (orig-ctor %) assoc CRUX-PROPERTIES properties))))


(defn -reify-entity-record! [domain-spec entity-symbol]
  (let [entity-spec (get-in domain-spec [ENTITIES entity-symbol])
        record-map  (defrecord-dynamically entity-symbol (FIELDS entity-spec))
        record-map  (-bind-properties-to-entity-record-ctor
                       entity-spec record-map)]
    (-> domain-spec
        (update-in [ENTITIES entity-symbol] merge record-map)
        (-add-constructor-and-records-to-domain-spec
            entity-symbol record-map))))


(defn -reify-all-entity-records! [domain-spec]
  (reduce -reify-entity-record! domain-spec (keys (ENTITIES domain-spec))))

;;;;;;;;;;;;;;;;;;;;


(defn -reify-domain-records!
  [domain-spec]
  (-> domain-spec
      -reify-all-entity-records!
      -reify-events-and-commands-for-each-entity!))
