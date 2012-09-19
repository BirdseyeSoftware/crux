(ns crux.reify
  (:require [crux.internal.keys :refer :all])
  (:require [crux.util :refer [defrecord-dynamically]]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Crux Reification code:
;;; abstract-spec -> real records, fns, protos/interfaces, etc.


(defn map-over-values [m modifier-fn]
  (into {} (for [[k v] m]
             [k (modifier-fn v)])))

(defn map-over-keys [m modifier-fn]
  (into {} (for [[k v] m]
             [(modifier-fn k) v])))

;;TODO: make this a configurable callback in the entity-map
(defn- -entity-subrecord-symbol [entity-symbol event-symbol record-type]
  (symbol (case record-type
            :event (format "%s%s" entity-symbol event-symbol)
            :command (format "%s%s" event-symbol entity-symbol))))

(defn reify-event-or-command-record-for-entity-event!
  [domain-spec [entity-symbol event-symbol command-or-event-keyword]]
  (let [command-or-event-rec-symbol
        (-entity-subrecord-symbol entity-symbol event-symbol
                                  command-or-event-keyword)
        fields (get-in domain-spec [ENTITIES entity-symbol
                                    EVENTS   event-symbol
                                    FIELDS])]
    (-> domain-spec
        (update-in [ENTITIES entity-symbol EVENTS event-symbol]
                   merge
                   (into {}
                         (for [[k v]
                               (defrecord-dynamically
                                 command-or-event-rec-symbol fields)]
                           [(keyword (format "%s-%s"
                                             (name command-or-event-keyword)
                                             (name k))) v]))))))

(defn reify-event-or-command-records-for-entity!
  [domain-spec [entity-symbol command-or-event-keyword]]
  (let [entity-spec (get-in domain-spec [ENTITIES entity-symbol])
        record-symbols (keys (EVENTS entity-spec))
        reduce-inputs (map #(do [entity-symbol % command-or-event-keyword])
                           record-symbols)]
    (reduce reify-event-or-command-record-for-entity-event!
            domain-spec reduce-inputs)))

(defn reify-entity-record! [domain-spec entity-symbol]
  (let [entity-spec (get-in domain-spec [ENTITIES entity-symbol])
        fields (FIELDS entity-spec)
        properties (PROPERTIES entity-spec)
        defrecord-map (defrecord-dynamically entity-symbol fields)
        orig-ctor (:record-ctor defrecord-map)
        crux-meta-fields {CRUX-PROPERTIES properties}
        defrecord-map (assoc defrecord-map
                        :record-ctor
                        (fn [m] (orig-ctor
                                 (merge crux-meta-fields m))))]
    (update-in domain-spec [ENTITIES entity-symbol]
               merge defrecord-map)))

(defn reify-all-entity-records! [domain-spec]
  (reduce reify-entity-record! domain-spec (keys (ENTITIES domain-spec))))

(defn reify-events-or-commands-for-each-entity! [domain-spec]
  (let [entity-symbols (keys (ENTITIES domain-spec))
        reduce-arguments (concat (map #(do [% :event])
                                      entity-symbols)
                                 (map #(do [% :command])
                                      entity-symbols))]
    (reduce reify-event-or-command-records-for-entity! domain-spec
            reduce-arguments)))


;; {
;;  ['Entity] (atom {id (atom {})})
;; }

(defn reify-get-entity-function [domain-spec]
  (let [entity-caches-map (map-over-values (ENTITIES domain-spec)
                                           (constantly (atom {})))]
    (-> domain-spec
        (assoc :entity-caches-map entity-caches-map)
        (assoc 
            :crux/get-entity
          (fn get-entity-fn [entity-symbol entity-id #_rev]
            (let [entity-cache-atom (get entity-caches-map entity-symbol)
                  entity-atom (get @entity-cache-atom entity-id)]
              (if entity-atom
                @entity-atom
                nil)))))))

(defn reify-domain-records! [domain-spec]
  (-> domain-spec
      reify-all-entity-records!
      reify-events-or-commands-for-each-entity!))


(defn reify-domain-spec! [domain-spec]
  (-> domain-spec
      reify-domain-records!
      reify-get-entity-function))

;; (defn defmulti-event-reducer [domain-map multi-name entity-symbol]
;;   (let [events (get-in domain-map
;;                        [ENTITIES entity-symbol EVENTS])]
;;     (eval `(defmulti ~multi-name (fn [entity# event#] (type event#))))
;;     (doseq [[event-symbol event-map] events]
;;       (let [event-symbol (-entity-event-symbol entity-symbol
;;                                                event-symbol)
;;             reducer-fn (REDUCER event-map)]
;;         (when-not reducer-fn
;;           (throw+ {:type :library/specify-reducer-error
;;                    :msg ""}))
;;         (eval `(defmethod ~multi-name ~event-symbol [state0# evt#]
;;                  (~reducer-fn state0# evt#)))))
;;     domain-map))
