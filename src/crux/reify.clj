(ns crux.reify
  (:require [slingshot.slingshot :refer [throw+]])
  (:require [crux.internal.keys :refer :all])
  (:require [crux.util :refer [defrecord-dynamically
                               addmethod-to-multi
                               map-over-keys map-over-values]]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Crux Reification code:
;;; abstract-spec -> real records, fns, protos/interfaces, etc.

(defn add-constructor-and-records-to-domain-spec
  [domain-spec record-symbol record-map]
  (-> domain-spec
      (update-in [:crux.reify/records]
                 assoc record-symbol record-map) 
      (update-in [:crux.reify/constructors]
                 assoc record-symbol (:record-ctor record-map))))

(defn add-command-to-event-mapping [domain-spec command-symbol event-symbol]
  (update-in domain-spec [:crux.reify/commands-to-events]
             assoc command-symbol event-symbol))

;; fugly, refactor
(defn reify-event-or-command-record-for-entity-event!
  [domain-spec [entity-symbol event-symbol command-or-event-keyword]]
  (let [event-spec (get-in domain-spec [ENTITIES entity-symbol
                                        EVENTS event-symbol])
        command-or-event-rec-symbol
        ;;TODO: make this naming a configurable callback in the entity-map
        (symbol (case command-or-event-keyword
                  :event (format "%s%s" entity-symbol event-symbol)
                  :command (format "%s%s"
                                   (COMMAND-SYMBOL event-spec)
                                   entity-symbol)))
        fields (FIELDS event-spec)
        record-map (defrecord-dynamically
                     command-or-event-rec-symbol fields)
        domain-spec (if (= :command command-or-event-keyword)
                      (add-command-to-event-mapping
                       domain-spec (COMMAND-SYMBOL event-spec)
                       event-symbol)
                      domain-spec)]
    (-> domain-spec
        (update-in [ENTITIES entity-symbol EVENTS event-symbol]
                   merge
                   (map-over-keys #(format "%s-%s"
                                           (name command-or-event-keyword)
                                           (name %))
                                  record-map))        
        (add-constructor-and-records-to-domain-spec
             command-or-event-rec-symbol record-map))))

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
                        (fn [m] (vary-meta
                                 (orig-ctor m)
                                 merge crux-meta-fields)))]
    (-> domain-spec
        (update-in [ENTITIES entity-symbol] merge defrecord-map)
        (add-constructor-and-records-to-domain-spec
            entity-symbol defrecord-map))))

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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; get-entity / set-entity

;; (defn set-entity [domain-spec entity-symbol entity-id entity]
;;   (if-let [entity-cache-atom (get-in domain-spec
;;                                      [:entity-caches-map entity-symbol])]
;;     (swap! entity-cache-atom
;;            (fn [entity-cache-map]
;;              (let [entity-atom (get entity-cache-map entity-id)
;;                    created-new-atom? (nil? entity-atom)
;;                    entity-atom (or entity-atom ; may be nil
;;                                    (atom nil))]
;;                (swap! entity-atom (constantly entity))
;;                (if created-new-atom?
;;                  (assoc entity-cache-map entity-id entity-atom)
;;                  entity-cache-map))))
;;     (throw+ (format "Entity `%s` not defined in domain" entity-symbol)))
  
;;   #_(swap! (get-in domain-spec [:entity-caches-map entity-symbol])
;;            (fn [entity-cache-map]
;;              (if-let [entity-atom (get entity-cache-map entity-id)]
;;                (swap! entity-atom (constantly entity)))
;;              (atom entity))))

;;(defn commit-events! [update-fn])

;; {
;;  ['Entity] (atom {id (atom {})})
;; }

(defn reify-get-entity-function [domain-spec]
  (let [entity-caches-map (map-over-values (constantly (atom {}))
                                           (ENTITIES domain-spec))]
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn reify-domain-records! [domain-spec]
  (-> domain-spec
      reify-all-entity-records!
      reify-events-or-commands-for-each-entity!))


(defn reify-entity-event-reducer! [domain-spec entity-symbol]
  (let [multi-name (symbol (format "%s-event-reducer" entity-symbol))
        events (get-in domain-spec  [ENTITIES entity-symbol EVENTS])
        multifn-var (eval `(defmulti ~multi-name
                             (fn [entity# event#] (type event#))))
        multifn (eval `~multi-name)]
    
    (doseq [[event-symbol event-spec] events]
      (let [event-rec-symbol
            (symbol (format "%s%s" entity-symbol event-symbol))
            reducer-fn (REDUCER event-spec)
            
            ;; reducer-fn (fn [ent ev]
            ;;              (reducer-fn
            ;;               ent
            ;;               (vary-meta ev assoc
            ;;                          CRUX-PROPERTIES
            ;;                          (CRUX-PROPERTIES (meta ent)))))
            
            record-map (get-in domain-spec
                               [:crux.reify/records event-rec-symbol])]
        (when-not reducer-fn
          (throw+ {:type :library/specify-reducer-error
                   :msg ""}))
        (addmethod-to-multi
         multifn (:record-class record-map) reducer-fn)))
    (update-in domain-spec [:crux.reify/reducers]
               assoc entity-symbol multifn)))

(defn reify-all-entity-event-reducers! [domain-spec]
  (reduce reify-entity-event-reducer!
          domain-spec (keys (ENTITIES domain-spec))) )

(defn reify-domain-spec! [domain-spec]
  (-> domain-spec
      reify-domain-records!
      reify-get-entity-function
      reify-all-entity-event-reducers!))


