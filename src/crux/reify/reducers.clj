(ns crux.reify.reducers
  (:require [slingshot.slingshot :refer (throw+)])
  (:require [crux.internal.keys :refer :all]
            [crux.util :refer
             [defrecord-dynamically
              add-method-to-multi]]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Reify event reducers

(defn -reify-multi-for-event-reducer!
  [entity-symbol]
  (let [multifn-name (symbol (format "%s-event-reducer" entity-symbol))
        _            (eval `(defmulti ~multifn-name
                              (fn [entity# event#] (type event#))))]
   (eval `~multifn-name)))

(defn -reify-entity-event-reducer!
  [domain-spec entity-symbol]
  (let [multifn     (-reify-multi-for-event-reducer! entity-symbol)
        event-specs (get-in domain-spec  [ENTITIES entity-symbol EVENTS])]

    (doseq [[event-symbol event-spec] event-specs]
      (let [event-rec-symbol (FULL-EVENT-SYMBOL event-spec)
            reducer-fn       (REDUCER event-spec)
            record-map       (get-in domain-spec
                                     [:crux.reify/records event-rec-symbol])]
        (when-not reducer-fn
          (throw+ {:type :library/specify-reducer-error
                   :msg ""}))

        (add-method-to-multi
         multifn (:record-class record-map) reducer-fn)))

    (update-in domain-spec [:crux.reify/reducers]
               assoc entity-symbol multifn)))

(defn -reify-all-entity-event-reducers!
  [domain-spec]
  (reduce -reify-entity-event-reducer!
          domain-spec (keys (ENTITIES domain-spec))))


(defn -reify-entity-event-reducer!
  [domain-spec entity-symbol]
  (let [multifn     (-reify-multi-for-event-reducer! entity-symbol)
        event-specs (get-in domain-spec  [ENTITIES entity-symbol EVENTS])]

    (doseq [[event-symbol event-spec] event-specs]
      (let [event-rec-symbol (FULL-EVENT-SYMBOL event-spec)
            reducer-fn       (REDUCER event-spec)
            record-map       (get-in domain-spec
                                     [:crux.reify/records event-rec-symbol])]
        (when-not reducer-fn
          (throw+ {:type :library/specify-reducer-error
                   :msg ""}))

        (add-method-to-multi
         multifn (:record-class record-map) reducer-fn)))

    (update-in domain-spec [:crux.reify/reducers]
               assoc entity-symbol multifn)))

(defn -reify-all-entity-event-reducers!
  [domain-spec]
  (reduce -reify-entity-event-reducer!
          domain-spec (keys (ENTITIES domain-spec))))
