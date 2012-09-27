(ns crux.reify.store
  (:require [slingshot.slingshot :refer [throw+]])
  (:require [crux.internal.keys :refer :all]
            [crux.util :refer
             [type-symbol]])
  (:require [savant.core :as store]
            [savant.store.memory :as mem]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Saviant store integration with crux

;;; TODO
;;; We should be able to support multiple store types


;;; TODO
;;; We should be able to choose a store on the fly
;;; from the input parameters of -add-event-store


(defn get-entity-symbol-from-event-symbol
  [domain-spec event-symbol]
  (get (:crux.reify/event+command-symbols-to-entity-symbols domain-spec)
       event-symbol))


(defn get-event-store [domain-spec]
  (if-let [store (get-in domain-spec [:reified :event-store :store])]
    store
    (throw+ {:type :crux/store-not-declared
             :message "Ensure `-add-event-store` is executed on `domain-spec`
                       before using this function."})))

(defn get-store-event-fn
  [domain-spec]
  (if-let [result (get-in domain-spec
                          [:reified :event-store :store-event-fn])]
    result
    (throw+ {:type :crux/store-event-function-not-found
             :message "Ensure -reify-store-events-function is
                       executed on `domain-spec` before using this
                       function."})))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn -add-event-store [domain-spec store-args]
  (cond
    (contains? store-args :memory)
    (update-in domain-spec [:reified :event-store]
               assoc :store (mem/get-event-store (:memory store-args)))
    :else (throw+ {:type :crux/store-not-supported
                   :message "The only store supported is memory"})))

;;;;;;;;;;;;;;;;;;;;

(defn -get-entity-stream [store entity-symbol oid]
  (if (store/exists? store entity-symbol oid)
    (store/get-stream store entity-symbol oid)
    (store/create-stream store entity-symbol oid)))

(defn- -rehydrate-cached-entity-with-events
  [domain-spec entity-symbol entity-oid events]
  (let [get-entity (get-in domain-spec [:crux.reify/get-entity])
        set-entity (get-in domain-spec [:crux.reify/set-entity])
        reducer-fn (get-in domain-spec [:crux.reify/reducers entity-symbol])
        entity     (get-entity entity-symbol entity-oid)]
    (set-entity entity-symbol entity-oid (reduce reducer-fn entity events))
    entity))

(defn -reify-store-events-function
  [domain-spec]
  (let [store (get-event-store domain-spec)]
    (update-in domain-spec [:reified :event-store]
               assoc :store-event-fn
               (fn store-event-fn [events]
                 (let [events        (seq events)
                       event-symbol  (type-symbol (first events))
                       entity-symbol (get-entity-symbol-from-event-symbol
                                        domain-spec
                                        event-symbol)
                       {oid :oid
                        rev :rev}    (-> events first meta :crux/target)
                       entity-stream (-get-entity-stream
                                        store entity-symbol oid)]
                   (do
                     (store/commit-events! entity-stream events)
                     (-rehydrate-cached-entity-with-events
                       domain-spec entity-symbol oid events)))))))

;;;;;;;;;;;;;;;;;;;;

(defn- -decorate-get-entity
  [get-entity-from-cache domain-spec]
  (let [store      (get-in domain-spec [:reified :event-store :store])
        set-entity (get-in domain-spec [:crux.reify/set-entity])]

    (fn get-entity-from-store-fn
      [entity-symbol entity-oid #_rev]
      (let [reducer-fn (get-in domain-spec [:crux.reify/reducers entity-symbol])
            entity     (get-entity-from-cache entity-symbol entity-oid)
            stream     (store/get-stream store entity-symbol entity-oid)]
        ;; return either the cached entity or reduce events,
        ;; store it on cache, and return the result
        (or entity
            (let [created-entity (and stream
                                      (reduce reducer-fn
                                              nil
                                              (store/get-events-seq stream)))]
              (set-entity entity-symbol entity-oid created-entity)))))))

(defn -reify-get-entity-function [domain-spec]
  (let [store (get-in domain-spec [:reified :event-store :store])]
    (update-in domain-spec [:crux.reify/get-entity]
               -decorate-get-entity domain-spec)))
