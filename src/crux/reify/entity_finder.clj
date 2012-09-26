(ns crux.reify.entity-finder
  (:require [crux.internal.keys :refer :all]
            [crux.util :refer
             [map-over-values]]))

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
