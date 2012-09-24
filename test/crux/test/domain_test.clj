(ns crux.test.domain-test
  (:require [clojure.test :refer :all]
            [clojure.data :refer [diff]]
            [clojure.pprint :refer [pprint]]
            [clojure.set :as set]
            [slingshot.slingshot :refer [throw+]]

            [crux.example.tickets :as tickets]
            [crux.example.orders-with-credit-allowance
             :as orders]

            [crux.domain :as domain]
            [crux.internal.keys :refer :all]
            [crux.util :refer [submap?
                               with-test-ns]]
            [crux.reify :refer
             [get-domain-data-readers
              read-domain-data-from-file
              read-domain-event-log
              get-reducer
              get-entity-ctor
              reduce-events
              reduction-of-events
              event-log-to-reductions-report]])
  (:import [crux.domain DomainSpec]))

(deftest test-domain-is-built-correctly
  (let [tickets-domain (tickets/build-test-domain-spec)]
    (is (instance? DomainSpec tickets-domain))))

(deftest test-reifier-is-building-domain-correctly
  (let [ticket-domain (tickets/build-test-domain-spec)]
    (with-test-ns tickets-test
      (require '[crux.reify :refer (reify-domain-spec!)])
      (require '[crux.example.tickets :as tickets])
      ;; TODO add tests for more than just barfs ...
      (-> (tickets/build-test-domain-spec)
          reify-domain-spec!))))

(deftest test-read-domain-event-log
  (let [events (read-domain-event-log
                (tickets/build-reified-test-domain-spec)
                "test/crux/example/ticket_sample_events2.clj"
                )]
    (is (-> events empty? not))))

(deftest test-get+set-entity-on-reified-domain
  (let [file-path "test/crux/example/tickets_sample_events1.clj"
        domain-spec (tickets/build-reified-test-domain-spec)
        test-map (first (read-domain-data-from-file domain-spec file-path))
        {:keys [entity initial events expected]} test-map

        entity-ctor (get-entity-ctor domain-spec entity)
        entity-reducer (get-reducer domain-spec entity)
        entity0 (entity-ctor (or initial {}))

        get-entity (:crux.reify/get-entity domain-spec)
        set-entity (:crux.reify/set-entity domain-spec)
        reduction-test (fn [ent0 ev]
                         (let [ent (entity-reducer ent0 ev)]
                           (set-entity entity 1 entity0)
                           (is (= entity0 (get-entity entity 1)))
                           ent))]
    (is (submap? expected
                 (reduce reduction-test entity0 events)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#_(deftest test-print-reductions-on-orders-domain
  (let [file-path "test/crux/example/orders_test_specs.clj"
        domain-spec (orders/build-reified-test-domain-spec)
        {:keys [initial events expected]}
        (first (read-domain-data-from-file domain-spec file-path))]
    (print-event-reductions domain-spec events :highlight 4)))

#_(deftest test-print-reductions-on-tickets-domain
  (let [file-path "test/crux/example/tickets_sample_events1.clj"
        domain-spec (tickets/build-reified-test-domain-spec)
        {:keys [initial events expected]}
        (first (read-domain-data-from-file domain-spec file-path))]
    (print-event-reductions domain-spec events)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn -perform-event-test [test-map domain-spec]
  (let [{:keys [initial events expected]} test-map
        final (reduce-events domain-spec events initial)]
    (is (submap? expected final))))

(defn -perform-command-test [test-map domain-spec]
  (let [{entity-symbol :entity
         :keys [initial command expected]} test-map
         entity-ctor (get-entity-ctor domain-spec entity-symbol)
        ;; entity-command-handler (get-in domain-spec
        ;;                                [:crux.reify/command-handler entity-symbol])
         entity0 (entity-ctor (or initial {}))
         command-symbol (symbol (.getSimpleName (type command)))
         event-symbol (get-in domain-spec [:crux.reify/commands-to-events
                                           command-symbol])]

    #_(println command-symbol event-symbol command
             (get-in domain-spec [:crux.reify/commands-to-events]))
    (is true)
    ;; 1 get event-spec from command type
    ;; 2 get constraints, conflict-checker, validator
    ;; 3 check each for failure
    ;; 4 if failure then FAIL-record else 5
    ;; 5 find event-constructor from event-spec and call it with command fields

    ))

(defn run-domain-test-specs-from-file [domain-spec file-path & [domain-prefix]]
  (doseq [test-map (read-domain-data-from-file
                    domain-spec file-path domain-prefix)]
    (let [{entity-symbol :entity
           :keys [name initial events command expected]} test-map]
      (cond
        (and events command)
        (throw+ (format "Specify just events or command on %s" name))
        events (-perform-event-test test-map domain-spec)
        command (-perform-command-test test-map domain-spec)))))

(deftest test-ticket-events-from-file
  (let [file-path "test/crux/example/tickets_sample_events1.clj"
        domain-spec (tickets/build-reified-test-domain-spec)]
    (run-domain-test-specs-from-file domain-spec file-path 'tickets)))

(deftest test-order-events-from-file
  (let [file-path "test/crux/example/orders_test_specs.clj"
        domain-spec (orders/build-reified-test-domain-spec)]
    (run-domain-test-specs-from-file domain-spec file-path 'orders)))
