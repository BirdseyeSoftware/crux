(ns crux.test.domain-test
  (:require [clojure.test :refer :all]
            [clojure.data :refer [diff]]
            [clojure.pprint :refer [pprint]]
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
              read-domain-event-log]])
  (:import [crux.domain DomainSpec]))


(deftest test-domain-is-built-correctly
  (let [tickets-domain (tickets/build-test-domain-spec)]
    (is (instance? DomainSpec tickets-domain))))

(deftest test-reifier-is-building-domain-correctly
  (let [ticket-domain (tickets/build-test-domain-spec)]
    (with-test-ns tickets-test
      (require '[crux.reify :refer (reify-domain-spec!)])
      (require '[crux.example.tickets :as tickets])
      (-> (tickets/build-test-domain-spec)
          reify-domain-spec!))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
(defn assert-reduction
  ([reducer-fn init-state events expected-final-state]
     (let [final (reduce reducer-fn init-state events)]
       (is (submap? expected-final-state final)))))

(defn -perform-event-test [test-map domain-spec]
  (let [{entity-symbol :entity
         :keys [initial events expected]} test-map
        entity-ctor (get-in domain-spec [:crux.reify/constructors entity-symbol])
        entity-reducer (get-in domain-spec [:crux.reify/reducers entity-symbol])
        entity0 (entity-ctor (or initial {}))]
    (assert-reduction entity-reducer entity0 events expected)))

(defn -perform-command-test [test-map domain-spec]
  (let [{entity-symbol :entity
         :keys [initial command expected]} test-map
         entity-ctor (get-in domain-spec
                             [:crux.reify/constructors entity-symbol])
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


(defn test-reductions [entity0 events reducer & [begin & [end]]]
  (let [event-reductions (reductions reducer entity0 events)
        begin (or begin 0)
        zipped (into [] (map vector events (rest event-reductions)))
        end (or end (count zipped))
        ]
    (subvec zipped begin end)))

(deftest test-get-domain-events
  (let [events (read-domain-event-log
                (tickets/build-reified-test-domain-spec)
                "test/crux/example/ticket_sample_events2.clj"
                'tickets)]
    (is events)))

(deftest test-get+set-entity-on-reified-domain
  (let [file-path "test/crux/example/tickets_sample_events1.clj"
        domain-spec (tickets/build-reified-test-domain-spec)
        test-map (first
                  (read-domain-data-from-file domain-spec file-path 'tickets))

        {entity-symbol :entity
         :keys [initial events expected]} test-map
        entity-ctor (get-in domain-spec [:crux.reify/constructors entity-symbol])
        entity-reducer (get-in domain-spec [:crux.reify/reducers entity-symbol])
        entity0 (entity-ctor (or initial {}))

        get-entity (:crux.reify/get-entity domain-spec)
        set-entity (:crux.reify/set-entity domain-spec)

        reduction-test (fn [ent0 ev]
                         (let [ent (entity-reducer ent0 ev)]
                           (set-entity entity-symbol 1 entity0)
                           (is (= entity0 (get-entity entity-symbol 1)))
                           ent))]

    (doseq [[event red]
            (test-reductions entity0 events entity-reducer)]
      (println "")
      (print (format "#tickets/%s" (.getSimpleName (type event))))
      (pprint (into {} (filter second event)))
      (print "\n   #_==>  #tickets/Ticket")
      (pprint (into {}
                    (filter second red))))
    (is (submap? expected
                 (reduce reduction-test entity0 events)))))


(deftest test-ticket-events-from-file
  (let [file-path "test/crux/example/tickets_sample_events1.clj"
        domain-spec (tickets/build-reified-test-domain-spec)]
    (run-domain-test-specs-from-file domain-spec file-path 'tickets)))

(deftest test-order-events-from-file
  (let [file-path "test/crux/example/orders_test_specs.clj"
        domain-spec (orders/build-reified-test-domain-spec)]
    (run-domain-test-specs-from-file domain-spec file-path 'orders)))
