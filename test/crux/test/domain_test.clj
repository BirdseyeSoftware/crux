(ns crux.test.domain-test
  (:require [clojure.test :refer :all]
            [clojure.data :refer [diff]]
            [clojure.pprint :refer [pprint]]
            [slingshot.slingshot :refer [throw+]]

            [crux.example.tickets :as tickets]
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
;;; unfinished command handling tests

;; (defn handle-command-pseudo-code [event-spec command-symbol command]
;;     (let [{entity-id :id
;;            entity-rev :rev} (:crux/target command)
;;            ;; rest of this atomically
;;            entity-from-cache (get-entity 'Ticket entity-id)
;;            entity-current-tip (:crux/rev entity-from-cache)
;;            conflict-detector (:conflict-detector event-spec)
;;            conflict (conflict-detector
;;                      entity-id entity-rev
;;                      entity-from-cache
;;                      entity-current-tip)]
;;       (if-not conflict
;;         (let [constraints (COMMAND-CONSTRAINTS event-spec)
;;               constraint-result (unmet-constraints entity constraints)
;;               validations (COMMAND-VALIDATIONS spec)
;;               validation-result (unmet-validations {:entity entity
;;                                                     :event command
;;                                                     :user nil}
;;                                                    validations)

;;               reducer (REDUCER assigned-event-spec)]
;;           (when-not (empty? constraint-result)
;;             (throw+
;;              (format "Entity doesn't meet constraints: %s"
;;                      (str/join ", " constraint-result))))
;;           (when-not (empty? validation-result)
;;             (throw+ validation-result))
;;           (commit-events! entity [(map->TicketAssigned command)]))
;;         (bitch-about conflict))))


;; (defn -handle-assignment [entity command]
;;   (let [reducer (REDUCER assigned-event-spec)
;;         validations (COMMAND-VALIDATIONS assigned-event-spec)
;;         constraints (COMMAND-CONSTRAINTS assigned-event-spec)
;;         constraint-result (unmet-constraints entity constraints)
;;         validation-result (unmet-validations {:entity entity
;;                                         :event command
;;                                         :user nil}
;;                                        validations)]
;;     (when-not (empty? constraint-result)
;;       (throw+
;;        (format "Entity doesn't meet constraints: %s"
;;                (str/join ", " constraint-result))))
;;     (when-not (empty? validation-result)
;;       (throw+ validation-result))
;;     ;(reducer entity (map->TicketAssigned command))
;;     ))

;; #_(unmet-validations {:user 123 :entity
;;           (merge t1 {:state :closed :title "ok"})
;;                     :event {}}
;;                    {'(some-form) v1
;;                     '(other) v2})

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
                           (set-entity 'Ticket 1 entity0)
                           (is (= entity0 (get-entity 'Ticket 1)))
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


(deftest test-read-events-from-file
  (let [file-path "test/crux/example/tickets_sample_events1.clj"
        domain-spec (tickets/build-reified-test-domain-spec)]
    (doseq [test-map (read-domain-data-from-file
                      domain-spec file-path 'tickets)]
      (let [{entity-symbol :entity
             :keys [name initial events command expected]} test-map]
        (cond
          (and events command)
          (throw+ (format "Specify just events or command on %s" name))
          events (-perform-event-test test-map domain-spec)
          command (-perform-command-test test-map domain-spec))))))
