(ns crux.test.domain-test
  (:require [clojure.test :refer :all]
            [clojure.data :refer [diff]]
            [slingshot.slingshot :refer [throw+]]
            [crux.example.tickets :as tickets]
            [crux.domain :as domain]
            [crux.internal.keys :refer :all]
            [crux.util :refer [map-over-keys submap?]])
  (:require [clojure.java.io :refer [input-stream reader]])
  (:import [crux.domain DomainSpec])
  (:import [clojure.lang LineNumberingPushbackReader])
  (:import [java.io File]))


;; Stolen from clojure.contrib.with-ns
(defmacro with-ns
  "Evaluates body in another namespace. ns is either a namespace
object or a symbol. This makes it possible to define functions in
namespaces other than the current one."
  [ns & body]
  `(binding [*ns* (the-ns ~ns)]
     ~@(map (fn [form] `(eval '~form)) body)))



(defmacro with-test-ns
  "Evaluates body in an anonymous namespace, which is then immediately
removed. The temporary namespace will 'refer' clojure.core."
  [name & body]
  `(do
     (when-let [test-ns# (find-ns '~name)]
       (remove-ns 'test-ns#))

     (create-ns '~name)
     (let [test-ns# (find-ns '~name)]
       (let [result# (with-ns '~name
                       (clojure.core/refer-clojure)
                       ~@body)]
         result#))))


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

(defn -read-file
  ;; see https://github.com/jonase/kibit/blob/master/src/kibit/check.clj
  "Gen a lazy sequence of top level forms from a LineNumberingPushbackReader"
  [^LineNumberingPushbackReader r]
  (lazy-seq
    (let [form (try
                 (read r false ::eof)
                 (catch Exception e
                   (throw (Exception.
                           (str "reader crashed"
                                (.getMessage e)) e))))]
      (when-not (= form ::eof)
        (cons form (-read-file r))))))

(defn read-file [file-or-path]
  (-read-file
   (LineNumberingPushbackReader. (reader file-or-path))))

(defn get-domain-data-readers [domain-spec prefix]
  (map-over-keys
   #(symbol (format "%s/%s" prefix %))
   (:crux.reify/constructors domain-spec)))

(defn assert-reduction
  ([reducer-fn init-st events expected-final-st]
     (let [final (reduce reducer-fn init-st events)]
       (is (submap? expected-final-st final)))))

(defn -perform-event-test [test-map domain-spec]
  (let [{entity-symbol :entity
         :keys [initial events expected]} test-map
        entity-ctor (get-in domain-spec [:crux.reify/constructors entity-symbol])
        entity-reducer (get-in domain-spec [:crux.reify/reducers entity-symbol])
        entity0 (entity-ctor (or initial {}))]
    (assert-reduction entity-reducer entity0 events expected)))

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

(deftest read-events-from-file
  (let [fpath "test/crux/example/tickets_sample_events1.clj"
        domain-spec (tickets/build-reified-test-domain-spec)
        data-readers (get-domain-data-readers domain-spec 'tickets)
        ;; _ (println (keys (get-in domain-spec [:crux.reify/constructors])))
        ]
    (binding [*data-readers* data-readers]
      (doseq [test-map (read-file fpath)]
        (let [{entity-symbol :entity
               :keys [name initial events command expected]} test-map]
          (cond
            (and events command)
            (throw+ (format "Specify just events or command on %s" name))
            events (-perform-event-test test-map domain-spec)
            command (-perform-command-test test-map domain-spec)))))))
