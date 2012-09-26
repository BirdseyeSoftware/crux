(ns crux.reify.commands
  (:require [clojure.string :as str])
  (:require [slingshot.slingshot :refer [throw+]])
  (:require [crux.internal.keys :refer :all]
            [crux.util :refer
             [type-symbol]]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Command constraint and validation checking

(defn first-unmet-constraint [entity constraint-forms+preds]
  (some (fn -or* [[form pred]] (if-not (pred entity) form))
        constraint-forms+preds))

(defn unmet-constraints [entity constraint-forms+preds]
  (for [[form pred] constraint-forms+preds
        :when (not (pred entity))]
    form))

(defn unmet-validations [validation-args validation-forms+vfns]
  (for [[form vfn] validation-forms+vfns
        [error-message] [[(vfn validation-args)]]
        :when (not (nil? error-message))]
    {:error-message error-message
     :validation-form form}))

(defn first-unmet-validation [validation-args validation-forms+vfns]
  (first (unmet-validations validation-args validation-forms+vfns)))


;;;;;;;;;;;;;;;;;;;;
;;; Utility getters


(defn get-entity-symbol-from-command-symbol
  [domain-spec command-symbol]
  (get (:crux.reify/event+command-symbols-to-entity-symbols domain-spec)
       command-symbol))


(defn get-event-symbol-from-command-symbol
  [domain-spec command-symbol]
  (get (:crux.reify/commands-to-events domain-spec)
       command-symbol))


(defn get-check-command-constraints-fn
  [domain-spec]
  (if-let [result (get domain-spec :crux.reify/check-command-constraints-fn)]
    result
    (throw+ {:type :crux/check-command-constraint-fn-not-reified
             :message "Ensure -reify-check-command-constraints-fn was called on
                       domain spec before calling this function"})))


(defn get-validate-command-fn
  [domain-spec]
  (if-let [result (get domain-spec :crux.reify/validate-command-fn)]
    result
    (throw+ {:type :crux/validate-command-fn-not-reified
             :message "Ensure -reify-validate-command-fn was called on
                       domain spec before calling this function"})))


(defn get-command-constraints
  [domain-spec command-symbol]
  (let [entity-symbol (get-entity-symbol-from-command-symbol
                       domain-spec command-symbol)]
    (get-in domain-spec
            [ENTITIES entity-symbol COMMAND-CONSTRAINTS])))


(defn get-command-validations
  [domain-spec command-symbol]
  (let [entity-symbol (get-entity-symbol-from-command-symbol
                       domain-spec command-symbol)]
    (get-in domain-spec
            [ENTITIES entity-symbol COMMAND-VALIDATIONS])))

;;;;;;;;;;;;;;;;;;;;
;;; Reification process

(defn -reify-validate-command-function
  [domain-spec]
  (assoc domain-spec :crux.reify/validate-command-fn
         (fn validate-command [command entity user]
           (let [command-symbol    (type-symbol command)
                 validations       (get-command-validations
                                    domain-spec command-symbol)
                 validation-args   {:entity entity
                                    :user user
                                    :command command}
                 validation-errors (unmet-validations
                                    validation-args validations)]

             (when-not (empty? validation-errors)
               (throw+ {:type :crux/validations-not-met
                        :message
                        (format "%d errors found:\n%s"
                                (str/join "\n"
                                          (map :error-message
                                               validation-errors)))}))))))


(defn -reify-check-command-constraints-function
  [domain-spec]
  (assoc domain-spec :crux.reify/check-command-constraints-fn
         (fn check-command-constraints [command entity]
           (let [command-symbol (type-symbol command)
                 constraints    (get-command-constraints
                                 domain-spec command-symbol)

                 constraint-failures (unmet-constraints entity constraints)]
             (when-not (empty? constraint-failures)
               (throw+ {:type :crux/constraints-not-met
                        :message
                        (format "Constraints were not met: %s"
                                (str/join "\n"
                                          (map str
                                               constraint-failures)))}))))))


(defn -build-event-from-command
  [domain-spec command]
  (let [command-symbol (type-symbol command)
        event-symbol (get-in domain-spec
                             [:crux.reify/commands-to-events command-symbol])
        event-ctor    (get-in domain-spec
                              [:crux.reify/constructors event-symbol])]
   (event-ctor command)))


(defn -reify-command->event-fn [domain-spec]
    (fn command-handler [command]
      (let [;;; get meta data needed in order to process the command
            {entity-oid :oid
             entity-rev :rev
             user-oid :user-oid} (:crux/target (meta command))

             ;;; get-command+event+entity-symbols-from-domain-spec-and-command
             command-symbol   (type-symbol command)
             entity-symbol    (get-entity-symbol-from-command-symbol
                               domain-spec command-symbol)

             check-command-constraints (get-check-command-constraints-fn
                                         domain-spec)
             validate-command          (get-validate-command-fn
                                         domain-spec)

             ;;; entity of the command
             get-entity       (:crux.reify/get-entity domain-spec)
             entity           (get-entity entity-symbol
                                          entity-oid
                                          #_entity-rev)

             ;; TODO: set a variable that has the 'User entity.
             user             (get-entity 'User user-oid)
             ]
        (do
          (check-command-constraints command entity)
          (validate-command command entity user)
          (-build-event-from-command domain-spec command)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; NOTE
;;;;;;;;;
;;; This function _requires_ reify-check-command-constraints-fn
;;; and -reify-validate-command-fn to be called first on the domain-spec
;;; otherwise it won't know how to get command's constraint and validations
;;; functions
(defn -reify-command->event-fn-to-domain-spec
  [domain-spec]
  (assoc domain-spec
    :crux.reify/command->event-fn (-reify-command->event-fn domain-spec)))
