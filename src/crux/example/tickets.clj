(ns crux.example.tickets
  (:require [crux.internal.keys :refer :all])
  (:require [slingshot.slingshot :refer (throw+)])
  (:require [clojure.pprint :refer (pprint)]
            [clojure.string :as str])
  (:require [crux.domain :refer :all])
  (:require [crux.reify :refer [reify-domain-spec!]]))

(defn check [{:keys [entity event user]}
             validation-value error-msg]
  (when (not validation-value)
    error-msg))

(def ticket-field-types
  '{oid :guid
    parent-ticket :Ticket
    title :String
    body :String
    creator :User
    assignee :User
    state :Ticket/state
    priority :Ticket/priority
    category :Ticket/category
    listeners :list.of.User
    read-by :list.of.User
    read-count :Integer
    attachments :list.of.String
    reminders :list.of.Datetime})

(def ticket-fields (into [] (keys ticket-field-types)))
;; optional vs required params
(defdomain tickets-domain
  (field-types {user-id :User
                name :String})


  (entity User [first last email]
          (id-field oid :Guid)
    (plural Users)
    (events
     [Created Create [first last email locked]
      (merge event)]
     [Locked Lock [why]
      (assoc :locked true)]))

  (entity Ticket ~ticket-fields
          (plural Tickets)
    (type-infer-rule #"\?$" :Bool)
    (field-types ~ticket-field-types)
    (field-types {Assigned/assignee :User})
    #_ (conflict-detector )
    (properties
     last-reader (last (:read-by entity))
     open? (= :open (:state entity))
     ok?  (= "ok" (:title entity)))

    (events
     [Created Create ~ticket-fields
      (merge event)]

     [DetailsChanged Change ~ticket-fields
      (merge event)]

     [Assigned Assign [assignee]
      {:constraints [ok?]
       :conflict-detector nil
       :validations [(check
                      (open? entity)
                      ;(prop (get-entity 'User assignee) :active?)
                      "Assignee must be active user")]}
      (merge event)]


     #_(command-validators
        [DetailsChanged
         [:ticket [:open?]]
         [:command-user [:active? :can-edit?]]
         [:event [:description-not-empty?]]]
        [Assign
         ;;[actor [policy ...] -> policy reified into
         ;; (fn [actor {:entity :event}] bool)
         [:command-user [:User/active? :User/can-assign-ticket?]]

         valid?

         ((active? *user*)  "Sorry you need to be an active user")
         (check false )
         (check (or (open? entity)
                    (p2? *))
                "must be open")
         (non-nil? event [:description])
         (check (not (empty? (get-in event [:description]))) "error")

         ;;[:assignee [:User/active? :User/can-own-ticket?]]
         ])

     [Read Read [user-id]
      (update-in [:read-by] conj user-id)
      (update-in [:read-count] inc)]

     [Closed Close []
      (assoc :state :closed)
      (dissoc :assignee)]

     [CommentAdded Comment [foo bar]
      (update-in [:comments] conj event)]

     [AttachmentAdded Attach  [url private?]
      ;; entity
      ;; event
      ;; event-id
      ;; entity-id
      ;; (crux/assoc-in )
      (update-in [:attachments] merge {event-id event})]


     [AttachmentDeleted DeleteAttachment
      [attachment-id]
      (update-in [:attachments] dissoc event-id)])
    (properties
     ok? (= (:title entity) "not-ok"))))

(def ticket-properties
  (get-in tickets-domain [ENTITIES 'Ticket PROPERTIES]))
(def ticket-constraints
  (get-in tickets-domain [ENTITIES 'Ticket EVENTS 'Assigned CONSTRAINTS]))

(defn get-entity-ctor [domain-spec entity]
  (get-in domain-spec [ENTITIES entity :record-ctor]))

(defn construct [domain-spec entity init-fields]
  ((get-entity-ctor domain-spec entity) init-fields))


;; (defmacro check [form message])
;; (defn open? [ticket]
;;   (= (:state ticket) :open))

;; (defn has-name? [user]
;;   (-> (:name user) nil? not))

;; (defn non-nil? [o fields]
;;   (check (not (nil? (get-in o fields)))
;;          "failed"))

;; (def valid? (check (or (open? entity)
;;                 (p2? *))
;;             "must be open"))

(def reified-tickets-domain (reify-domain-spec! tickets-domain))

;; {
;;  ['Entity] (atom {id (atom {})})
;; }

(def get-entity (:crux/get-entity reified-tickets-domain))

(defn set-entity [domain-spec entity-symbol entity-id entity]
  (if-let [entity-cache-atom (get-in domain-spec
                                     [:entity-caches-map entity-symbol])]
    (swap! entity-cache-atom
           (fn [entity-cache-map]
             (let [entity-atom (get entity-cache-map entity-id)
                   created-new-atom? (nil? entity-atom)
                   entity-atom (or entity-atom ; may be nil
                                   (atom nil))]
               (swap! entity-atom (constantly entity))
               (if created-new-atom?
                 (assoc entity-cache-map entity-id entity-atom)
                 entity-cache-map))))
    (throw+ (format "Entity `%s` not defined in domain" entity-symbol)))
  
  #_(swap! (get-in domain-spec [:entity-caches-map entity-symbol])
           (fn [entity-cache-map]
             (if-let [entity-atom (get entity-cache-map entity-id)]
               (swap! entity-atom (constantly entity)))
             (atom entity))))

(def t1 (construct reified-tickets-domain 'Ticket
                   {:oid 1
                    :state :open :title "not-ok"}))


(set-entity reified-tickets-domain 'Ticket 1 t1)

;;(defn commit-events! [update-fn])

#_(defn handle-command-pseudo-code [event-spec command-symbol command]
    (let [{entity-id :id
           entity-rev :rev} (:crux/target command)
           ;; rest of this atomically
           entity-from-cache (get-entity 'Ticket entity-id)
           entity-current-tip (:crux/rev entity-from-cache)
           conflict-detector (:conflict-detector event-spec)
           conflict (conflict-detector
                     entity-id entity-rev
                     entity-from-cache
                     entity-current-tip)]
      (if-not conflict
        (let [constraints (COMMAND-CONSTRAINTS event-spec)
              constraint-result (unmet-constraints entity constraints)
              validations (COMMAND-VALIDATIONS spec)
              validation-result (unmet-validations {:entity entity
                                                    :event command
                                                    :user nil}
                                                   validations)
           
              reducer (REDUCER assigned-event-spec)]
          (when-not (empty? constraint-result)
            (throw+
             (format "Entity doesn't meet constraints: %s"
                     (str/join ", " constraint-result))))
          (when-not (empty? validation-result)
            (throw+ validation-result))
          (commit-events! entity [(map->TicketAssigned command)]))
        (bitch-about conflict))))


#_(defn -handle-assignment [entity command]
  (let [reducer (REDUCER assigned-event-spec)
        validations (COMMAND-VALIDATIONS assigned-event-spec)
        constraints (COMMAND-CONSTRAINTS assigned-event-spec)
        constraint-result (unmet-constraints entity constraints)
        validation-result (unmet-validations {:entity entity
                                        :event command
                                        :user nil}
                                       validations)]
    (when-not (empty? constraint-result)
      (throw+
       (format "Entity doesn't meet constraints: %s"
               (str/join ", " constraint-result))))
    (when-not (empty? validation-result)
      (throw+ validation-result))
    ;(reducer entity (map->TicketAssigned command))
    ))


;; (def ticket-entity-spec
;;   (get-in reified-tickets-domain [ENTITIES 'Ticket ]))

;; (def assigned-event-spec
;;   (get-in ticket-entity-spec [EVENTS 'Assigned]))



;; ;(def assignment1 (map->AssignTicket {:assignee 123}))


;; ;; (handle-assignment (merge t1 {:title "not-ok"
;; ;;                               :state "closed"}) assignment1)

;; #_(unmet-validations {:user 123 :entity
;;           (merge t1 {:state :closed :title "ok"})
;;                     :event {}}
;;                    {'(some-form) v1
;;                     '(other) v2})

