(ns crux.example.tickets
  (:require [clojure.pprint :refer (pprint)])
  (:require [crux.domain :refer :all])
  (:require [crux.reify :refer [reify-domain-records!]]))

#_(check-validators #_entity & validators)
#_(check-constraints #_entity & constraints)


(defn or* [o & preds]
  (some (fn -or* [pred] (pred o)) preds))

(defn and* [o & preds]
  (every? (fn -and* [pred] (pred o)) preds))

(defn meets-constraints? [entity & constraints]
  (and* entity constraints))

(defn validate [[entity command user] & validators]
  (or* [entity command user] validators))

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
      {:constraints [open? ok?]
       :validations [(or* active?)]}
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
  (get-in tickets-domain [:entities 'Ticket :properties]))
(def ticket-constraints
  (get-in tickets-domain [:entities 'Ticket :events 'Assigned :constraints]))

(defn get-entity-ctor [domain-spec entity]
  (get-in domain-spec [:entities entity :record-ctor]))

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

(def reified-tickets-domain (reify-domain-records! tickets-domain))
reified-tickets-domain

(def ticket-entity-spec
  (get-in reified-tickets-domain [:entities 'Ticket ]))

(def assigned-event-spec
  (get-in ticket-entity-spec [:events 'Assigned]))

(def t1 (construct reified-tickets-domain 'Ticket {:state :open :title "not-ok"}))

(unmet-constraints t1 ticket-constraints)

(defn check [{:keys [entity event user]}
             validation-value error-msg]
  (when (not validation-value)
    error-msg))

(def v1 (gen-single-command-validator
         ticket-entity-spec
         assigned-event-spec '(check (ok? entity) "fail")))
