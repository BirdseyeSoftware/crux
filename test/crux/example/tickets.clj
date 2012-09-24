(ns crux.example.tickets
  (:require [crux.internal.keys :refer :all])
  (:require [slingshot.slingshot :refer (throw+)])
  (:require [clojure.pprint :refer (pprint)]
            [clojure.string :as str])
  (:require [crux.domain :refer :all])
  (:require [crux.reify :refer [reify-domain-spec!]]))

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

(defdomain tickets
  (entity User [first last email locked]
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
     {last-reader (last (:read-by entity))
      open? (= :open (:state entity))
      ok?  (= "ok" (:title entity))})

    (events
      [Created Create entity-fields
       (merge {:read-count 0
               :read-by []}
              (filter second event))]

      [DetailsChanged Change entity-fields
       (merge (filter second event))]

      [Assigned Assign [assignee]
       {:entity-constraints [ok?]
        :conflict-detector nil
        :validations [(check
                       (open? entity)
                       "Assignee must be active user")]}
       (assoc :assignee assignee)]

      [Read Read [user-id]
       (update-in [:read-by] conj user-id)
       (update-in [:read-count] inc)]

      [Closed Close []
       (assoc :state :closed)
       (assoc :assignee nil)]

      [CommentAdded Comment [foo bar]
       (update-in [:comments] conj event)]

      [AttachmentAdded Attach  [url private?]
       (update-in [:attachments] merge {event-id event})]

      [AttachmentDeleted DeleteAttachment
       [attachment-id]
       (update-in [:attachments] dissoc event-id)])

    (properties
     {ok? (= (:title entity) "not-ok")})))

(defn build-test-domain-spec []
  tickets)

(def reified-tickets  (reify-domain-spec! tickets))

(defn build-reified-test-domain-spec []
  reified-tickets)
