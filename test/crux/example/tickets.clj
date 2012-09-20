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

(defn build-test-domain-spec []
 (create-domain {:name "tickets-domain"}
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
    #_(conflict-detector )
    (properties
     last-reader (last (:read-by entity))
     open? (= :open (:state entity))
     ok?  (= "ok" (:title entity)))

    (events
     [Created Create ~ticket-fields
      (merge {:read-count 0}
             (filter second event))]

     [DetailsChanged Change ~ticket-fields
      (merge (filter second event))]

     [Assigned Assign [assignee]
      {:entity-constraints [ok?]
       :conflict-detector nil
       :validations [(check
                      (open? entity)
                      ;(prop (get-entity 'User assignee) :active?)
                      "Assignee must be active user")]}
      (merge {:ok (ok? entity)} event)]

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
     ok? (= (:title entity) "not-ok")))))


(defn build-reified-test-domain-spec []
  (-> (build-test-domain-spec)
      reify-domain-spec!))

;(build-test-domain-spec)

