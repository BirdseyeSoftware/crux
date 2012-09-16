(ns crux.example.tickets
  (:require [clojure.pprint :refer (pprint)])
  (:require [crux.domain :refer :all])
  (:require [crux.reify :refer [reify-domain-records!]]))

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
     [Created Create [first last email locked] :merge]
     [Locked Lock [why] {:locked true}]
     ;; the following line should raise an exception
     ;;[Locked Lock [why] {:locked true}]
     )
    (properties
     {:active? '(not :locked)}))

  (entity Ticket ~ticket-fields
    (plural Tickets)
    (type-infer-rule #"\?$" :Bool)
    (field-types ~ticket-field-types)
    (field-types {Assigned/assignee :User})

    (events
     [Created Create ~ticket-fields  :merge]
     [DetailsChanged Change ~ticket-fields :merge]
     [Assigned Assign [assignee] :merge]
     [Read Read [user-id]
      [:conj :read-by user-id]
      [:inc :read-count]]
     [Closed Close []
      {:state :closed}
      [:dissoc :assignee]]
     [CommentAdded Comment [foo bar]
      [:conj :comments]]
     [AttachmentAdded Attach  [url private?]
      [:assoc :attachments]]
     [AttachmentDeleted DeleteAttachment
      [attachment-id]
      [:dissoc :attachments attachment-id]])

    (properties
     {:User/can-assign-ticket? nil})

    (command-validators
     [DetailsChanged
      [:ticket [:open?]]
      [:command-user [:active? :can-edit?]]
      [:event [:description-not-empty?]]]
     [Assign
      ;;[actor [policy ...] -> policy reified into
      ;; (fn [actor {:entity :event}] bool)
      [:command-user [:User/active? :User/can-assign-ticket?]]
      [:assignee [:User/active? :User/can-own-ticket?]]])))

(def reified-tickets-domain (reify-domain-records! tickets-domain))
reified-tickets-domain
