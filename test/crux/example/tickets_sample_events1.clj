{:entity Ticket
 :name "Ticket test 1"
 :events [
          #tickets/TicketCreated {:title "Foo"}
          #tickets/TicketDetailsChanged {:title "Bar"}
          #tickets/TicketAssigned {:assignee 987}
          #tickets/TicketAssigned {:assignee 123}
          #tickets/TicketRead {:user-id 444}
          #tickets/TicketClosed {}
          ]
 :expected {:title "Bar"
            :assignee nil
            :state :closed
            :read-by [444]
            :read-count 1}}

{:entity User
 :name "User test 1"
 :initial {:first "Tavis"}
 :events [
          #tickets/UserLocked {:why "Because he drinks to much coffee"}
          ]
 :expected {:first "Tavis" :locked true}}

{:entity Ticket
 :name "Ticket test2"
 :initial {:title "Already here"}
 :events [#tickets/TicketDetailsChanged {:body "lies under the sea"}]
 :expected {:assignee nil :title "Already here"}}

{:entity Ticket
 :name "Ticket command 1"
 :initial #tickets/Ticket {:status :closed}
 :command #tickets/AssignTicket {:assignee 123}
 :failure true
 }
