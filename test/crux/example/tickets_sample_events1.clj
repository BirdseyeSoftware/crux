{:entity Ticket
 :name "Ticket test 1"
 :events [
          #tickets/TicketCreated {:title "Ticket 1 Foo"}
          #tickets/TicketDetailsChanged {:title "Ticket 1 ok"}
          #tickets/TicketAssigned {:assignee 123}
          #tickets/TicketClosed {}
          ]
 :expected {:title "Ticket 1 ok" :assignee nil :state :closed}}

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