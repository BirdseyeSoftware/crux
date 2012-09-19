{:entity Ticket
 :events [
          #tickets/TicketCreated {:title "Ticket 1 Foo"}
          #tickets/TicketDetailsChanged {:title "Ticket 1 ok"}
          #tickets/TicketAssigned {:assignee 123}
          #tickets/TicketClosed {}
          ]
 :expected {:title "Ticket 1 ok" :assignee nil :state :closed}}

{:entity User
 :initial #tickets/User {:first "Tavis"}
 :events [
          #tickets/UserLocked {:why "Because he drinks to much coffee"}
          ]
 :expected {:first "Tavis" :locked true}}

{:entity Ticket
 :initial #tickets/Ticket {:title "Already here"}
 :events [#tickets/TicketDetailsChanged {:body "lies under the sea"}]
 :expected {:assignee nil :title "Already here"}}