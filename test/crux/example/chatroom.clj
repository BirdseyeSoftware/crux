(ns crux.example.tickets
  (:require [clojure.pprint :refer (pprint)])
  (:require [crux.domain :refer :all])
  (:require [crux.reify :refer [reify-domain-records!]]))

;;; This example is a work in progress and is not being fully
;;; completed and tested.


(defdomain chatroom-domain
  (entity User [id nick first-name last-name banned?]

    (events
      [Create Created entity-fields
       (merge event)]

      [NickSet SetNick [nick]
       (assoc :nick nick)]))

  (entity Room [id name ops users max-users]
    (properties
     user-count (count (:users entity))
     op-count (count (:ops entity))
     accepting-users? (< (user-count entity)
                         ;; (prop entity :user-count)
                         (:max-users entity)))

    (events

      [Create Created entity-fields
       (merge {:users #{}
               :ops #{}
               :max-users 100}
              event)]

      [Entered Enter [user-id]
       {:constraints [accepting-users?]}
       (update-in [:users] conj user-id)]

      [Left Leave [user-id]
       (update-in [:users] disj user-id)]

      [OpAdded AddOp [user-id]
       (update-in [:ops] conj user-id)]

      [OpRemoved RemoveOp [user-id]
       (update-in [:ops] disj user-id)]

      )))
