(ns crux.example.orders-with-credit-allowance
  (:require [crux.internal.keys :refer :all])
  (:require [slingshot.slingshot :refer (throw+)])
  (:require [clojure.walk :refer [macroexpand-all]]
            [clojure.pprint :refer (pprint)]
            [clojure.string :as str])
  (:require [crux.domain :refer :all])
  (:require [crux.reify :refer [reify-domain-spec!]]))

#_(pprint (macroexpand-all '(entity {} Blah [uno dos]
                        (events 
                         [Created Create entity-fields
                          (merge {credit-allowance 1000
                                  outstanding-balance 0
                                  price-table {:prod1 100
                                               :prod2 200}}
                                 (filter second event))])))) 

(defdomain orders
    (entity Product [oid name])
  
    (entity Customer [name credit-allowance outstanding-balance price-table]
            (properties
             available-credit (- credit-allowance outstanding-balance)
             has-credit? (pos? (available-credit entity))
             )
      
      (events
       
       [Created Create entity-fields
        (merge {credit-allowance 1000
                outstanding-balance 0
                price-table {:prod1 100
                             :prod2 200}}
               (filter second event))]
  
       [BalancePaid PayBalance [amount]
        (update-in [:outstanding-balance] - amount)]
  
       [OrderPlaced PlaceOrder [product qty]
        {:entity-constraints [has-credit?]
         :properties {order-total (* (:qty event)
                                     100
                                     ;(get-in entity [:price-table product])
                                     )}
         :validations [(check (contains? (get-in entity [:price-table product])))
                       ;; (check (< (prop event :order-total)
                       ;;           (available-credit entity)))
                       ]}
        (update-in [:outstanding-balance] + 100
                   ;(order-total event)
                   )]
       
       )))
