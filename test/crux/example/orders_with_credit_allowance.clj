(ns crux.example.orders-with-credit-allowance
  (:require [crux.internal.keys :refer :all])
  (:require [slingshot.slingshot :refer (throw+)])
  (:require [clojure.walk :refer [macroexpand-all]]
            [clojure.pprint :refer (pprint)]
            [clojure.string :as str])
  (:require [crux.domain :refer :all])
  (:require [crux.reify :refer [reify-domain-spec!]]))

(defdomain orders
  (entity Customer [name credit-allowance outstanding-balance]
          (properties
           {available-credit (- credit-allowance outstanding-balance)
            has-credit? (pos? (available-credit entity))})

    (events
      [Created Create entity-fields
       (merge {:credit-allowance 1000
               :outstanding-balance 0}
              (filter second event))]

      [BalancePaid PayBalance [amount]
       (update-in [:outstanding-balance] - amount)]

      [OrderPlaced PlaceOrder [product unit-price qty]
       {:entity-constraints [has-credit?]
        :properties {order-total (* (:qty event) (:unit-price event))}}
       (update-in [:outstanding-balance] + (order-total event))])))

(defn build-reified-test-domain-spec []
  (reify-domain-spec! orders))
