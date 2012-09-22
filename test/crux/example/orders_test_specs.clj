{:entity Customer
 :name "Customer test 1"
 :events [
          #orders/CustomerCreated {:name "Customer 1"}
          #orders/CustomerOrderPlaced {:product :coffee
                                       :unit-price 100
                                       :qty 2}
          #orders/CustomerOrderPlaced {:product :a-book
                                       :unit-price 50
                                       :qty 1}]
 :expected {:name "Customer 1" :outstanding-balance 250}}
