{:entity Customer
 :name "Simple test of :outstanding-balance state machine"
 :events
 [
  #orders/CustomerCreated {:name "Customer 1"}
  #orders/CustomerOrderPlaced {:product :coffee :unit-price 100 :quantity 2}
  #orders/CustomerOrderPlaced {:product :book1 :unit-price 50 :quantity 1}
  #orders/CustomerBalancePaid {:amount 90}
  #orders/CustomerOrderPlaced {:product :book2 :unit-price 15 :quantity 2}
  #orders/CustomerBalancePaid {:amount 2}
  #orders/CustomerOrderPlaced {:product :book2 :unit-price 15 :quantity 2}
  ]
 :expected {:name "Customer 1" :outstanding-balance 218}}
