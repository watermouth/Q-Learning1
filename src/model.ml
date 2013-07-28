(* models *)
let transition ~(inventory:int) ~(demand:int) ~(purchase:int) =
  inventory - (min inventory demand) + purchase

let contribution ~(inventory:int) ~(demand:int) ~(purchase:int) 
  ~(price_sell:float) ~(price_buy:float) =
  price_sell *. (float (min inventory demand))
  -. price_buy *. (float purchase)


