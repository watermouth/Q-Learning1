(* models *)
let transition ~(inventory:int) ~(demand:int) ~(purchase:int) =
  inventory - (min inventory demand) + purchase

let contribution ~(inventory:int) ~(demand:int) ~(purchase:int) 
  ~(price_sell:float) ~(price_buy:float) =
  price_sell *. (float (min inventory demand))
  -. price_buy *. (float purchase)

let demand ~(max_value:int) () = Random.int max_value 

(*  ~(decision_fun:(float -> float -> int -> int -> int)) =*)
let simulate ~(inventory:int) ~(demand:int) ~(price_sell:float) ~(price_buy:float)
  ~(decision_fun:(price_sell:float -> price_buy:float -> inventory:int -> demand:int -> int)) =
  let purchase = (decision_fun price_sell price_buy inventory demand) in
  let inventory_value = (transition inventory demand purchase) in
  let contribution_value =
    (contribution inventory demand purchase price_sell price_buy) in
  (inventory_value, contribution_value)

let batch_simulation ~(batch_size:int) ~(initial_inventory:int)
  ~(demands: int array) ~(price_sell:float array) ~(price_buy:float array)
  ~(decision_fun:(price_sell:float -> price_buy:float -> inventory:int -> demand:int -> int)) =
  (* Input check*)
  if (Array.length demands) <> batch_size || (Array.length price_sell) <> batch_size
     || (Array.length price_buy) <> batch_size
  then
    raise (Invalid_argument "input array's sizes and batch_size don't match")
  else
    (); 
  (* loop *)
  let inventory = Array.create batch_size initial_inventory in
  let contribution = Array.create batch_size 0.0 in
  for i=1 to (batch_size-1) do
    let (temp1, temp2) =
      (simulate inventory.(i-1) demands.(i-1) price_sell.(i-1)
       price_buy.(i-1) decision_fun) in
    inventory.(i) <- temp1;
    contribution.(i-1) <- temp2; 
    ()
  done;
  (inventory, contribution)

let constant_decision ~(constant:int) 
  ~(price_sell:float) ~(price_buy:float) ~(inventory:int) ~(demand:int) =
  constant

let predefined_decision ~(decisions:int array) () = 
  (*~(price_sell:float) ~(price_buy:float) ~(inventory:int) ~(demand:int) =*)
  let index = ref 0 in
  let f = fun ~(price_sell:float) ~(price_buy:float) ~(inventory:int) ~(demand:int) -> 
    let value = decisions.(!index) in
    index := !index + 1;
    Printf.printf "index = %d\n" !index;
    value 
  in f

(* sample execution *)
;;
Random.init 100;;
let decision_rule_1 = constant_decision ~constant:5 ;;
let time_steps = 10 ;;
let demands_sample_1 = (Array.init time_steps (fun i -> (demand ~max_value:10 ()))) ;;
let (inventories, contributions) =
  batch_simulation time_steps 10 demands_sample_1 (Array.create time_steps 101.0) (Array.create time_steps 100.0) decision_rule_1;;
let value1_at_0 = Array.reduce (fun i j -> i +. j) contributions;;

Random.init 100;;
let demands_sample_2 = (Array.init time_steps (fun i -> if i < ((Array.length demands_sample_1) - 1)
    then demands_sample_1.(i+1) else 0));;
let decision_rule_2 = predefined_decision ~decisions:demands_sample_2 ()
let (inventories2, contributions2) =
  batch_simulation time_steps 10 demands_sample_1 (Array.create time_steps 101.0) (Array.create time_steps 100.0) decision_rule_2;;
let value2_at_0 = Array.reduce (fun i j -> i +. j) contributions2;;

