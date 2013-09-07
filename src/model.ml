(*
#use "src/qlearning.ml"
*)
open Batteries
(* models *)
let transition ~(inventory:int) ~(demand:int) ~(purchase:int) =
  inventory - (min inventory demand) + purchase

let contribution ~(inventory:int) ~(demand:int) ~(purchase:int) 
  ~(price_sell:float) ~(price_buy:float) =
  price_sell *. (float (min inventory demand))
  -. price_buy *. (float purchase)

(*  ~(decision_fun:(float -> float -> int -> int -> int)) =*)
let simulate ~(inventory:int) ~(demand:int) ~(price_sell:float) ~(price_buy:float)
  ~(decision_fun:(price_sell:float -> price_buy:float -> inventory:int -> demand:int -> int)) =
  let purchase = (decision_fun price_sell price_buy inventory demand) in
  let inventory_value = (transition inventory demand purchase) in
  let contribution_value =
    (contribution inventory demand purchase price_sell price_buy) in
  (inventory_value, contribution_value)

let simulate2 ~(inventory:int) ~(demand:int) ~(price_sell:float) ~(price_buy:float)
  ~(purchase:int)
  = 
  let inventory_value = (transition inventory demand purchase) in
  let contribution_value =
    (contribution inventory demand purchase price_sell price_buy) in
  (inventory_value, contribution_value)

let batch_simulation
  ~(do_print:bool)
  ~(batch_size:int) ~(initial_inventory:int)
  ~(demands: int array) ~(price_sell:float array) ~(price_buy:float array)
  ~(decision_fun:(price_sell:float -> price_buy:float -> inventory:int -> demand:int -> int))
  =
  (* Input check*)
  if (Array.length demands) <> batch_size || (Array.length price_sell) <> batch_size
     || (Array.length price_buy) <> batch_size
  then
    raise (Invalid_argument "input array's sizes and batch_size don't match")
  else
    (); 
  (* loop *)
  let inventory = Array.make batch_size initial_inventory in
  let contribution = Array.make batch_size 0.0 in
  for i=1 to (batch_size-1) do
    let action = decision_fun price_sell.(i-1) price_buy.(i-1) inventory.(i-1) demands.(i-1) in 
    let (temp1, temp2) =
      (simulate2 inventory.(i-1) demands.(i-1) price_sell.(i-1)
       price_buy.(i-1) action) in
    inventory.(i) <- temp1;
    contribution.(i-1) <- temp2; 
    if do_print then
      Printf.printf "%d inventory:%d\t, demand:%d\t, action:%d\t, contribution:\t%f\n" i inventory.(i-1) demands.(i-1) action temp2
    else ()
    ;
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
    (* Printf.printf "index = %d\n" !index; *)
    value 
  in f

let reduce_contributions ~(gamma:float) ~(contributions:float array) =
  let rec sub gamma contributions value =
    let size = (Array.length contributions) - 1 in 
    let value = value +. contributions.(0) in
    if size = 0 then value 
    else if size < 0 then 0.0
    else 
      let sub_contributions =
        (Array.map (fun x -> x *. gamma) (Array.sub contributions 1 size)) in
      sub gamma sub_contributions value
  in sub gamma contributions 0.0

