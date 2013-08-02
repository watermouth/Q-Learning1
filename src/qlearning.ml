(* open Model;; *)

let create_q_table ~(state_size:int) ~(action_size:int) ~(initial_value:float)
  = Array.init state_size 
    (fun i -> (Array.make action_size initial_value))

let decide_with_q_table ~(q_table:(float array array))
  ~(price_sell:float) ~(price_buy:float) ~(inventory:int) ~(demand:int)
  =
  let index = ref 0 in
  let max_value = ref (-1.) in
  Array.iteri (fun i x -> if x > !max_value 
               then (max_value := x; index := i) else () ) q_table.(inventory); 
  !index

let batch_simulation_q_learning
  ?(epsilon=0.1) ?(gamma=1.0) ?(alpha=0.9)
  ~(max_purchase:int)
  ~(batch_size:int) ~(initial_inventory:int)
  ~(demands:int array) ~(price_sell:float array) ~(price_buy:float array)
  ~(q_table:float array array)
  =
  (* Input check*)
  if (Array.length demands) <> batch_size || (Array.length price_sell) <> batch_size
     || (Array.length price_buy) <> batch_size
  then
    raise (Invalid_argument "input array's sizes and batch_size don't match")
  else
    (); 
  let decision_fun = decide_with_q_table ~q_table in 
  (* loop *)
  let inventory = Array.make batch_size initial_inventory in
  let contribution = Array.make batch_size 0.0 in
  for i=1 to (batch_size-1) do
(*
  Printf.printf "here ? ;;\n";
*)
    let action = if ((Random.float 1.0) < epsilon)
      then (Random.int max_purchase)
      else decide_with_q_table q_table price_sell.(i-1)(*dummy*) price_buy.(i-1)(*dummy*) inventory.(i-1) demands.(i-1)(*dummy*)
    in
    Printf.printf "inventory:%d,\taction:%d\n" inventory.(i-1) action;
(*
*)
    let (temp1, temp2) =
      (simulate2 inventory.(i-1) demands.(i-1) price_sell.(i-1)
       price_buy.(i-1) action) in
    inventory.(i) <- temp1;
    contribution.(i-1) <- temp2; 
(*
    Printf.printf "%d inventory %d, contribution %f\n" i temp1 temp2;
*)
    (* q learning *)
    let action_for_next_state = 
      decide_with_q_table q_table price_sell.(i)(*dummy*) price_buy.(i)(*dummy*) inventory.(i) demands.(i)(*dummy*)
    in
    let q = temp2 +. gamma *. q_table.(inventory.(i)).(action_for_next_state) in 
    q_table.(inventory.(i-1)).(action) <- 
      (1.0 -. alpha) *. q_table.(inventory.(i-1)).(action) +.
      (alpha *. q);
    ()
  done;
  (inventory, contribution)
