open Model
open Qlearning
open Batteries
open CalendarLib
(* sample execution *)

(* Random seed settings *)
let seed = (CalendarLib.Time.to_seconds (CalendarLib.Time.now ()));;
Random.init seed;;
(*
*)

let time_steps = (int_of_string Sys.argv.(1));;
let gamma = (float_of_string Sys.argv.(2));;
let epsilon = (float_of_string Sys.argv.(3));;
let alpha_max = (float_of_string Sys.argv.(4));;
let alpha n () = 
  (n := !n +. 1.;
  alpha_max /. !n);;
let alpha = alpha (ref 0.);;
let alpha () = alpha_max;;
let do_print = (bool_of_string Sys.argv.(5));;
let maximum_inventory = 1000;;
let maximum_purchase = 9;;
let initial_inventory= 0;;
let price_sells = (Array.create time_steps 200.0);;
let price_buys  = (Array.create time_steps 100.0);;
let demand ~(max_value:int) () = (Random.int max_value);; 
let demands_sample_1 = (Array.init time_steps (fun i -> (demand ~max_value:maximum_purchase ()))) ;;

(* decision rule 1 *)
let decision_rule_1 = constant_decision ~constant:(maximum_purchase / 2 );;
let (inventories, contributions) =
  batch_simulation do_print time_steps initial_inventory demands_sample_1 price_sells price_buys decision_rule_1;;
Printf.printf "\n";;

(* decision rule 2 *)
let cheat = (Array.init time_steps (fun i -> if i < ((Array.length demands_sample_1) - 1)
  then demands_sample_1.(i+1) else 0));;
let decision_rule_2 = predefined_decision ~decisions:cheat ()
let (inventories2, contributions2) =
  batch_simulation do_print time_steps initial_inventory demands_sample_1 price_sells price_buys decision_rule_2;;
Printf.printf "\n";;

(* decision rule 3 *)
let contributions3 = Array.init 10 (fun i -> 0.0);;
let q_table = create_q_table ~state_size:maximum_inventory ~action_size:maximum_purchase
  ~init_func:(fun i -> -10.0);;
q_table.(0) <- Array.init maximum_purchase (fun i -> -1. *. (float i));;
(*
  ~init_func:(fun i -> if i > (maximum_purchase / 2) then 0.0 else 0.01 *. (float i));; 
*)
let (inventories3, contributions3) =
  batch_simulation_q_learning ~max_purchase:maximum_purchase
  ~do_print ~epsilon ~gamma ~alpha ~batch_size:time_steps 
  ~initial_inventory ~demands:demands_sample_1
  ~price_sell:price_sells
  ~price_buy:price_buys
  ~q_table ;;
Printf.printf "\n";;

let value1_at_0 = reduce_contributions gamma (Array.sub contributions (time_steps / 2) (time_steps / 2));; 
let value2_at_0 = reduce_contributions gamma (Array.sub contributions2 (time_steps / 2) (time_steps / 2));; 
let value3_at_0 = reduce_contributions gamma (Array.sub contributions3 (time_steps / 2) (time_steps / 2));; 
(*
let value2_at_0 = reduce_contributions gamma contributions2;; 
let value3_at_0 = reduce_contributions gamma contributions3;; 
*)
Printf.printf "\n%f, %f, %f\n" value1_at_0 value2_at_0 value3_at_0;;

(* *)
(*
 let alpha () = alpha_max;;
 time ./unittest_main 30000 1 0.2 0.75 true
 time ./unittest_main 30000 0.9 0.2 0.7 true
*)
