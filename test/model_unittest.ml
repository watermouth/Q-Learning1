let counter = ref 0;;
let validate group_id x = 
  counter := !counter + 1;
  let result = match x with
    | true -> "OK"
    | false-> "NG"
  in Printf.printf "%d,%s:%s\n" !counter group_id result;; 

let v1 = (validate "transition_test");;
v1 ((Model.transition 1 2 3) = 3);;
v1 ((Model.transition 3 2 3) = 4);;

let v2 = (validate "contribution_test");;
v2 ((Model.contribution 1 2 3 10.0 9.0) = (-. 17.0));; 
v2 ((Model.contribution 2 2 3 10.0 9.0) = (-. 7.0));; 
v2 ((Model.contribution 2 2 3 0.0 9.0) = (-. 27.0));; 

