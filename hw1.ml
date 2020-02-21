(* GRADE:  100% *)
(* Q1 TODO: Correct these tests for the double function. *)
let double_tests = [
  (0, 0);
  (1, 2);
  (3, 6); 
]

(* Q1 TODO: Correct this implementation so that it compiles and returns
         the correct answers.
*)
let rec double n = match n with
  | 0 -> 0
  | n -> 2 + double (n - 1)



(* Q1 TODO: Write your own tests for the fact function.
         See the provided tests for double, above, for how to write test cases.
         Remember that you should NOT test cases for n < 0.
*)
let fact_tests = [
  (1,1.0);
  (0,1.0);
  (2,2.0);
]

(* Q1 TODO: Correct this implementation so that it compiles and returns
         the correct answers.
*)
let rec fact (n: int): float = match n with
  | 0 -> 1.0
  | _ -> float_of_int n *. fact (n - 1)

(* Q2 TODO: Write your own tests for the mysqrt function.
         You should NOT test cases for n < 0.
*)
let mysqrt_tests = [
  (1.0,1.0);
  (4.0,2.0);
  (0.0,0.0);
]

(* Q2 TODO: Implement mysqrt. *)
let mysqrt (x:float) =
  let rec find(x,g) =
    if close(x,g*.g) then g
    else find (x, (g +. x/.g)/.2.0)
  in find(x, 1.0)

(* Q3 TODO: Write your own tests for the cube_root function.
            You should NOT test cases for n < 0.
*)
let cube_root_tests = [
  (1.0,1.0);
  (8.0,2.0);
  (0.0,0.0);
]

(* Q3 TODO: Implement cube_root. *)
let cube_root (x:float) = 
  let rec findd(x,g) =
    if close(x,g*.g*.g) then g
    else findd (x, (2.0*.g +. x/.(g*.g))/.3.0)
  in findd(x, 1.0)
(* Q4 TODO: Write your own tests for the fast_exp function.
            You should NOT test cases for negative bases or powers.
*)
let fast_exp_tests = [ 
  ((0,0),0);
  ((1,1),1);
  ((0,1),0);
  ((1,0),1);
]

(* Q4 TODO: Implement tail recursive helper fast_exp_aux. *)
let rec fast_exp_aux (base, power, acc) =
  if power = 1 then acc
  else if power = 0 then 1
  else
  if (odd power) then fast_exp_aux(base, power-1,base*acc)
  else fast_exp_aux(base, power/2, acc*acc)
              

(* Q4 TODO: Implement fast_exp using fast_exp_aux. *)
let fast_exp (base, power) =
  if base = 0 then 0
  else if power = 1 then base
  else if (odd power) then fast_exp_aux(base, power, base)
  else fast_exp_aux(base, power, base)
                                         

                           


