(* GRADE:  100% *)
(* Q1a TODO: Write your own tests for the pairlists function.
         You should NOT test lists of different lengths.
*)
let pairlists_tests = [
  (([1;2;3],[4;2;3]),[(1,4);(2,2);(3,3)]);
  (([],[]),[]);
  
]

(* Q1a TODO: Implement pairlists. *)
let rec pairlists (l1, l2) = 
  match l1, l2 with
  | ([], []) -> []
  | (x :: l1, y :: l2) -> (x,y)::pairlists(l1,l2)
;;

(* Q1b TODO: Write your own tests for the w_mean function.
         You should NOT test lists of different lengths.
*)
let w_mean_tests = [
  (([1.0], [2.0]),2.0);
  (([1.0],[0.0]),0.0); 
  (([1.0;1.0], [1.0;1.0]),1.0); 
  (([1.0;1.0],[-1.0;-1.0]),-1.0);
]

(* Q1b TODO: Implement w_mean. *)
let w_mean weights data =
  sumlist(List.map(fun (x,y) -> x*.y) (pairlists(weights, data))) /.sumlist(weights)
;;

(* Q2 TODO: Write your own tests for the memberof function. *)
let memberof_tests = [
  ((1, [1]), true);
  ((0,[1]), false);
  ((0,[]), false);
  
]

(* Q2 TODO: Implement memberof. *)
let rec memberof (x, xs) =
  match (x, xs) with
  | (x,[]) -> false
  | (x, y::xs) -> if (x = y) then true else memberof(x,xs)
;;

(* Q2 TODO: Write your own tests for the remove function. *)
let remove_tests = [
  ((0,[0]),[]);
  ((0,[1]),[1]);
  ((0,[0;0]),[]);
  ((0,[]),[]);
]

(* Q2 TODO: Implement remove. *)
let rec remove (item, lst) =
  match lst with
  | [] -> []
  | x:: xs -> if (item = x) then remove (item, xs) else x:: remove(item,xs)
;;

(* Q3 TODO: Write your own tests for the find_max function. *)
let find_max_tests = [
  ([1;2],2);
  ([-1],-1); 
  ([-1;-5],-1)
]

(* Q3 TODO: Implement find_max. *)
let find_max l = 
  let rec helper(acc, l) =
    match l with
    | [] -> acc
    | x::xs -> if (x>=acc) then helper(x,xs) else helper(acc,xs) 
  in 
  let x:: xs = l in helper(x,xs)
;;

(* Q4 TODO: Write your own tests for the selsort function. *)
let selsort_tests = [
  ([1;2],[2;1]);
  ([],[]);
  ([-1;2],[2;-1]);
  ([-1;0],[0;-1]);
  ([1;5;-2],[5;1;-2]);
  ([1;2;3],[3;2;1])
]

(* Q4 TODO: Implement selsort. *)
let rec selsort l =
  match l with
  | [] -> []
  | x::xs -> let max = (find_max l) in
      max::(selsort (remove(max, l)))
;;

