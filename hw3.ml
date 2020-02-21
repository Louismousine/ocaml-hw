(* GRADE:  100% *)
(* Question 1. *)

let common_tests = [
  (([1;2],[]),[]);
  (([],[]),[]);
  (([1;1],[1]),[1]);
]

let common (l1, l2) =
  let rec helpme (l1, l2, acc) = 
    match l1 with
    | [] -> acc
    | x::ls -> if (List.mem x l2) then (helpme(ls, remove(x,l2), acc@[x])) else (helpme(ls, l2, acc))
  in helpme (l1, l2, [])
;;

(* Question 2. Mergesort requires that you use recursion.  Using List.sort or
some other sort defeats the whole purpose.  This question is for the
implementation of split.*)

let split_tests = [
  ([1],([1],[]));
  ([1;1],([1],[1]));
]

let split l =
  let rec helper(l, l1, l2)=
    match l with
    | [] -> l2, l1
    | x::ls -> helper(ls, l2, l1@[x])
  in helper(l, [], [])
;;

(* Question 3 Here you implement merge. *)

let merge_tests = [
  (([],[]),[]);
  (([1;2],[3]),[1;2;3]);
  (([1;1],[]),[1;1])
]

let merge (l1,l2)=
  let rec helper(l1,l2,acc)=
    match l1,l2 with
    |([],[])->acc
    |([],x::xs)->helper(l1,xs,acc@[x])
    |(x::xs,[])->helper(xs,l2,acc@[x])
    |(x::xs,y::ys)->if (x<y) then helper(xs, l2,acc@[x]) else helper(l1, ys,acc@[y])
  in helper (l1,l2,[])
;;

(* Question 4 Finally you combine split and merge and use them to implement mergesort. *)

let mergesort_tests = [
  (([]),[]);
  (([1;2]),[1;2]);
  ([1],[1])

]

let rec mergesort l =
  match l with
  |[]->[]
  |x::[]->x::[]
  |x::xs-> let (l1,l2) = split(l) in let x1 = mergesort(l1);
      in let x2 = mergesort(l2);
      in merge(x1,x2)
;;

