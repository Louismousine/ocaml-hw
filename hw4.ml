(* GRADE:  100% *)
(* Question 1 *)

let mapTree_tests =
  [
    (((fun x -> x+1), Node(Empty, 0, (Node(Empty, 1, Empty)))), Node(Empty,1, (Node(Empty, 2, Empty))));
    (((fun x -> x+1), Node(Node(Empty, 1, Empty), 0, Empty))), Node(Node(Empty, 2, Empty), 1, (Empty)); 
    (((fun x -> x), Empty), Empty);
  ]

let rec mapTree (f, (t: 'a tree)) =
  match t with
  | Empty -> Empty
  | Node (l, m, r) -> Node ((mapTree (f,l)), f m, (mapTree (f,r)))
;;

(* Question 2. *)

let halfint_tests =
  [
    (((fun x -> x+.1.0), 9.0,-6.0,0.0001),-1.0000762939453125);
  ]

let rec halfint (f, (negValue : float), (posValue : float), (epsilon : float)) =
  let midPoint =(posValue +. negValue)/.2.0 in
  if (abs_float (f midPoint) < epsilon) then midPoint
  else if (f midPoint > 0.0) then halfint (f, midPoint, posValue, epsilon)
  else halfint (f, negValue, midPoint, epsilon)
;;

(* Question 3. *)

let newton_tests =
  [
    ((sin, 5.,0.0001,0.0001),9.42477);
  ]

let rec newton (f,  (guess:float), (epsilon:float), (dx:float)) =
  let close x y = abs_float (x -. y) < epsilon in
  (*let improve (guess:float) = raise NotImplemented in *)
  if close (f guess) 0.0 then guess
  else newton (f, guess -. f guess /. deriv(f,dx) guess, epsilon, dx)
;;

(* Question 4. *)

let indIntegral (f, (dx:float)) =
  fun x -> integral (f,0.0,x,dx)
;;

