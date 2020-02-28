(* GRADE:  100% *)
(* Question 1.1 *)
let rec occurCheck (v: char) (tau: typExp) : bool =
  match tau with
  |TypVar char -> char == v
  |TypInt -> false
  |Arrow(t1,t2)-> (occurCheck v t1) || (occurCheck v t2)
  |Lst t-> occurCheck v t
;;

(* Question 1.2 *)
let rec substitute (tau1 : typExp) (v : char) (tau2 : typExp) : typExp =
  match tau2 with
  |TypInt -> TypInt
  |TypVar char -> if (char == v) then tau1 else TypVar char
  |Arrow (t1,t2) ->Arrow ((substitute tau1 v t1),(substitute tau1 v t2))
  |Lst t -> Lst (substitute tau1 v t) 
;;

(* Question 1.3 *)
let applySubst (sigma: substitution) (tau: typExp) : typExp =
  List.fold_right (fun (a, b) -> substitute b a) sigma tau
;;

(* Question 2 *)
let rec unify (tau1: typExp) (tau2:typExp) : substitution = 
  match tau1 with
  |TypInt ->
      (match tau2 with
       |TypInt -> []
       |TypVar x -> [x, TypInt]
       |_ -> failwith "Not unifiable"
      )
  |TypVar x ->
      (match tau2 with
       |TypInt -> [x, TypInt]
       |TypVar x2 -> if (x=x2) then [] else [x2, TypVar x] 
       |Arrow(x2,y2) ->
           if (occurCheck x tau2) then failwith "Not unifiable"
           else [x, Arrow (x2,y2)]
       |Lst _ ->
           if (occurCheck x tau2) then failwith "Not unifiable"
           else [x, tau2] 
      )
  |Arrow (x,y) ->
      (match tau2 with
       |TypInt -> failwith "Not unifiable"
       |TypVar x2 ->
           if (occurCheck x2 tau1) then failwith "Not unifiable" 
           else [x2, Arrow (x,y)]
       |Arrow(x2,y2) ->
           let subs1:substitution = (unify x x2) in
           (unify (applySubst subs1 y) (applySubst subs1 y2)) @ subs1
       |Lst _ -> failwith "Not unifiable" 
      )
  |Lst x ->
      ( match tau2 with
        |TypInt -> failwith "Not unifiable"
        |TypVar x2 ->
            if (occurCheck x2 tau1) then failwith "Not unifiable"
            else [x2, tau1] 
        |Arrow (_) -> failwith "Not unifiable"
        |Lst x2 -> unify x x2 
      )
;;
