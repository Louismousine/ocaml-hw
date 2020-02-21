(* GRADE:  100% *)
(* Q1 Polynomials TODO: Implement the following four functions *)



let multiplyPolyByTerm (Term(c,e), Poly p)= 

  let rec helper(Term(c,e), Poly p)= 
    let x = (c,e) in
    match x with 
    | (coefficient,deg) -> match p with
      |(a::af) ->if coefficient=0. then (0.,0)::[] else (fst a *.coefficient,snd a + deg)::helper(Term(c,e),Poly af)
      |[]->[]
  in let x = helper(Term(c,e), Poly p)
  in Poly x ;;

let addTermToPoly (Term(c,e), Poly p) =
  let rec helper(Term(c,e), Poly p, isAdded)= 
    let x = (c,e) in
    match x with 
    | (coefficient,deg) -> match p with 
      |(a::af) ->if deg=snd a then (fst a+.coefficient,deg)::helper(Term(c,e),Poly af, true)
          else if (deg>snd a && not isAdded) then (coefficient,deg)::a::helper(Term(c,e),Poly af, true)
          else a::helper(Term(c,e),Poly af, isAdded)
      |[]->if (not isAdded && coefficient != 0.) then (coefficient,deg)::[] else []
  in let x = helper(Term(c,e), Poly p, false)
  in Poly x ;; 

let rec addPolys (Poly p1, Poly p2) =
  match p1 with
  | (a::af) -> let x = Term(fst a, snd a)
      in addPolys(Poly af,addTermToPoly(x,Poly p2))
  |[]->Poly p2
;;

let rec multPolys (Poly p1, Poly p2) =
  match p1,p2 with
  | (a::af,s::ss) -> if(fst a=0.) then Poly[(0.,0)] else 
        let x = Term(fst a, snd a)
        in addPolys(multiplyPolyByTerm(x, Poly p2), multPolys(Poly af, Poly p2)) 
  | ([],[]) -> Poly []
  | ([], p2) -> Poly []
  | (p1, []) -> Poly []
;;

(* Q2 References TODO: implement the `insert` function *)

let rec insert comp (item: int) (list: rlist) =
  match !list with
  | None -> list := Some {data=item; next= ref None} 
  |Some{data = d; next = n} -> 
      if comp (item,d)
      then (list := Some {data = item; next = ref (Some {data=d;next=n})})
      else insert comp item n 
;;

