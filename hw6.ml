(* GRADE:  100% *)
(* Question 1 *)

let makeProtectedAccount ((openingBalance: int), (password: string)) =
  let balance = ref openingBalance in
  let pwd = ref password in
  let isAccClosed = ref false in
  fun ((password: string),(t: transaction)) ->
    match t with
    | Withdraw(m) ->  if (!balance >= m && !pwd = password && not !isAccClosed)
        then
          ((balance := !balance - m);
           (Printf.printf "The new balance is: %i." !balance))
        else if (!pwd = password) then
          print_string "Insufficient funds."
        else if (!isAccClosed) then print_string "Account closed."
        else
          (Printf.printf "Incorrect password.")
    | Deposit(m) -> if (!pwd = password && not !isAccClosed) then
          ((balance := !balance + m);
           (Printf.printf "The new balance is: %i." !balance)) 
        else if(!isAccClosed) then print_string "Account closed." else(Printf.printf "Incorrect password.")
    | CheckBalance -> if(!pwd = password && not !isAccClosed) then (Printf.printf "The balance is: %i." !balance) else if(!isAccClosed) then print_string "Account closed." else
          (Printf.printf "Incorrect password.")
    | Close -> if (!pwd = password) then (isAccClosed:=true; print_string "Account successfully closed.") else (Printf.printf "Incorrect password.") 
    | ChangePassword (newPassword: string)-> if (!pwd = password) then (pwd:= newPassword; print_string "Password changed.";)
        else (Printf.printf "Incorrect password.") 
;;
