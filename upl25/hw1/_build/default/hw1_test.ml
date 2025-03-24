(* Test Code *)

(* success *)
let%test _ = Hw1.calc '+' 1 1 = 2

let%test _ = 
  try let _ = Hw1.calc '%' 1 1 in false
  with
    |Failure msg -> msg = "Unsupported operation"
    |_ -> false

let%test _ = 
  try let _ = Hw1.calc '/' 1 0 in false
  with
    |Failure msg -> msg = "Divide-by-Zero"
    |_ -> false

