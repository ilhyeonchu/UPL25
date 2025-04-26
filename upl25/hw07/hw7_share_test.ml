(* add *)
let%test _ = Store.add "owo" (ClosureV ("x", (Add (Id "x", Num 1)), [])) [] = [("owo", (ClosureV ("x", (Add (Id "x", Num 1)), [])))]

let%test _ = Store.add "x" (NumV 99) [] = [("x", NumV 99)]

let%test _ = Store.add "qwer" (ClosureV ("x", (Sub (Id "x", Num 1)), [])) [("owo", (ClosureV ("x", (Add (Id "x", Num 1)), [])))] = 
  [("qwer", (ClosureV ("x", (Sub (Id "x", Num 1)), []))); ("owo", (ClosureV ("x", (Add (Id "x", Num 1)), [])))]

(* find *)
let%test _ = Store.find "owo" [("owo", (ClosureV ("x", (Add (Id "x", Num 1)), [])))] = ClosureV ("x", (Add (Id "x", Num 1)), [])

let%test _ = Store.find "x" [("x", NumV 1)] = NumV 1

let%test _ = Store.find "f" [("f", ClosureV ("x", (Id "x"), []))] = ClosureV ("x", Id "x", [])

(* interp *)
let%test _ =
try
  let _ = Hw7.interp (ParserMain.parse "1 1") [] in false
with
|Failure msg -> msg = "Not a function: 1" 
|_ -> false 

let%test _ = Hw7.interp (ParserMain.parse "let x = 1 in x + 2") [] = NumV 3

let%test "fe" = Hw7.interp (ParserMain.parse "let x = 5 in let addxy = (fun n1 n2 -> n1 + n2) in let y = 3 in addxy x y") [] = NumV 8

(* parameter 가 1개면 안되는듯? *)
let%test "asd" = Hw7.interp (ParserMain.parse "let x = 5 in let add2 = (fun n1 -> n1 + 2) in let sub3 = (fun n2 -> n2 - 3) in add2 (sub3 x) ") [] = NumV 4
