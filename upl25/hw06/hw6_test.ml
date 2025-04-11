(* add *)
let%test _ = Store.add "owo" (NumV 939) [] = [("owo", NumV 939)]

(* find *)
let%test _ = 
  try
    let _ = Store.find "omo" [] in
    false
  with
  |Failure msg -> msg = "Free identifier: omo"
  |_ -> false

(* interp *)
let%test _ = Hw5.interp (ParserMain.parse "let oxo = 11 in oxo + 22") [] = NumV 33


let%test _ = Store.add "x" (NumV 99) [] = [("x",NumV 99)]
let%test _ = Store.add "x" (NumV 78) [("x", NumV 99)] = [("x", NumV 78)]
let%test _ = Store.add "x" (NumV 43) [("y", NumV 42); ("x", NumV 99)] = [("x", NumV 43); ("y", NumV 42)]

let%test _ = Store.find "x" [("x", NumV 99); ("y", NumV 42)] = NumV 99
let%test _ = Store.find "y" [("x", NumV 99); ("y", NumV 42)] = NumV 42
let%test _ = 
  try
    let _ = Store.find "z" [("x", NumV 43); ("y", NumV 99)] in 
    false
  with
    | Failure msg -> msg = "Free identifier: z"
    | _ -> false

let%test _ = Hw5.interp (ParserMain.parse "let x = 1 in x + 2") [] = NumV 3
let%test _ = Hw5.interp (ParserMain.parse "let x = 1 in let x = x + 2 in x + 3") [] = NumV 6
let%test _ = Hw5.interp (ParserMain.parse "let x = 1 in let y = x + 2 in x + y") [] = NumV 4
