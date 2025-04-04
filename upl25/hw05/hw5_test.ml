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
