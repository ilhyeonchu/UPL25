let%test _ = Fstore.add "add" ["a"; "b"] (Add ((Id "a"), (Id "b"))) []  = ["add", (["a"; "b"], (Add ((Id "a"), (Id "b"))))]
let%test _ = Fstore.find "add" ["add", (["a"; "b"], (Add ((Id "a"), (Id "b"))))]  = (["a"; "b"], (Add ((Id "a"), (Id "b"))))
let%test _ = try
  let _ = Fstore.find "add" [] in
  false
  with
  |Failure msg -> msg = "Undefined function: add"
  |_ -> false
let%test _ = Hw6.interp_expr [("add", (["a"; "b"], (Add ((Id "a"), (Id "b")))))] [] (Call ("add", [Num 2;Num 3])) = NumV 5
let%test _ = Hw6.interp_def (Ast.FunDef ("add", ["a"; "b"], (Add ((Id "a"), (Id "b"))))) [] = [("add", (["a"; "b"], (Add ((Id "a"), (Id "b")))))]
let%test _ = Hw6.interp_prog (Ast.Prog ([], (Ast.Add (Num 3, Num 1)))) = NumV 4



let%test _ = Fstore.add "f" ["x"] (Id "x") [] = [("f", (["x"], Id "x"))]

let%test _ = Fstore.add "f" ["a"; "b"] (Add (Id "a", Id "b")) [("g", (["y"], Id "y"))]
              = [("f", (["a"; "b"], Add (Id "a", Id "b"))); ("g", (["y"], Id "y"))]

let%test _ = Fstore.add "f" ["x"] (Num 42) [("f", (["x"], Num 0))]
              = [("f", (["x"], Num 42))]

let%test _ = Fstore.find "f" [("f", (["x"], Id "x"))] = (["x"], Id "x")

let%test _ = Fstore.find "g" [("f", (["x"], Id "x")); ("g", (["y"], Num 1))]
              = (["y"], Num 1)

let%test _ = try
    let _ = Fstore.find "efsdv" [] in false
  with Failure msg -> msg = "Undefined function: efsdv"
  | _ -> false

let%test _ = Hw6.interp_expr [("add", (["a"; "b"], Add (Id "a", Id "b")))] [] (Call ("add", [Num 5; Num 4])) = NumV 9

let%test _ = Hw6.interp_expr [("f", (["x"], Sub (Id "x", Num 1)))] [] (Call ("f", [Num 10])) = NumV 9

let%test _ = try
    let _ = Hw6.interp_expr [] [] (Call ("cewiug", [Num 1])) in false
  with Failure msg -> msg = "Undefined function: cewiug"
  | _ -> false


let%test _ = Hw6.interp_def (FunDef ("sub", ["a"; "b"], Sub (Id "a", Id "b"))) []
              = [("sub", (["a"; "b"], Sub (Id "a", Id "b")))]

let%test _ = Hw6.interp_def (FunDef ("efe", ["x"], Num 10)) [("ddd", (["x"], Id "x"))]
              = [("efe", (["x"], Num 10)); ("ddd", (["x"], Id "x"))]

let%test _ = Hw6.interp_def (FunDef ("f", ["x"], Sub (Id "x", Num 1))) [("f", (["x"], Num 0))]
              = [("f", (["x"], Sub (Id "x", Num 1)))]


let%test _ = Hw6.interp_prog (Prog ([], Sub (Num 5, Num 2))) = NumV 3

let%test _ = Hw6.interp_prog (
    Prog ([FunDef ("fhe", ["x"], Add (Id "x", Id "x"))],
          Call ("fhe", [Num 5]))) = NumV 10

let%test _ = Hw6.interp_prog (
    Prog ([FunDef ("f", ["x"], Add (Id "x", Num 1));
           FunDef ("g", ["y"], Call ("f", [Id "y"]))],
          Call ("g", [Num 9]))) = NumV 10
