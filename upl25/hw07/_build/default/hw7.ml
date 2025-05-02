let rec interp (ast: Ast.expr) (mem: Store.t) : Store.value =
  match ast with
    | Num n -> NumV n 
    | Id st -> Store.find st mem 
    | Add (e1, e2) -> 
      (match (interp e1 mem, interp e2 mem) with
        | NumV n1, NumV n2 -> NumV (n1 + n2)
        | _ -> failwith (Format.asprintf "Not a number: %a + %a" Ast.pp e1 Ast.pp e2))
    | Sub (e1, e2) ->
      (match (interp e1 mem, interp e2 mem) with
        | NumV n1, NumV n2 -> NumV (n1 - n2)
        | _ -> failwith (Format.asprintf "Not a number: %a - %a" Ast.pp e1 Ast.pp e2))
    | LetIn (st, e1, e2) ->
      interp e2 (Store.add st (interp e1 mem) mem)
    | App (e1, e2) ->
      (match (interp e1 mem, interp e2 mem) with
        | ClosureV (st, e1, mem'), NumV n2 -> interp e1 (Store.add st (NumV n2) mem')  (* C1, C2 는 안되는건가? C2가 파라메터 없다면? 무조건 있어야해서 unit 같은게 있다고 하신거 같긴한데 *)
        | _, _ -> failwith (Format.asprintf "Not a function: %a" Ast.pp e1))
    | Lambda (st, e2) -> 
        ClosureV (st, e2, mem)

