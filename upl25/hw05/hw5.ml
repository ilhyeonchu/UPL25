let rec interp (ast: Ast.expr) (mem: Store.t) : Store.value =
  match ast with
    | Num n -> NumV n 
    | Id st -> Store.find st mem 
    | Add (e1, e2) -> 
      (match (interp e1, interp e2) with
        | NumV n1, NumV n2 -> NumV (n1 + n2))
    | Sub (e1, e2) ->
      (match (interp e1, interp e2) with
        | NumV n1, NumV n2 -> NumV (n1 - n2))
    | LetIn (st, e1, e2) ->

