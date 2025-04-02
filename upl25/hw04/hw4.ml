let rec interp (ast: Ast.expr) : Value.t =
  match ast with
    | Num n -> NumV n
    | Sub (e1, e2) -> 
      (match (interp e1, interp e2) with
        | NumV n1, NumV n2 -> NumV (n1 - n2))
    | Add (e1, e2) -> 
      (match (interp e1, interp e2) with
        | NumV n1, NumV n2 -> NumV (n1 + n2))
    
