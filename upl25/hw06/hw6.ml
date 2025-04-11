(* len: int list -> int *)
let rec len (ls: int list) : int = 
  match ls with
    | [] -> 0
    | _ :: t -> 1 + (len t)

(* interp_def: Ast.fundef -> Fstore.t -> Fstore.t *)
let interp_def (ast: Ast.fundef) (memf: Fstore.t) : Fstore.t =
  Fstore.add ast memf 

(* interp_expr: Fstore.t -> Store.t -> Ast.expr -> Store.value *)
let rec interp_expr (memf: Fstore.t) (mem: Store.t) (ast: Ast.expr) : Store.value =
  match ast with
    | Num n -> NumV n 
    | Id st -> Store.find st mem 
    | Add (e1, e2) -> 
      (match (interp_expr memf mem e1, interp_expr memf mem e2) with
        | NumV n1, NumV n2 -> NumV (n1 + n2))
    | Sub (e1, e2) ->
      (match (interp_expr memf mem e1, interp_expr memf mem e2) with
        | NumV n1, NumV n2 -> NumV (n1 - n2))
    | LetIn (st, e1, e2) ->
      interp_expr memf (Store.add st (interp_expr memf mem e1) mem) e2
    | Call (st, exprl) ->
      let (stl, exprf) = Fstore.find st memf in 
        if (len exprl = len stl) then Store.add' stl exprl (* mem 에 stl 에 있는 문자들과 exprl에 있는 값을 묶어서 추가 *)
        match (stl, exprl) with
          | (stlh::stlt, exprh::exprt) -> Store.add stlh exprh mem
      match Fstore.find st memf with
        | (stl, exprf) -> 


