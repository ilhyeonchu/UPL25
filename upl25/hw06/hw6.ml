(* interp_def: Ast.fundef -> Fstore.t -> Fstore.t *)
let interp_def (ast: Ast.fundef) (memf: Fstore.t) : Fstore.t =
  match ast with
    | FunDef (st, stl, expr) -> Fstore.add st stl expr memf
    (* | _ -> memf *)

(* fmem: Ast.fundef list -> Fstore.t -> Fstore.t *)
let rec fmem (ast: Ast.fundef list) (memf: Fstore.t) : Fstore.t =
  match ast with
    | [] -> memf
    | h :: t -> fmem t (interp_def h memf) 

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
      (* stl = 인자 리스트, funexpr = 함수 내용?  *)
      let (stl, funexpr) = Fstore.find st memf in 
        if (List.length exprl = List.length stl) then 
        (* mem' 에 mem에다가  stl 에 있는 문자들과 exprl에 있는 값을 묶어서 추가 *)
        (* funmem: Fstore.t -> Store.t -> string list -> Ast.exper -> Store.t *)
          let rec funmem (memf: Fstore.t) (mem: Store.t) (stl: string list) (ast: Ast.expr list) : Store.t =
            match stl, ast with
              | (hs::ts), (he::te) -> let mem' = Store.add hs (interp_expr memf mem he) mem in
                                        funmem memf mem' ts te 
              | _, _ -> mem
          in
          let mem' = funmem memf mem stl exprl in
            interp_expr memf mem' funexpr
        else failwith "Unmatched number of arguments" 

(* interp_prog: Ast.prog -> Store.value *)
let interp_prog (ast: Ast.prog) : Store.value =
  (* prog: (decllist*expr) | expr *)
  match ast with
    | Prog (defl, expr) -> 
                let memf = fmem defl Fstore.empty in
                  interp_expr memf Store.empty expr
    (* | Prog (_, expr) -> interp_expr Fstore.empty Store.empty expr  *)


