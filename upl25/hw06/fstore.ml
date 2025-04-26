type t = (string * (string list * Ast.expr)) list

let empty : t = []

(* find : string -> t -> string list * Ast.expr *)
let rec find (st: string) (t: t) : (string list * Ast.expr) =
  match t with
    | [] -> failwith ("Undefined function: " ^ st)
    | (h, tuple) :: t -> if h = st then tuple else find st t 

(* add : string -> string list -> Ast.expr -> t -> t *)
let add (st: string) (stl:string list) (exp: Ast.expr) (memf: t) : t =
  let memf' = List.remove_assq st memf in
  (st, (stl, exp)) :: memf'
