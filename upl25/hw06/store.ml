type value = NumV of int
type t = (string * value) list

let empty: t = []

let rec search (st: string) (mem: t) : value =
  match mem with
    | [] -> failwith ("Free identifier: " ^ st) 
    | (h, v) :: t -> if h = st then v else search st t 


let find (st: string) (mem: t) : value =
  search st mem

let add (st: string) (vl: value) (mem: t) : t =
  let mem' = List.remove_assq st mem in 
    (st, vl) :: mem'

let rec add' (stl: string list) (vl: value list) (mem: t) : t =
  match (stl, vl) with
    | (stlh::stlt, vlh::vlt) -> add' stlt vlt (add stlh vlh mem)
    | ([], []) -> mem
    | (_, _) -> failwith " "
