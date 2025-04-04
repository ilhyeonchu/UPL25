type value = NumV of int
type t = (string * value) list
type state = fal | suc

let empty: t = []

let rec search (st: string) (mem: t) : value =
  match mem with
    | [] -> failwith "Free identifier: %s" st 
    | (h, v) :: t -> if h = st then v else search st t 


let find (st: string) (mem: t) : value =
  search st mem

let add (st: string) (val: value) (mem: t) : t =
  let mem' = List.remove_assq st mem in 
    (st, val) :: mem'

