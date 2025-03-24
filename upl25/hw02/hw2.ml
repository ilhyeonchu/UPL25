type id = int
type tree = Nil | N of id * tree * tree

(* fib: int -> inr *)
let fib (n: int) : int =
  let rec fib' (n:int) : int = 
    match n with
    | 0 -> 0
    | 1 -> 1
    | _ when n < 0 -> -1
    | i -> fib'(i - 1) + fib'(i - 2)
  in 
  fib' n
  

(* fib_opt: int -> int *)
let fib_opt (n: int) : int =
  let rec fib_opt' (n: int) (a: int) (b: int) : int = (* a > b *)
    match n with
    | 1 -> a
    | i -> fib_opt' (i-1) (a+b) a
  in
  match n with
    | 0 -> 0
    | 1 -> 1
  | _ when n < 0 -> -1
  | i -> fib_opt' (i-1) 1 1

(* last: int list -> int *)
let last (ls: int list) : int =
  let rec last' (ls: int list) (n: int) : int =
    match ls with
    | [] -> n 
    | m :: t -> last' t m 
  in
  match ls with
  | [] -> failwith "The given list is empty"
  | m :: t -> last' t m 

(* second_last: int list -> int *)
let second_last (ls: int list) : int =
  let rec second_last' (ls: int list) : int =
    match ls with
    | [] -> failwith "The given list is empty" 
    | [_] -> failwith "The given list has a sole element"
    | [fst ; _] -> fst
    | _ :: t -> second_last' t 
  in
  second_last' ls

(* len: int list -> int *)
let len (ls: int list) : int =
  let rec len' (ls: int list) : int =
    match ls with
    | [] -> 0
    | _ :: t -> 1 + (len' t)
  in 
  len' ls

(* rev: int list -> int list *)
let rev (ls: int list) : int list =
  let rec rev' (ls: int list) (re: int list) : int list =
    match ls with
    | [] -> re
    | h :: t -> rev' t (h :: re)
  in
  rev' ls []

(* is_palindrome : int list -> bool *)
let is_palindrome (ls: int list) : bool = 
  let revline = rev ls in 
  if ls = revline then true
  else false

(* compress : string -> (int*char) list *)
let compress (s: string) : (int * char) list = 
  let cl = List.of_seq (String.to_seq s) in 
  let rec compress' (cl: char list) (tp: (int*char) list) = 
     match cl with
    | [] -> tp 
    | h :: t -> 
      match tp with
      | (int, ch) :: tplist when ch = h -> compress' t ((int + 1, ch) :: tplist)
      | _ -> compress' t ((1, h) :: tp) 
  in 
  List.rev (compress' cl [])

(* let fun (a: int) (b: int) : bool =
  if a *)

(* sort : (int -> int -> bool) -> int list -> int list *)
let sort (f: int -> int -> bool) (ls: int list) : int list = 
  let rec insert (f: int -> int -> bool) (sl: int list) (i: int) : int list =
    match sl with
    | [] -> [i]
    | h :: t -> if f i h then i :: sl else h :: insert f t i 
  in     
  let rec sort' (f: int -> int -> bool) (ls: int list) (sl: int list) : int list =
    match ls with
    | [] -> sl 
    | h :: t -> sort' f t (insert f sl h) 
  in
  sort' f ls []

(* Traverse : tree -> id list *)
let traverse (t: tree) : id list = 
  let rec traverse' (t: tree) : id list =
    match t with
    | Nil -> [] 
    | N (id, left, right) -> traverse' left @ traverse' right @ [id]
  in 
  traverse' t 

(* let _ = *)
(*  let print_list lst =
    let _ = Format.printf "[ " in
    let _ = List.iter (fun x -> Format.printf "%d " x) lst in 
  Format.printf "]\n"
  in 
  let lst = [1;2;3;4] in 
  let lstr = rev lst in 
  let _ = print_list lst in 
  print_list lstr
let print_compress lst =
  let _ = Format.printf "[ " in
  List.iter (fun (count, ch) -> Format.printf "(%d, %c) " count ch) lst;
  Format.printf "]\n"

let test = compress "GGGHH" 
let _ = print_compress test *)
