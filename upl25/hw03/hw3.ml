type IDENT = string (* q0 q4 *)
type NUMBER = string (* q0 q6 *)
type KW_LET = "let" (* q0 q1 q2 q3 *)
type KW_IN = "in" (* q0 q7 q8 *)
type OP_EQ = "=" (* q0 q5 *)
type OP_PLUS = "+" (* q0 q5 *)
type OP_MINUS = "-" (* q0 q5 *)
type state = q0 | q1 | q2 | q3 | q4 | q5 | q6 | q7 | q8 | false
type token = IDENT | NUMBER | KW_LET | KW_IN | OP_MINUS | OP_PLUS | OP_EQ
type parse_stack_elem = | T of token | E of expr
type TKL = token list 

let chf (cl: char list) : bool =
  match cl with
  | [] -> false
  | _ -> true

let lexS (sl: string list) (S: state) : state =  
  match sl with
    | [] -> S 
    | h :: t ->
      match S with
      | q3 | q4 | q8 -> lexS t q4 
      | q0 -> if h = 'l' then lexS t q1 else lexS t q4
      | q1 -> lexS t q2
        | ('t', q2) -> lexS t q3
        | ('i', q0) -> lexS t q7
        | ('n', q7) -> lexS t q8
        | (_, _) -> false
      | _ -> false

let lexN (sl: string list) (S: state) : state =
  match sl with
    | [] -> S 
    | h :: t ->
      match S with
      | 

let lex (str: string) : token =
  let cl = List.of_seq (String.to_seq str) in 
  let lex' (ls: string list) (S: state) : state =
      match ls with
      | [] -> S 
      | h :: t -> 
        match S with
        | q0 | q1 | q2 | q3 -> 
