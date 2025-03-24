type IDENT = string
type NUMBER = string
type KW_LET = "let" (* q0 q1 q2 q3 *)
type KW_IN = "in" (*
type OP_EQ = "="
type OP_PLUS = "+"
type OP_MINUS = "-"
type token = IDENT | NUMBER | KW_LET | KW_IN | OP_MINUS | OP_PLUS | OP_EQ
type parse_stack_elem = | T of token | E of expr
type TKL = token list 

let chf (cl: char list) : bool =
  match cl with
  | [] -> false
  | _ -> true

let lex (str: string) : token =
  let cl = List.of_seq (String.to_seq str) in 
  match (cl, S) with
    | (, Q0) -> 
