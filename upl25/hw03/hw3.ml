type state = Q0 | Q1 | Q2 | Q3 | Q4 | Q5 | Q6 | Q7 | Q8 | Fal 
type token = IDENT of string | NUMBER of string | KW_LET | KW_IN | OP_MINUS | OP_PLUS | OP_EQ
type expr = LetIn of string * expr * expr | Plus of expr * expr | Minus of expr * expr | Num of string | Id of string 
type parse_stack_elem = | T of token | E of expr 

let rec lexS (cl: char list) (s: state) : state =  
  match cl with
  | [] -> if s = Q3 || s = Q4 || s = Q8 then s else Fal
  | h :: t -> 
    match (h, s) with
    | ('l', Q0) -> lexS t Q1
    | ('e', Q1) -> lexS t Q2
    | ('t', Q2) -> lexS t Q3
    | ('i', Q0) -> lexS t Q7
    | ('n', Q7) -> lexS t Q8
    | ('A'..'Z', Q0) -> Fal
    | ('A'..'Z', _) -> lexS t Q4
    | ('a'..'z', _) -> lexS t Q4
    | (_, _) -> Fal

let rec lexN (cl: char list) (s: state) : state =
  match cl with
    | [] -> s 
    | h :: t -> if (h <= '9' && h >= '0') then lexN t Q6 else Fal

let lex (str: string) : token =
  let cl = List.of_seq (String.to_seq str) in 
  let state = 
    match cl with
    | [] -> Fal
    | h :: t ->
    match h with
      | 'l' -> lexS t Q1
      | 'i' -> lexS t Q7
      | 'a'..'z' -> lexS t Q4
      | '1'..'9' -> lexN t Q6
      | '=' -> Q5
      | '+' -> Q5
      | '-' -> Q5
      | _ -> Fal
    in 
    match state with
      | Q3 -> KW_LET
      | Q8 -> KW_IN
      | Q4 -> IDENT str
      | Q6 -> NUMBER str
      | Q5 -> if str = "=" then OP_EQ else if str = "+" then OP_PLUS else OP_MINUS
      | Fal -> failwith ("Lexing error: " ^ str)
      | _ -> failwith ("Lexing error: " ^ str)


let rev (ls: token list) : token list =
  let rec rev' (ls: token list) (re: token list) : token list =
    match ls with
    | [] -> re
    | h :: t -> rev' t (h :: re)
  in
  rev' ls []


let rec make_tkl (sl: string list) (tkl: token list) : token list =
  match sl with
    | [] -> rev tkl
    | h :: t -> make_tkl t ((lex h) :: tkl)

let rec parse' (stack: parse_stack_elem list) (tkl: token list) (input: string) : expr =
  match (stack, tkl) with 
    | [E e], [] -> e 
    | T (NUMBER h) :: t, _ -> parse' (E (Num h) :: t) tkl input
    | T (IDENT h) :: t, tk :: _ when tk <> OP_EQ -> parse' (E (Id h) :: t) tkl input
    | E e2 :: T OP_PLUS :: E e1 :: t, [] -> parse' (E (Plus (e1, e2)) :: t) [] input
    | E e2 :: T OP_MINUS :: E e1 :: t, [] -> parse' (E (Minus (e1, e2)) :: t) [] input
    | E e2 :: T KW_IN :: E e1 :: T OP_EQ :: T (IDENT id) :: T KW_LET :: t, [] -> parse' (E (LetIn (id, e1, e2)) :: t) [] input
    | _, h :: t -> parse' (T h :: stack) t input
    | _, _ -> failwith ("Parsing error: " ^ input)
let parse (str : string) : expr =
  let sl = String.split_on_char ' ' str in 
  let tkl = make_tkl sl [] in 
  parse' [] tkl str 
