let tow (ch: char) : _ =
  match ch with
    |'A'..'Z' -> Format.printf "%c\n" 'U'
    |'a'..'z' -> Format.printf "%c\n" 'd'
    | _ -> Format.printf "%c\n" 'n'
let _ = tow 'v'
let _ = tow 'V'
let _ = tow '4'

type state = Q1 | Q1 | ...

let lex (str:string) : token = 
  let aux (s: state) (c: char) : state =
  match s, c with
    |Q0, '0' -> Q1
    |...
