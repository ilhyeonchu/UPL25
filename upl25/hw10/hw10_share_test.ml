let%test _ = 
try
  let _ = Hw10.interp (ParserMain.parse "1 + false") [] in false
with
|Failure msg -> msg = "Not a number: 1 + false"

let%test _ = 
try
  let _ = Hw10.interp (ParserMain.parse "let rec x = 3 in x") [] in false
with
|Failure msg -> msg = "Not a function: 3"

let%test _ = 
try
  let _ = Hw10.interp (ParserMain.parse "true true") [] in false
with
|Failure msg -> msg = "Not a function: true"

let%test _ = 
try
  let _ = Hw10.interp (ParserMain.parse "if 1 then 2 else 3") [] in false
with
|Failure msg -> msg = "Not a bool: 1"

let%test _ = 
try
  let _ = Hw10.interp (ParserMain.parse "let owo = 3 < false in owo + 1") [] in false
with
|Failure msg -> msg = "Not a number: 3 < false"
