let%test _ =
try
  let _ = Hw9.interp (ParserMain.parse "1 1") [] in false
with
|Failure msg -> msg = "Not a function: 1"

let%test _ =
try
  let _ = Hw9.interp (ParserMain.parse "1 + true") [] in false
with
|Failure msg -> msg = "Not a number: 1 + true"
