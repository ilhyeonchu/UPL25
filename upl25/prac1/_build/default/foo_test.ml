(* Test Code*)

(* success *)
let%test _ = Foo.foo 2 = 2

(* fail *)
  let%test _ = Foo.foo 2 = 3
