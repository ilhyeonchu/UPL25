let sum x y = x + y in (* sum : int -> int -> int *)
let sum' = sum 1 in (* sum' : int -> int -> int *)
Format.printf "Result: %d\n" (sum' 3)
