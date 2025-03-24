let sum x y = x + y
let sub x y = x - y
let mul x y = x * y
let div x y = 
  if y = 0 then failwith "Divide-by-Zero"
  else x / y
let calc oper x y = (* calc : char -> int -> int *)
  if oper = '+' then sum x y (* sum  *)
  else if oper = '-' then sub x y 
  else if oper = '*' then mul x y
  else if oper = '/' then div x y
  else failwith "Unsupported operation"
 
  
