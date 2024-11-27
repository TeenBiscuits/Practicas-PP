let rec mcd a b =
  if b = 0 then a
  else mcd (min a b) ((max a b) mod (min a b))
;;

let mcm' a b =
  if a <= 0 || b <= 0 then invalid_arg "argumento inválido"
  else if (a / mcd a b) > (max_int / b) then invalid_arg "mcm excede max_int"
  else a / (mcd a b) * b
;;

