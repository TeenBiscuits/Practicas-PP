let rec fib n =
  if n <= 1 then n
  else fib (n-1) + fib (n-2)
;;

let per_area r =
  let pi = 2. *. asin 1. in
  (2. *. pi *. r, pi *. r *. r)
;;

let per_area =
  let pi = 2. *. asin 1. in
  function r -> (2. *. pi *. r)
;;

(* dados dos enteros devuelve el conciente *)
let rec quo x y = (* x >= 0, y > 0 *)
  if x < y then 0
  else 1 + quo (x-y) y
;;

(* dados dos enteros devuelve el resto *)
let rec rem x y =
  if x < y then x
  else rem (x-y) y
;;


(*
let rec div x y =
  if x < y then (0, x)
  else
    let p = div (x-y) y in 
    (1+fst)
*)

let rec div x y =
  if x < y then (0, x)
  else
    let (q, r) = div (x-y) y in
    (1 + q, r)
;;

(* FALTAN ALGUNAS COSAS DE LA CLASE DEL 07/10 *)


