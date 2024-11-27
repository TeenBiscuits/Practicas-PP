print_string "mcd\n\n";;

(* ESTA FUE MI PRIMERA IDEA *)
(*
let rec mcm a b =
  let rec aux n1 n2 =
    if a * n1 = b * n2 then n1
    else if (a * n1) > (b * n2) then aux n1 (n2+1)
    else aux (n1+1) n2
  in
    (aux 1 1) * a
;;
*)

(* AQUI APRENDÍ A LEER *)

(* x * y = mcm(x,y) * mcd(x,y) *)
(* x * y / mcd(x,y) = mcm(x,y) *)

let rec mcd a b =
  if b = 0 then a
  else mcd b (a mod b)
;;

(* Para evitar overflows primero se divide y luego se multiplica *)
let mcm a b =
  a / (mcd a b) * b
;;

(* PRUEBAS *)
print_endline "# mcm 15 6;;";;
print_endline (string_of_int (mcm 15 6));;
print_endline "# mcm 540 84;;";;
print_endline (string_of_int (mcm 540 84));;
print_endline "# mcm 2100 198;;";;
print_endline (string_of_int (mcm 2100 198));;
print_endline "# mcm 12884901888 32212254720;;";;
print_endline (string_of_int (mcm 12884901888 32212254720));;
print_endline "# mcm 90352612350 11394068049;;";;
print_endline (string_of_int (mcm 90352612350 11394068049));;
print_endline "# mcm 2305843009213693952 1152921504606846976;;";;
print_endline (string_of_int (mcm 2305843009213693952 1152921504606846976));;


print_string "\nmcd'\n\n";;

(* ESTA FUE MI PRIMERA IDEA *)
(*
let rec mcm' a b =
  if a <= 0 || b <= 0 then -1
  else
  let rec aux n1 n2 =
    if (a*n1) <= (a*(n1-1)) || (b*n2) <= (b*(n2-1)) then -1
    else if a * n1 = b * n2 then n1
    else if (a * n1) > (b * n2) then aux n1 (n2+1)
    else aux (n1+1) n2
  in
  (* 
    let r = aux 1 1 in 
    if r = -1 then -1 
    else r * a 
  *)
  match aux 1 1 with
     -1 -> -1
    | r -> r * a
;;
*)
(*
let rec mcm' a b =
  if a <= 0 || b <= 0 then -1
  else
  let rec aux n1 n2 =
    if (a*n1) <= (a*(n1-1)) || (b*n2) <= (b*(n2-1)) then -1
    else if a * n1 = b * n2 then n1
    else if (a * n1) > (b * n2) then aux n1 (n2+1)
    else aux (n1+1) n2
  in
  (* 
    let r = aux 1 1 in 
    if r = -1 then -1 
    else r * a 
  *)
  match aux 1 1 with
     -1 -> -1
    | r -> r * a
;;
*)

(* x * y / mcd (x,y) > max_int *)
(* x / mcd(x,y) > max_int / y  *)

let mcm' a b =
  if a <= 0 || b <= 0 then -1
  else if (a / mcd a b) > (max_int / b) then -1
  else a / (mcd a b) * b
;;

(* PRUEBAS *)
print_endline "# mcm' 90352612350 11394068049;;";;
print_endline (string_of_int (mcm' 90352612350 11394068049));;
print_endline "# mcm' 2305843009213693952 1152921504606846976;;";;
print_endline (string_of_int (mcm' 2305843009213693952 1152921504606846976));;
print_endline "# mcm' 0 3;;";;
print_endline (string_of_int (mcm' 0 3));;
print_endline "# mcm' 1 (-1);;";;
print_endline (string_of_int (mcm' 1 (-1)));;
print_endline "# mcm' 2147483649 2147483650;;";;
print_endline (string_of_int (mcm' 2147483649 2147483650));;
print_endline "# mcm' 4294967297 2147483648;;";;
print_endline (string_of_int (mcm' 4294967297 2147483648));;

