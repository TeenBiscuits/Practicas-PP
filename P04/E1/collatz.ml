let f n = if n mod 2 = 0 then n / 2 else 3 * n + 1;;

let rec verify n =
  n = 1 || verify (f n);;

print_string "Veracidad de la conjetura en el número 871: ";;
print_endline (string_of_bool (verify 871));;

(* ESTA FUNCIÓN LA ESCRIBI YO *)
let rec verify_to n =
  n = 0 || (verify (n) && verify_to (n-1));;

(* ESTA ES LA IMPLEMENTACIÓN QUE EXPLICÓ EL PROFESOR *)
(*
let rec verify_to n =
  if n = 1 then verify n
  else verify n && verify_to (n-1);;
*)

print_string "Veracidad de la conjetura del primer millón de números naturales: ";;
print_endline (string_of_bool (verify_to 1_000_000));;

let rec orbit n =
  if n = 1 then "1"
  else (string_of_int n ^ ", " ^ orbit (f n));;

print_string "Órbita del número 13: ";;
print_endline (orbit 13);;

let rec length n =
  if n = 1 then 1
  else (1 + length (f n));;

print_string "Longitud de la órbita del número 27: ";;
print_endline (string_of_int (length 27));;

let i = 0;;
let rec top n =
  if n = 1 then 1
  else max n (top (f n));;

print_string "Valor más alto de la órbita de 27: ";;
print_endline (string_of_int (top 27));;