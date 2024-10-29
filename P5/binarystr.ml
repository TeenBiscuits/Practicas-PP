(* Convierte un entero positivo en su representación binaria *)

let rec binstr_of_int x = 
  if x = 0 then "0"
  else  binstr_of_int(x/2) ^ (string_of_int)(x mod 2)
;;

(* Dado un string con un número natural binario devolver 
un valor de tipo int *)

exception Foo of string;;

let valor x = 
  if x == '1' then 1
  else if x == '0' then 0
  else if x == ' ' then 0
  else raise (Foo "Encontrado caracter que no era ni 1 ni 0.")
;;

let rec int_of_binstr x =
  if (String.length x = 1) then valor x.[0]
  else (valor x.[String.length x - 1] + 
    2 * (int_of_binstr(String.sub x 0 (String.length x - 1))))
;;


let rec int_of_binstr' x =
  if (String.length x = 1) then valor x.[0] else
  if x.[String.length x - 1] == ' '
    then int_of_binstr'(String.sub x 0 (String.length x - 1))
  else(valor x.[String.length x - 1] + 
    2 * (int_of_binstr'(String.sub x 0 (String.length x - 1))))
;;