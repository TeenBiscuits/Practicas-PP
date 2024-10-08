(* let rec factorial = function 0 -> 1 | n -> n * factorial (n-1) *)

let rec sumto = function 0 -> 0 | n -> n + sumto (n-1)

let rec exp2 = function 0 -> 1 | n -> 2 * exp2 (n-1)

let rec num_cifras = function n -> let n = abs n in if n < 10 then 1 else 1 + num_cifras (n / 10);;

let rec sum_cifras = function n -> let n = abs n in if n < 10 then n else (n mod 10) + sum_cifras (n / 10);;
