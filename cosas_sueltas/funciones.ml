(* let rec factorial = function 0 -> 1 | n -> n * factorial (n-1) *)

let rec sumto = function 0 -> 0 | n -> n + sumto (n-1)

let rec exp2 = function 0 -> 1 | n -> 2 * exp2 (n-1)

let rec num_cifras = function 0 -> 0 | n -> n mod (10 ** (* SIN ACABAR *)
