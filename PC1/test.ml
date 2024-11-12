open Hanoi_plus;;

let crono f x =
  let t = Sys.time () in
  let _ = f x in
  Sys.time () -. t
;;

let crono2 f n l1 l2 =
  let t = Sys.time () in
  let _ = f n l1 l2 in
  Sys.time () -. t
;;
let rec fib n = if n <= 2 then n else fib (n-1) + fib (n-2);;

print_endline "TEST DE HANOI PLUS";;
(*
print_string "1. ¿Funciona? ";;
print_endline (string_of_bool (Hanoi_plus.tests 1));;
print_endline "2. TIEMPOS";;
*)
print_string "  2.1 Tiempo Fib: ";;
print_float (crono fib 45);;
print_endline "";;
print_string "  2.2 Tiempo Hanoi: ";;
let l = List.init 24 (fun i -> i + 1);;
print_float (crono2 Hanoi_plus.hanoi 24 (l, [], []) ([], [], l));;