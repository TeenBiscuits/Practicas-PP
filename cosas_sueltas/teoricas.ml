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


let rec fib n = (* n >= 1 *)
  if n <= 2 then 1
  else fib (n-1) + fib (n-2);;
  
let crono f x =
  let t = Sys.time () in
  let _ = f x in
  Sys.time () -. t;;

(* CLASE DEL 14/10 *)

(* Operaciones con Listas https://ocaml.org/manual/5.2/api/List.html *)

let l = ['a';'e';'i';'o';'u'];;

List.hd l;; (* Devuelve 'a' *)

List.tl l;; (* Devuelve ['e'; 'i'; 'o'; 'u'] *)

List.rev l;; (* Devuelve ['u'; 'o'; 'i'; 'e'; 'a'] *)

let rec lenght l =
  if l = [] then 0
  else 1 + List.length (List.tl l) 
  (* Con cada ejecución se acumula un 1 + (llamada recursiva) *)
  (* Con un array lo suficientemente larga podría agotarse el stack *)
;;

(* TRANQUILIDAD :/ *)

List.map abs [1;-2;3];; (* Se aplica la función en cada elemento de la lista *) (* [1; 2; 3] *)

(* Sumarle a cada elemento 1 *)
let suma x y = x + y;;
let suma' (x,y) = x + y;;

List.map (suma 1) [1;2;3];;
List.map (function n -> suma' (1,n)) [1;2;3];;

(* Multiplicar cada elemento por 2 *)
List.map (( * ) 2) [1;2;3];;

(* Que elemento lo cumplen *)
List.filter ((<) 0) [1;-2;3];; (* [1;3] *)

List.exists ((<) 0) [1;2;3];; (* True *)
List.exists ((<) 0) [-1;2;3];; (* False *)

List.for_all ((<) 0) [1;2;3];; (* True *)

(* El primero que encuentra lo devuelve *)
List.find ((<) 0) [-1;2;3];; (* 2 *)
List.find ((<) 0) [-1;-2;-3];; (* Exception: Not_found *)

(* ¿Es miembro? *)
List.mem 2 [1;2;3];; (* True *)

(* Longitud *)
List.length [1;2;3;4];; (* 4 *)

(* Creación de listas *)
List.init 10 abs;; (* [0; 1; 2; 3; 4; 5; 6; 7; 8; 9] *)

let l2 = List.init 50_000_000 abs;; 

List.hd (List.rev l2);; (* 49999999 *)

List.length l2;; (* 50000000 *)

lenght l2;; (* Stack Overflow *)

List.init 26 (function n -> char_of_int (65+n));; (* El abecedario completo *)
