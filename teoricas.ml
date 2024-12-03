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

(* CLASES DEL 15/10 *)

let rec last l =
  if List.tl l = [] then List.hd l
  else last (List.tl l)
;;

let rec last l =
  if List.length l = 1 then List.hd l
  else last (List.tl l)
;;

(*
<e1> : t
<e2> : t list
-------------------
<e1>::<e2> : t list
*)

(* Constructor de listas *)

3 :: 7 :: 9 :: [];; (* [3; 7; 9] *)

let hd = function
  [] -> 2/0
| h::_ -> h
;;

let hd = function h::t -> h;;

let hd = function h::_ -> h;;

let hd (h::_) = h;;

let hd = function
  [] -> raise (Failure "hd")
| h::_ -> h
;;

(* CREAR UNA VERSIÓN TERMINAL DE LA SIGUIENTE FUNCIÓN *)

let rec lenght = function
  [] -> 0
| _::t -> 1 + lenght t
;;


(* Existe el elemento x en la lista l *)
(*              mem x l;;             *)

let rec mem x = function
  [] -> false
| h::t -> x = h || mem x t
(* Esto es lo mismo que escribir lo siguente *)
(* | l -> x = List.hd l || mem x (List.tl l) *)
;;

(* mem 3 [1;2;3]      1::2::3::[] *)
(* mem 3 [2;3]           2::3::[] *)
(* mem 3 [3]                3::[] *) (* x = h TRUE*)

(* mem 3 [4;5;6]      4::5::6::[] *)
(* mem 3 [5;6]           5::6::[] *)
(* mem 3 [6]                6::[] *)
(* mem 3 [1;2;3]      1::2::3::[] *)
(* mem 3 [2;3]           2::3::[] *)
(* mem 3 []                    [] *) (* [] -> FALSE *)


(* REIMPLEMENTAMOS List.find *)

let rec find p = function
  [] -> raise Not_found
| h::t -> if p h then h else find p t
;;

(* REIMPLEMENTAMOS List.filter *)

let rec filter p = function
  [] -> []
| h::t ->
  if p h then h :: filter p t
  else filter p t
;;


(* Clase del 21/10 *)

(* Reimplementación de lenght *)

let rec aux = function
  (n,[]) -> n
| (n, _::t) -> aux(n+1,t)
;;

let lenght l =
  aux (0,l)
;;

let lenght l =
  let rec aux = function
    (n,[]) -> n
  | (n, _::t) -> aux(n+1,t)
  in
    aux (0,l)
;;


let rec append = function
    [] -> (function  l -> l)
  | h::t -> (function l -> h :: append t l)
;;

let rec append l1 l2 =
  match l1 with
  [] -> l2
  | h::t -> h :: append t l2
;;

(* CLASE 22/10 *)

let compare_lengs l1 l2 =
  compare (List.length l1) (List.length l2)
;;


let rec compare_lengs = function
  [] -> (function [] -> 0
                | _  -> -1)
| _::t1 -> (function [] -> 1
                | _::t2 -> compare_lengs t1 t2)
;;
let rec compare_lengs l1 l2 =
  match (l1, l2) with
    ([], []) -> 0
  | ([], _) -> -1
  | (_, []) -> 1
  | (_::t1, _::t2) -> compare_lengs t1 t2
;;

let rec fact = function
    0 -> 1
  | n -> n * fact (n-1)
;;

(* Versión terminal del factorial *)

(* El acumulador n va guardando los valores de las multiplicaciones *)
(*
aux 0 1
aux 1 1
aux 2 2
aux 3 6
aux 4 24
aux 5 120
120
*)
let fact n =
  let rec aux acc = function
    0 -> acc
  | i -> aux (i*acc) (i-1)
  in
    aux 1 n
;;

(* El único defecto de la función anterior es que en cada repetición se redefine aux *)
(* Si el compilador no es lo suficientemente inteligente *)


(*Esta función es parcialmente incorrecta *)
let fact n =
  let rec aux i fi =
    if i = n then fi
    else aux (i+1) ((i+1)*fi)
  in
    aux 0 1
;;

List.fold_left (+) 0 [1;2;3;4;5] (* 15 *)

let lenght l =
  List.fold_left (function a -> function _ -> a+1) 0 l
;;

(*
let rec fold_left a = function
  [] -> a
| h::t -> fold_left f (f a h) t
;;
*)

let rev l =
  List.fold_left (function a -> function x -> x::a) [] l
;;

(* Dado una lista devolver el máximo *)
(* lmax / también existe la función max en ocaml *)
(* soltar error cuando se envie una lista vacía *)


(* CLASES 28/10 *)

let rec lmax = function
  h::[] -> h
| h::t ->
      if h >= lmax t then h else lmax t
;;

let rec lmax = function
  h::[] -> h
| h::t -> max h (lmax t)
;;

let lmax (h::t) =
  let rec aux m = function
    [] -> m
    | h::t -> aux (max h m) t
  in
    aux h t
;;

let rec lmax = function
  h::[] -> h
| h1::h2::t -> lmax (max h1 h2::t)
;;

let rec append l1 l2 =
  match l1 with
    [] -> l2
  | h::t -> h :: append t l2
;;

let rec rev_append l1 l2 =
  match l1 with
    [] -> l2
  | h::t -> rev_append t (h::l2)
;;

let append' l1 l2 =
  List.rev_append (List.rev l1) l2
;;

let rev l =
  List.rev_append l []
;;

let rev l =
  List.fold_left
    (function a -> function x -> x::a) [] l
;;

(* ESTA ES LA UTILIDAD DE FUN *)
let rev l =
  List.fold_left
    (fun a x -> x::a) [] l
;;


(* MUY MALA IMPLEMENTACIÓN DE REV LIST*)
let rec rev = function
    [] -> []
  | h::t -> (rev t) @ [h]
;;

(*
rev [1;2;3;4]
(rev [2;3;4]) @ [1]
(rev [3;4] @ [2]) @ [1]
((rev [4] @ [3]) @ [2]) @ [1]
(((rev [] @ [4]) @ [3]) @ [2]) @ [1]
((([] @ [4]) @ [3]) @ [2]) @ [1]
(([4] @ [3]) @ [2]) @ [1]
([4;3] @ [2]) @ [1]
[4;3;2] @ [1]
[4;3;2;1]
*)

let rec for_all p = function
    [] -> true
  | h::t -> p h && for_all p t
;;

let for_all p l =
  List.fold_left (fun a x -> p x && a) true l
;;


(* CLASES 29/10 *)

let rec sorted = function
  h1::h2::t -> h1 <= h2 && sorted (h2::t)
| _ -> true
;;

(*if h1 <= h2 then sorted (h2::t) else then*)

let rec insert x = function
  [] -> [x]
| h::t ->
    if x <= h then x::h::t
    else h :: insert x t
;;

(* Versión terminal *)
let insert' x l =
  let rec aux (before, after) =
    match after with
      [] -> List.rev (x::before)
    | h::t ->
      if x <= h then List.rev_append before (x::after)
      else aux (h::before, t)

  in
    aux ([], l)
;;

let rec i_sort = function
  []-> []
| h::t -> insert' h (i_sort t)
;;

(* Versión terminal *)
let i_sort' l =
  let rec aux sorted = function
      [] -> sorted
    | h::t -> aux (insert' h sorted) t
  in
    aux [] l
;;

let l1 = List.init 10_000 (function _ -> Random.int 1_000_000);;

let l2 = List.init 300_000 (function _ -> Random.int 1_000_000);;

let l3 = List.init 10_000_000 (function _ -> Random.int 1_000_000);;

(* Función de medir tiempos *)
let crono f x =
  let t = Sys.time () in
  let _ = f x in
  Sys.time () -. t
;;

(* CLASES 04/11 *)

(* Dado una función de ordenación "ord" y una lista *)
let rec sorted ord = function
  h1::h2::t -> ord h1 h2 && sorted ord (h2::t)
| _ -> true
;;

(* sorted (>=) [9;9;8;6;0];; - : bool = true;; *)

let rec sorted_g ord = function
  h1::h2::t -> ord h1 h2 && sorted_g ord (h2::t)
| _ -> true
;;

let rec insert x = function
  [] -> [x]
| h::t -> if x <= h then x::h::t
  else h :: insert x t
;;

let rec insert_g ord x = function
  [] -> [x]
| h::t ->
    if ord x h then x::h::t
    else h :: insert_g ord x t
;;

let rec isort = function
  [] -> []
| h::t -> insert h (isort t)
;;

let rec isort_g ord = function
  [] -> []
| h::t -> insert_g ord h (isort_g ord t)
;;

(*
# isort_g (<=) [22; 3; 14; 0; -2; 8; 0; 7; 3; 5];;
- : int list = [-2; 0; 0; 3; 3; 5; 7; 8; 14; 22]
# isort_g (>=) [22; 3; 14; 0; -2; 8; 0; 7; 3; 5];;
- : int list = [22; 14; 8; 7; 5; 3; 3; 0; 0; -2]
# isort_g (=) [22; 3; 14; 0; -2; 8; 0; 7; 3; 5];;
- : int list = [5; 3; 3; 7; 0; 0; 8; -2; 14; 22]
*)

let l = [("Luis", 8.9); ("Juana", 9.2); ("Rosa", 10.0); ("Ana", 7.5)];;

(* isort_g (ord) l;; *)

(<=);; (* Compara los primeros elementos y en caso de empate el segundo *)
(* En este caso comparación de strings es orden alphavético *)

fun (_, n1) (_,n2) -> n1 <= n2;; (* Ordenar por notas *)

fun (s1, _) (s2, _) -> String.length s1 <= String.length s2;; (* Ordenar por tamaño de los strings *)

let rec fusion l1 l2 =
  match l1, l2 with
  [], l | l, [] -> l
| h1::t1, h2::t2 ->
  if h1 <= h2
  then h1 :: fusion t1 l2 (* h2::t2 == l2 *)
  else h2 :: fusion l1 t2
;;

(*
let rec fusion_g ord l1 l2 =
  match l1, l2 with
  [], l | l, [] -> l
| h1::t1, h2::t2 ->
  if ord h1 h2
  then h1 :: fusion_g ord t1 l2 (* h2::t2 == l2 *)
  else h2 :: fusion_g ord l1 t2
;;
*)

let rec divide = function
  h1::h2::t ->
    let t1, t2 = divide t in
    h1::t1, h2::t2
  | l -> l, []
;;

let rec merge_sort l =
  match l with
    [] | [_] -> l
  | _ ->
      let l1, l2 = divide l in
      fusion (merge_sort l1) (merge_sort l2)
;;

let rlist n =
  List.init n (function _ -> Random.int 1_000_000)
;;

let crono f x =
  let t = Sys.time () in
  let _ = f x in
  Sys.time () -. t
;;


(* CLASES 05/11 *)

let rec divide = function
  h1::h2::t ->
    let l1, l2 = divide t in
    h1::l1, h2::l2
| l -> l, []
;;

let divide' l =
  let rec aux acc1 acc2 = function
    [] -> acc1, acc2 (* Una vez repartidos todos los elem. de una lista par que la lista vacía *)
    | h::[] -> h::acc1, acc2 (*h::[] == [h]*) (* De ser una lista impar sobraría uno*)
    | h1::h2::t ->
      aux (h1::acc1) (h2::acc2) t (* Añadir a los acumuladores los dos primeros elementos de la lista *)
  in
  aux [] [] l
;;


let rec fusion l1 l2 =
  match l1, l2 with
    [], l | l, [] -> l
  | h1::t1, h2::t2 ->
      if h1 <= h2
        then h1 :: fusion t1 l2
        else h2 :: fusion l1 t2
;;


let fusion' l1 l2 =
  let rec aux acc = function

    h1::t1, h2::t2 ->
      if h1 <= h2
        then aux (h1::acc) (t1, h2::t2)
        else aux (h2::acc) (h1::t1, t2)
  in
    aux [] (l1, l2)
;;

(*
fusion'   134 25689
aux []    134 25689
aux 1     34  25689
aux 21    34  5589
aux 321   4   5689
aux 4321  []  5689
*)


(* REALIZAR QUICK SORT DE MANERA TERMINAL NO ES RENTABLE *)


(* CLASES 11/11 *)

(*

'a option

None;;
Some 'a

*)

let (//) m n =
  if n <> 0
  then Some (m / n)
  else None
;;

let come (i1, j1) (i2, j2) =
  i1 = i2 || (* misma fila *)
  j1 = j2 || (* misma columna *)
  abs (i1-i2) = abs (j1-j2) (* misma diagonal *)
;;

let rec compatible c = function
  [] -> true
| h::t -> not (come c h) && compatible c t
;;

let reinas n =
  let rec completa camino (i, j) =
      if i > n then Some camino         (* Salí fuera del tablero filas, es decir terminé *)
      else if j > n then None           (* Salí fuera del tablero columnas, estado de bloqueo *)
      else if compatible (i,j) camino   (* ¿ Es la posición encontrada válida ?  *)

        (* Si, devuelvo las demás        )
        (  reinas y la nueva, avanzamos  )
        (  a la siguiente fila          *)

        then
          match completa ((i,j)::camino) (i+1, 1) with
            None -> completa camino (i, j+1)
          | solucion -> solucion
        (*| Some sol -> Some sol *)

        (* No, y continuamos a la        )
        (  siguiente columna            *)

        else completa camino (i, j+1)
  in
    completa [] (1,1)
;;


(*
type exn =
  Invalid_argument of string
| Failure of string
| Not_found
| Division_by_zero
| ...
| Reinas

exception Reinas;;

raise : exn -> a'

*)

let tl' = function
  [] -> []
| _::t -> t
;;

let tl' l =
  try List.tl l with
    Failure _ -> []
;;

let reinas n =
  let rec completa camino (i, j) =
      if i > n then camino
      else if j > n then raise Not_found (* None *)
      else if compatible (i,j) camino
        then
          (* SUSTITUIMOS ESTE PATTERN MATCHING CON UN TRY WITH
          match completa ((i,j)::camino) (i+1, 1) with
            None -> completa camino (i, j+1)
          | solucion -> solucion
          *)
          try completa ((i, j) :: camino) (i+1, 1) with
            Not_found -> completa camino (i, j+1)
      else completa camino (i, j+1)
  in
    completa [] (1,1)
;;

(* En vez de SOME O NONE o un Not_found usamos la lista vacía *)
let reinas n =
  let rec completa camino (i, j) =
      if i > n then [camino]
      else if j > n then []
      else if compatible (i,j) camino
        then
          match completa ((i, j) :: camino) (i+1, 1) with
            [] -> completa camino (i, j+1)
          | sol -> sol
      else completa camino (i, j+1)
  in
    completa [] (1,1)
;;

(* TIENE MAYOR COHERENCIA INTERNA YA QUE
# reinas 0;; - : (int * int) list list = [[]]                                 (* SOLUCIÓN: NO HACER NADA*)
# reinas 1;; - : (int * int) list list = [[(1, 1)]]                           (* SOLUCIÓN: LA TRIVIAL *)
# reinas 2;; - : (int * int) list list = []                                   (* NO TIENE SOLUCIÓN *)
# reinas 3;; - : (int * int) list list = []                                   (* NO TIENE SOLUCIÓN *)
# reinas 4;; - : (int * int) list list = [[(4, 3); (3, 1); (2, 4); (1, 2)]]   (* SOLUCIÓN *)
*)

let all_reinas n =
  let rec completa camino (i, j) =
      if i > n then [camino]
      else if j > n then []
      else if compatible (i,j) camino
        then
          completa ((i, j) :: camino) (i+1, 1) @ completa camino (i,j+1)
      else completa camino (i, j+1)
  in
    completa [] (1,1)
;;

(* Contador de soluciones

all_reinas 4;;
- : (int * int) list list =
[[(4, 3); (3, 1); (2, 4); (1, 2)]; [(4, 2); (3, 4); (2, 1); (1, 3)]]

all_reinas 5;;
all_reinas 6;;
List.length (all_reinas 8);; - : int = 92
List.length (all_reinas 10);;
List.lenght (all reinas 13);;
*)

let all_reinas n =
  let rec completa camino (i, j) =
      if i > n then 1
      else if j > n then 0
      else if compatible (i,j) camino
        then
          completa ((i, j) :: camino) (i+1, 1) + completa camino (i,j+1)
      else completa camino (i, j+1)
  in
    completa [] (1,1)
;;

(* CLASES 18/11 *)

(* ÁRBOLES *)

type maybe_an_int =
	Some of int
	| None
;;

let (//) x y =
	match x, y with
	Some _, Some 0 -> None
	| Some a, Some b -> Some (a/b)
	| _ -> None
;;

(*Definir un tipo booleano*)
type booleano = V | F;;

(*Se utiliza con pattern matching*)
let (&&&) b1 b2 =
  match b1, b2 with
    V, V -> V
    | _ -> F
;;

let (|||) b1 b2 =
  match b1, b2 with
  V, _ | _, V -> V
  | _ -> F
;;


type palo =
  Pica
  | Trebol
  | Corazon
  | Diamante
;;


type numero =
  I of int
| F of float
;;

let rec (++) n1 n2 =
  match n1, n2 with
  I x, I y -> I (x + y)
| F x, F y -> F (x +. y)
| I x, F y -> F (float x +. y)
| _ -> n2 ++ n1
;;

type maybe_an_int =
   Some of int
 | None
;;

(* PREGUNTAR POR ESTE ERROR *)
type maybe_a_string =
  Somes of string
| Nones
;;

type 'a option =
  Some of 'a
| None
;;

type nat =
  Zero
| Succ of nat     (* Succ: sucesor *)
;;



(* CLASES 19/11 *)

type nat =
  Zero
| Succ of nat
;;

let rec sum n1 = function
  Zero -> n1
| Succ n2 -> sum (Succ n1) n2
;;

let rec nat_of_int = function
  0 -> Zero
| n ->
    if n < 0 then raise (Invalid_argument "nat_of_int")
    else Succ (nat_of_int (n-1))
;;

(* Versión con una sola comprobación y no en cada iteración *)

(* VERSIÓN PROFESOR
let rec nat_of_int n =
  0 -> Zero
| n -> Succ (nat_of_int (n-1))
;;

let nat_of_int n =
  if n < 0 then raise (Invalid_argument "nat_of_int")
  else nat_of_int n
;;
*)

(*
let nat_of_int n =
  (* FUNCIÓN AUX *)
  let rec nat_of_int n = function
      0 -> Zero
    | n -> Succ (nat_of_int (n-1))
  in

  if n < 0 then raise (Invalid_argument "nat_of_int")
  else nat_of_int n (* LLAMADA A AUX *)

;;
*)

type 'a tree =
  Empty
| Node of ('a * 'a tree * 'a tree)
;;

let rec num_nodes = function
  Empty -> 0
| Node (_, l, r) -> 1 + num_nodes l + num_nodes r
;;

let rec altura = function
  Empty -> 0
| Node (_, l, r) -> 1 + max (altura l) (altura r)
;;

let rec preorden = function
  Empty -> []
| Node (n, l, r) -> (n :: preorden l) @ (preorden r)
;;

let rec hojas = function
  Empty -> []
| Node (n, Empty, Empty) -> [n]
| Node (_, l, r) -> (hojas l) @ (hojas r)
;;


(* CLASES 25/11 *)

(*
            2
          7   5

  Node of (2)     (7)       (5)
  Node of 'a * 'a tree * 'a tree
*)

(* Tipo de árbol de la clase pasada *)
type 'a tree =
  Empty
| Node of 'a * 'a tree * 'a tree
;;

(* Tipo de árbol ESTRICTAMENTE binario, no puede haber un árbol con solo una hoja *)
type 'a btree =
  Leaf of 'a
| Node of 'a * 'a btree * 'a btree
;;

let rec mirror = function
  Leaf x -> Leaf x
| Node (x, l, r) -> Node (x, mirror r, mirror l)
;;

type 'a gtree =
  GT of 'a * 'a gtree list
;;

let rec num_nodes = function
  GT (_, []) -> 1
| GT (_, l) ->
        List.fold_left (+) 1 (List.map num_nodes l)
        (* Suma todos los elementos de la lista, mas el nodo padre *)
;;


(* Suma todos los elementos de la lista, mas el nodo padre *)
let rec num_nodes (GT (_, l)) = List.fold_left (+) 1 (List.map num_nodes l);;

let rec num_nodes' = function
  GT (_, []) -> 1
| GT (x, h::t) -> num_nodes' h + num_nodes' (GT (x, t))
;;


(* PARADIGMA FUNCIONAL TERMINA AQUÍ *)

(* PARADIGMA IMPERATIVO *)

ref;;
(* - : 'a -> 'a ref = <fun> *)
ref 8;;
(* - : int ref = {contents = 8} *)
let i = ref 0;;
(* val i : int ref = {contents = 0} *)
(* i + 3;; *)
(*
Line 1, characters 0-1:
1 | i + 3;;
    ^
Error: This expression has type "int ref"
       but an expression was expected of type "int"
*)
(!);;
(* - : 'a ref -> 'a = <fun> *)
!i;;
(* - : int = 0 *)
!i + 3;;
(* - : int = 3 *)
(:=);;
(* - : 'a ref -> 'a -> unit = <fun> *)
i := 5;;
(* - : unit = () *)
i;;
(* - : int ref = {contents = 5} *)
!i;;
(* - : int = 5 *)

for i = 0+1 to 4+1 do
  print_endline (string_of_int i)
done

let fact n =
  let f = ref 1 in
  for i = 1 to n do
    f := i * !f
  done;
  !f
;;

(* EQUIVALENCIAS DEL PUNTO Y COMA *)

(*
e1 ; e2 ;;

let _ = e1 in e2

e1 ; e2 ; e3 ;;

let _ = e1 in let _ = e2 in e3
*)

let fac n =
  let f = ref 1 and
    i = ref 1 in
  while !i <= n do
    f := !f * !i;
    i := !i + 1
  done;
  !f
;;

(*

let _ = e1 in let _ = e2 in e3

variables (ref, !, :=)
secuencial (;)
alternativa (if-then-else)
bucles (while, for)

*)

let v = Array.init 10 (fun _ -> Random.float 1.);;

let vprod v1 v2 =
  if Array.length v1 = Array.length v2 then
    let p = ref 0. in
    for i = 0 to Array.length v1 do
      p := !p +. v1.(i) *. v2.(i)
    done;
    !p
  else
    raise (Invalid_argument "vprod")
;;

let vprod v1 v2 =
  Array.fold_left (+.) 0. (Array.map2 ( *. ) v1 v2 )
;;

let m =
  [|
    [| 1; 2; 3 |];
    [| 4; 5; 6 |];
    [| 7; 8; 9; 10 |];
  |]
;;

(* CLASES 2/12 *)

type persona = { nombre : string ; edad : int } ;;

type persona = {
    nombre : string;
    mutable edad : int
}

let envejece p = p.edad <- p.edad +1;;

let mas_vieja p =
  { nombre = p.nombre; edad = p.edad + 1}
;;

let mas_vieja p =
  { p with edad = p.edad + 1}
;;

let envejece p =
  p.edad <- p.edad + 1
;;

type 'a ref =
  { mutable contents : 'a}

(*
let ref x =
  { mutable contents : 'a}


let ref x =
  {contens = x}

let (!) v =
  v.contents

let (:=) v x =
  v.contents <- x
*)

