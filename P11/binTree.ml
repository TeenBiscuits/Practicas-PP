type 'a bintree = Empty | BT of 'a bintree * 'a * 'a bintree

type 'a t = 'a bintree
(* el tipo para representar árboles binarios con 
   nodos etiquetados con valores de tipo 'a *)

let empty = Empty
(* el árbol vacío *)

let is_empty = function
  Empty -> true
| _ -> false

let leaftree nodo =
  BT (Empty, nodo, Empty)
(* árbol con un sólo nodo  (árbol hoja) *)

let root = function
  BT (_, x, _) -> x
| Empty -> raise (Failure "root")
(* etiqueta o valor de la raíz 
   raises Failure "root" if is empty *)


let left_b = function
  Empty -> raise (Failure "left_b") 
| BT (left, _, _) -> left
(* rama izda. 
   raises Failure "left_b" if is empty *)

let right_b = function
  Empty -> raise (Failure "right_b") 
| BT (_, _, right) -> right
(* rama decha 
   raises Failure "right_b" if is empty *)

let root_replacement t x =
match t with
  Empty -> raise (Failure "root_replacement")
| BT (left, _, right) -> BT (left, x, right)
(* root_replacement t x es un árbol con las mismas ramas que t y 
    la raíz etiquetada con el valor x 
    raises Failure "root_replacement" if empty *)

let left_replacement t l =
match t with
  Empty -> raise (Failure "left_replacement")
| BT (_, root, right) -> BT (l, root, right)
(* left_replacement t l es un árbol con la misma raíz y rama dcha que t y
    rama izda l 
    raises Failure "left_replacement" if empty *)


let right_replacement t r =
match t with
  Empty -> raise (Failure "right_replacement")
| BT (left, root, _) -> BT (left, root, r)
(* right_replacement t r es un árbol con la misma raíz y rama izda que t y
    rama dcha r 
    raises Failure "right_replacement" if empty *)

let rec size = function
  Empty -> 0
| BT (left, _, right) -> 1 + size left + size right
(* número de nodos *)

let rec height = function
  Empty -> 0
| BT (left, _, right) -> 1 + max (height left) (height right)
(* altura; 0 para el árbol vacío; 1 si tiene solo un nodo *)

let rec preorder = function
  Empty -> []
| BT (left, root, right) -> [root] @ preorder left @ preorder right
(* primero la raíz *)

let rec inorder = function
  Empty -> []
| BT (left, root, right) -> inorder left @ [root] @ inorder right
(* la raíz entre las ramas *)

let rec postorder = function
  Empty -> []
| BT (left, root, right) -> postorder left @ postorder right @ [root]
(* la raíz al final *)

let complete_tree n =
  let rec aux i =
    if i > n then Empty
    else BT (aux (2*i), i, aux (2*i+1))
  in aux 1

(* POR TERMINAR *)
let breadth a =
  let rec aux = function
      [] -> []
    | Empty::t -> aux t
    | BT (l,x,r) :: t -> x :: aux (t @ [l;r]) (* ineficiente *)
  in aux [a]
(* enumeración de los nodos del árbol recorrido por niveles de izda a dcha ("en anchura") *)

let rec leaves = function
  Empty -> [] 
  (*  Esto es innecesario, porque no se va a dar el 
      caso pero si no es un weak pattern matching *)
| BT (Empty, x, Empty) -> [x]
| BT (l, _,r) -> leaves l @ leaves r
(* lista de hojas de izda a decha *)

(* val find_in_depth : ('a -> bool) -> 'a t -> 'a *)
let find_in_depth p t =
  let rec aux = function
    [] -> raise Not_found
  | Empty::t -> aux t
  | BT (l,x,r) :: t -> if p x then x else aux ([l] @ [r] @ t)
  in 
    aux [t]
(* busca en profundidad (priorizando las ramas izquierdas)
   un nodo que satisfaga el predicado.
   Raises Not_found if not found *)

let find_in_depth_opt p t =
  let rec aux = function
    [] -> None
  | Empty::t -> aux t
  | BT (l,x,r) :: t -> if p x then Some x else aux ([l] @ [r] @ t)
  in 
    aux [t]

let exists p t =
  let rec aux = function
    [] -> false
  | Empty::t -> aux t
  | BT (l,x,r) :: t -> if p x then true else aux ([l] @ [r] @ t)
  in 
    aux [t]

let for_all p t =
  let rec aux = function
    [] -> true
  | Empty::t -> aux t
  | BT (l,x,r) :: t -> if p x then aux ([l] @ [r] @ t) else false
  in 
    aux [t]

let rec map p = function 
  Empty -> Empty
| BT (l,x,r) -> BT (map p l, p x, map p r)
;;

let rec mirror = function
  Empty -> Empty
| BT (l,x,r) -> BT (mirror r, x, mirror l)
;;

let rec replace_when p t r =
  match t with
    Empty -> Empty
  | BT (left,x,right) -> if p x then r else BT (replace_when p left r,x, replace_when p right r)
(* replace_when p t r es un árbol como el t, pero en el que se han reemplazado
   los nodos que satisfacen p (con todos sus descendientes) por el árbol r *)

let rec cut_above p t =
match t with
  Empty -> Empty
| BT (left,x,right) -> if p x then Empty else BT (cut_above p left,x, cut_above p right) 
(*
val cut_above : ('a -> bool) -> 'a t -> 'a t 
(* cut_above p t  es un árbol como el t, pero en el que se han eliminado
   todos los nodos que satisfacen p (con todos sus descendientes) *)
*)

let rec cut_below p t =
  match t with
    Empty -> Empty
  | BT (left,x,right) -> if p x then BT(Empty,x,Empty) else BT (cut_below p left,x, cut_below p right) 
(*
val cut_below : ('a -> bool) -> 'a t -> 'a t 
(* "corta" el árbol por debajo (si la raíz está en la cima)
   de cualquier nodo que satisfaga el predicado *)
*)