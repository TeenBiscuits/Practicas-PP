type 'a st_bintree =
  Leaf of 'a
| SBT of 'a st_bintree * 'a * 'a st_bintree

type 'a t = 'a st_bintree

let leaftree x = Leaf x

let is_leaf = function
  Leaf x -> true
| _ -> false

let comb x l r =
  SBT (l, x, r)
(* comb x l r devuelve el árbol con raíz x, rama izda l y rama decha r *)

let root = function
  Leaf x -> x
| SBT (_, x, _) -> x

let left_b = function
  Leaf _ -> raise (Failure "left_b")
| SBT (l, _, _) -> l

let right_b = function
  Leaf _ -> raise (Failure "right_b")
|  SBT (_, _, r) -> r

let root_replacement t x =
  match t with
    Leaf _ -> raise (Failure "root_replacement")
  | SBT (left, _, right) -> SBT (left, x, right)

let left_replacement t l =
  match t with
    Leaf _  -> raise (Failure "left_replacement")
  | SBT (_, root, right) -> SBT (l, root, right)

let right_replacement t r =
match t with
  Leaf _ -> raise (Failure "right_replacement")
| SBT (left, root, _) -> SBT (left, root, r)

let rec size = function
  Leaf _ -> 1
| SBT (left, _, right) -> 1 + size left + size right

let rec height = function
  Leaf _ -> 1
| SBT (left, _, right) -> 1 + max (height left) (height right)
(* altura como número de niveles; 1 si tiene solo un nodo *)


let rec preorder = function
  Leaf x -> [x]
| SBT (left, root, right) -> [root] @ preorder left @ preorder right
(* primero la raíz *)

let rec inorder = function
  Leaf x -> [x]
| SBT (left, root, right) -> inorder left @ [root] @ inorder right
(* la raíz entre las ramas *)

let rec postorder = function
  Leaf x -> [x]
| SBT (left, root, right) -> postorder left @ postorder right @ [root]
(* la raíz al final *)

let breadth a =
  let rec aux = function
      [] -> []
    | Leaf x ::t -> x :: aux t
    | SBT (l,x,r) :: t -> x :: aux (t @ [l;r]) (* ineficiente *)
  in aux [a]

let rec leaves = function
  Leaf x -> [x] 
| SBT (l, _,r) -> leaves l @ leaves r
(* lista de hojas de izda a decha *)

let find_in_depth p t =
  let rec aux = function
    [] -> raise Not_found
  | Leaf x::t -> if p x then x else aux t
  | SBT (l,x,r) :: t -> if p x then x else aux ([l] @ [r] @ t)
  in 
    aux [t]
(* busca en profundidad (priorizando las ramas izquierdas)
   un nodo que satisfaga el predicado.
   Raises Not_found if not found *)

let find_in_depth_opt p t =
  let rec aux = function
    [] -> None
  | Leaf x::t -> if p x then Some x else aux t
  | SBT (l,x,r) :: t -> if p x then Some x else aux ([l] @ [r] @ t)
  in 
    aux [t]

let exists p t =
  let rec aux = function
    [] -> false
  | Leaf x::t -> if p x then true else aux t
  | SBT (l,x,r) :: t -> if p x then true else aux ([l] @ [r] @ t)
  in 
    aux [t]

let for_all p t =
  let rec aux = function
    [] -> true
  | Leaf x::t -> if p x then aux t else false
  | SBT (l,x,r) :: t -> if p x then aux ([l] @ [r] @ t) else false
  in 
    aux [t]

let rec map p = function 
  Leaf x -> Leaf (p x)
| SBT (l,x,r) -> SBT (map p l, p x, map p r)
;;

let rec mirror = function
  Leaf x -> Leaf x
| SBT (l,x,r) -> SBT (mirror r, x, mirror l)
;;

let rec replace_when p t r =
  match t with
    Leaf x -> if p x then r else Leaf x
  | SBT (left,x,right) -> if p x then r else SBT (replace_when p left r,x, replace_when p right r)
(* replace_when p t r es un árbol como el t, pero en el que se han reemplazado
   los nodos que satisfacen p (con todos sus descendientes) por el árbol r *)

let rec cut_below p t =
  match t with
    Leaf x -> Leaf x
  | SBT (left,x,right) -> if p x then Leaf x else SBT (cut_below p left,x, cut_below p right) 
(*
val cut_below : ('a -> bool) -> 'a t -> 'a t 
(* "corta" el árbol por debajo (si la raíz está en la cima)
   de cualquier nodo que satisfaga el predicado *)
*)

let rec to_bin = function
  Leaf x -> BinTree.leaftree x
| SBT(left,x,right) -> BinTree.right_replacement ( BinTree.left_replacement (BinTree.leaftree x) (to_bin left)) (to_bin right)

let rec from_bin t =
	if BinTree.is_empty t then raise (Failure "from_bin")
	else match (BinTree.is_empty (BinTree.left_b t), BinTree.is_empty (BinTree.right_b t) ) with
		(true, true) -> Leaf (BinTree.root t)
		| (false, false) -> SBT (from_bin (BinTree.left_b t), BinTree.root t, from_bin (BinTree.right_b t))
		| (_, _) -> raise (Failure "from_bin");;
