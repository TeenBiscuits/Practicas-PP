type 'a st_bintree = Leaf of 'a | SBT of 'a st_bintree * 'a * 'a st_bintree

type 'a t = 'a st_bintree

val leaftree: 'a -> 'a t

val is_leaf: 'a t -> bool

val comb : 'a -> 'a t -> 'a t -> 'a t
(* comb x l r devuelve el árbol con raíz x, rama izda l y rama decha r *)

val root : 'a t -> 'a

val left_b : 'a t -> 'a t

val right_b : 'a t -> 'a t

val root_replacement : 'a t -> 'a -> 'a t

val left_replacement : 'a t -> 'a t -> 'a t

val right_replacement : 'a t -> 'a t -> 'a t

val size : 'a t -> int 

val height : 'a t -> int 
(* altura como número de niveles; 1 si tiene solo un nodo *)

val preorder : 'a t -> 'a list

val inorder : 'a t -> 'a list

val postorder : 'a t -> 'a list

val breadth : 'a t -> 'a list 

val leaves : 'a t -> 'a list (* lista de hojas de izda a decha *)

val find_in_depth  : ('a -> bool) -> 'a t -> 'a (* raises Not_found if not found *)

val find_in_depth_opt : ('a -> bool) -> 'a t -> 'a option

val exists : ('a -> bool) -> 'a t -> bool

val for_all : ('a -> bool) -> 'a t -> bool

val map : ('a -> 'b) -> 'a t -> 'b t

val mirror : 'a t-> 'a t (* imagen especular *)

val replace_when : ('a -> bool) -> 'a t -> 'a t -> 'a t 

val cut_below : ('a -> bool) -> 'a t -> 'a t 

val to_bin : 'a t -> 'a BinTree.t

val from_bin : 'a BinTree.t -> 'a t 
   (* raises (Failure "from_bin") if tree is empty or not strictly binary *)
       






