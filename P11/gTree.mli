type 'a gtree = GT of 'a * 'a gtree list

type 'a t = 'a gtree

val leaftree : 'a -> 'a t

val root : 'a t -> 'a

val branches : 'a t -> 'a t list

val size : 'a t -> int 

val height : 'a t -> int 
(* altura como número de niveles; 1 si tiene solo un nodo *)

val preorder : 'a t -> 'a list

val postorder : 'a t -> 'a list

val breadth : 'a t -> 'a list 

val leaves : 'a t -> 'a list 

val find_in_depth : ('a -> bool) -> 'a t -> 'a 

val find_in_depth_opt : ('a -> bool) -> 'a t -> 'a option

val breadth_find :  ('a -> bool) -> 'a t -> 'a 

val breadth_find_opt :  ('a -> bool) -> 'a t -> 'a option

val exists : ('a -> bool) -> 'a t -> bool

val for_all : ('a -> bool) -> 'a t -> bool

val map : ('a -> 'b) -> 'a t -> 'b t

val mirror : 'a t -> 'a t 

val replace_when : ('a -> bool) -> 'a t -> 'a t -> 'a t 

val cut_below : ('a -> bool) -> 'a t -> 'a t 

val from_bin : 'a BinTree.t -> 'a t 
(* no inyectiva (no reversible) *)
(* raises Failure "from_bin" if empty *)

val from_stbin : 'a StBinTree.t -> 'a t
(* raises Failure "from_bin" if empty *)
       
