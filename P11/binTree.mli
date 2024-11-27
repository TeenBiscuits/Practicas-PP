type 'a t 
(* el tipo para representar árboles binarios con 
   nodos etiquetados con valores de tipo 'a *)

val empty : 'a t
(* el árbol vacío *)

val is_empty : 'a t -> bool

val leaftree : 'a -> 'a t
(* árbol con un sólo nodo  (árbol hoja) *)

val root : 'a t -> 'a
(* etiqueta o valor de la raíz 
   raises Failure "root" if is empty *)

val left_b : 'a t -> 'a t
(* rama izda. 
   raises Failure "left_b" if is empty *)

val right_b : 'a t -> 'a t
(* rama decha 
   raises Failure "right_b" if is empty *)


val root_replacement : 'a t -> 'a -> 'a t
(* root_replacement t x es un árbol con las mismas ramas que t y 
   la raíz etiquetada con el valor x 
   raises Failure "root_replacement" if empty *)

val left_replacement : 'a t -> 'a t -> 'a t
(* left_replacement t l es un árbol con la misma raíz y rama dcha que t y
   rama izda l 
   raises Failure "left_replacement" if empty *)


val right_replacement : 'a t -> 'a t -> 'a t
(* right_replacement t r es un árbol con la misma raíz y rama izda que t y
   rama dcha r 
   raises Failure "right_replacement" if empty *)


val size : 'a t -> int (* número de nodos *)

val height : 'a t -> int (* altura; 0 para el árbol vacío; 1 si tiene solo un nodo *)

val preorder : 'a t -> 'a list
(* primero la raíz *)

val inorder : 'a t -> 'a list
(* la raíz entre las ramas *)

val postorder : 'a t -> 'a list
(* la raíz al final *)

val breadth : 'a t -> 'a list 
(* enumeración de los nodos del árbol recorrido por niveles de izda a dcha ("en anchura") *)

val leaves : 'a t -> 'a list 
(* lista de hojas de izda a decha *)

val find_in_depth : ('a -> bool) -> 'a t -> 'a 
(* busca en profundidad (priorizando las ramas izaquierdas)
   un nodo que satisfaga el predicado.
   Raises Not_found if not found *)

val find_in_depth_opt : ('a -> bool) -> 'a t -> 'a option

val exists : ('a -> bool) -> 'a t -> bool

val for_all : ('a -> bool) -> 'a t -> bool

val map : ('a -> 'b) -> 'a t -> 'b t

val mirror : 'a t -> 'a t (* imagen especular *)

val replace_when : ('a -> bool) -> 'a t -> 'a t -> 'a t 
(* replace_when p t r es un árbol como el t, pero en el que se han reemplazado
   los nodos que satisfacen p (con todos sus descendientes) por el árbol r *)

val cut_above : ('a -> bool) -> 'a t -> 'a t 
(* cut_above p t  es un árbol como el t, pero en el que se han eliminado
   todos los nodos que satisfacen p (con todos sus descendientes) *)

val cut_below : ('a -> bool) -> 'a t -> 'a t 
(* "corta" el árbol por debajo (si la raíz está en la cima)
   de cualquier nodo que satisfaga el predicado *)







