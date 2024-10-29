val from0to : int -> int list
(* from0to n = [0; 1; 2; ... ; n] 
   provoca una excecpción Failure si n < 0 *)

val to0from : int -> int list
(* to0from n  = [n; n-1; ... ; 0]
   provoca una excecpción Failure si n < 0 *)

val pair : 'a -> 'b list -> ('a * 'b) list
(* pair x [y1; y2; ...; yn] = [(x, y1; (x, y2); ... ; (x, yn)] *)

val remove_first : 'a -> 'a list -> 'a list
(* remove_first x l devuelve una lista como la l,
   pero sin la primera aparición del valor x.
   Si el valor x no aparece en l, devuelve l *)

val remove_all : 'a -> 'a list -> 'a list
(* remove_all x l devuelve una lista como la l,
   en la que han desaparecido todas las apariciones del valor x *)

val ldif : 'a list -> 'a list -> 'a list
(* ldif l1 l2 devuelve una lista como l1,
   pero de la que han desaparecido todas las apariciones 
   de los valores que aparecen el l2 *)
