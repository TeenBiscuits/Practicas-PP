(* Alogritmo de Euclides - Literalmente implmentando como en Wikipedia *)
let rec mcd a b =
  if b = 0 then a
  else mcd b (a mod b)
;;

(* Versión optimizada *)
let rec mcd' a b =
  if b = 0 then a
  else mcd (min a b) ((max a b) mod (min a b))
;;

