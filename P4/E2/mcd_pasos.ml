(* HE APRENDIDO A AÑADIR ACUMULADORES *)
let mcd_pasos a b =
  let rec aux a b n =
    if b = 0 then (a,n+1)
    else aux (min a b) ((max a b) mod (min a b)) (n+1)
  in
    aux a b 0
;;