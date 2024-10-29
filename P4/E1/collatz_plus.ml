let f n = if n mod 2 = 0 then n / 2 else 3 * n + 1;;
let length'n'top n =
  let rec aux l n =
    if n = 1 then (l+1,1)
    else max (l+1,n) (aux (l+1) (f n))
  in 
   aux 0 n
;;
