(* #load "binTree.cmo";; *)
(* #load "stBinTree.cmo";; *)

(* o #load_rec "stBinTree.cmo";; *)

open StBinTree;;

let leaf = leaftree;;

let complete_tree n = (* da el mismo valor para (2*i) y (2*i+1) *)
    let rec aux i =
        if 2*i > n then leaf i
        else comb i (aux (2*i)) (aux (2*i+1))
   in aux 1;;

let t9 = complete_tree 9;;

size t9, height t9;;

breadth t9;;

let t9' = mirror t9;;

breadth t9';;

preorder t9;;

inorder t9;;

postorder t9;;

find_in_depth ((<) 3) t9;;

find_in_depth ((<) 3) t9';;

exists ((<) 7) t9;;

exists ((<) 8) t9;;

for_all ((<) 7) t9;;

for_all ((<) 0) t9;;

leaves t9;;

leaves (right_b t9);;

leaves (right_b t9');;

let t9b = map (fun n -> n mod 2 = 0) t9;;

breadth t9b;;

let t9c = map (fun n -> if n mod 2 = 0 then 2 * n else n) t9;;

breadth t9c;;

let tr = left_b (left_b t9);;

breadth tr;;

let t = replace_when ((<) 2) t9 tr;;

breadth t;; 

let ta = cut_below ((<=) 3) t9;;

breadth ta;; 

from_bin (to_bin t9) = t9;;
                   



