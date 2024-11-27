(* #load "binTree.cmo";; *)

open BinTree;;

let comb x l r = 
    left_replacement (right_replacement (leaftree x) r) l;;

let leaf = leaftree;;

let complete_tree n = 
    let rec aux i =
        if i > n then empty
        else comb i (aux (2*i)) (aux (2*i+1))
   in aux 1;;

let t8 = complete_tree 8;;

size t8, height t8;;

breadth t8;;

let t8' = mirror t8;;

breadth t8';;

preorder t8;;

inorder t8;;

postorder t8;;

find_in_depth ((<) 3) t8;;

find_in_depth ((<) 3) t8';;

exists ((<) 7) t8;;

exists ((<) 8) t8;;

for_all ((<) 7) t8;;

for_all ((<) 0) t8;;

leaves t8;;

leaves (right_b t8);;

leaves (right_b t8');;

let t8b = map (fun n -> n mod 2 = 0) t8;;

breadth t8b;;

let t8c = map (fun n -> if n mod 2 = 0 then 2 * n else n) t8;;

breadth t8c;;

let tr = left_b (left_b t8);;

breadth tr;;

let t = replace_when ((<) 1) t8 tr;;

breadth t;;

let ta = cut_below ((<=) 3) t8;;

breadth ta;; 

let tb = cut_above ((<=) 3) t8;;
                   
breadth tb;; 



