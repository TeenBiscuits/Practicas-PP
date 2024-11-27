
(* o #load_rec "gTree.cmo";; *)

open GTree;;

let leaf x = GT (x, []);;

let t5 = GT (5, [leaf 12]);;

let t3 = GT (3, [t5; leaf 6; leaf 7]);;

let t4 = GT (4, [leaf 8; leaf 9; leaf 10; leaf 11]);;

let t2 = GT (2, [t3; t4]);;

let t1 = GT (1, [t2]);;

size t1, height t1;;

breadth t1;;

let t1' = mirror t1;;

breadth t1';;

preorder t1;;

postorder t1;;

leaves t1;;

leaves t1';;

find_in_depth ((<) 3) t1;;

find_in_depth ((<) 3) t1';;

find_in_depth_opt ((<) 11) t1';;

find_in_depth_opt ((<) 12) t1';;

breadth_find ((<) 4) t1;;

breadth_find ((<) 4) t1';;

breadth_find_opt ((<) 11) t1';;

breadth_find_opt ((<) 12) t1';;

exists ((<) 11) t1;;

exists ((<) 12) t1;;

for_all ((<) 7) t1;;

for_all ((<) 0) t1;;

for_all ((<) 4) t1;;

for_all ((<) 4) t5;;

let t1c = map (fun n -> char_of_int (96 + n)) t1;;

breadth t1c;;

let t = replace_when (fun i -> i mod 3 = 0) t1 (GT (0, [leaf (-1)]));;

breadth t;; 

leaves t;;

let ta = cut_below (fun i -> i mod 3 = 0) t1;;

breadth ta;; 

let stl = StBinTree.leaftree;;
                   
let comb = StBinTree.comb;;

let s1 = comb 1 (stl 2) (stl 3);;

let s4 = comb 4 (stl 5) (stl 6);;

let s = comb 0 s1 s4;;

breadth (from_bin (StBinTree.to_bin s));;

breadth (from_stbin s);;



