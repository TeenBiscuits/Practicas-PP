(* NOTA: 10 *)
type 'a st_bintree =
	Leaf of 'a
	| SBT of 'a st_bintree * 'a * 'a st_bintree;;

type 'a t = 'a st_bintree;;

let leaftree x = Leaf x;;

let is_leaf = function
	Leaf _ -> true
	| SBT(_, _, _) -> false;;

let comb x l r = SBT (l, x, r);;

let root = function
	Leaf x -> x
	| SBT (_, x, _) -> x;;

let left_b = function
	Leaf _ -> failwith "left_b"
	| SBT (izq, _, _) -> izq;;

let right_b = function
	Leaf _ -> failwith "right_b"
	| SBT (_, _, der) -> der;;

let root_replacement tree x =
	match tree with
	Leaf _ -> Leaf x
	| SBT (izq, _, der) -> SBT (izq, x, der);;
	
let left_replacement tree izq =
	match tree with
	Leaf _ -> failwith "left_replacement"
	| SBT (_, x, der) -> SBT (izq, x, der);;
	
let right_replacement tree der =
	match tree with
	Leaf _ -> failwith "left_replacement"
	| SBT (izq, x, _) -> SBT (izq, x, der);;

let rec size = function
	Leaf x -> 1
	| SBT (izq, _, der) -> 1 + size izq + size der;;
	
let rec height = function
	Leaf x -> 1
	| SBT (izq, _, der) -> 1 + max(height izq) (height der);;


let rec preorder = function
	Leaf x -> [x]
	| SBT (izq, x, der) -> x :: (preorder izq @ preorder der);;

let rec inorder = function
	Leaf x -> [x]
	| SBT (izq, x, der) -> inorder izq @ (x :: inorder der);;

let rec postorder = function
	Leaf x -> [x]
	| SBT (izq, x, der) -> postorder izq @ postorder der @ [x];;


let breadth tree =
	let rec aux cola acc =
		match cola with
		[] -> List.rev acc
		| Leaf x :: l -> aux l (x :: acc)
		| SBT (izq, x, der) :: l -> aux (l @ [izq; der]) (x :: acc)
	in
	aux [tree] [];;

let rec leaves = function
	Leaf x -> [x]
	| SBT (izq, _, der) -> leaves izq @ leaves der;;


let rec find_in_depth f tree =
	match tree with
	Leaf x -> 
		if f x then x
		else raise Not_found
		| SBT (left, x, right) ->
			if f x then x
			else
			try find_in_depth f left
			with Not_found -> find_in_depth f right;;

let rec find_in_depth_opt f tree =
	match tree with
	Leaf x -> 
		if f x then Some x
		else None
	| SBT (left, x, right) ->
		if f x then Some x
		else
		match find_in_depth_opt f left with
		Some value -> Some value
		| None -> find_in_depth_opt f right;;

let rec for_all f = function
	Leaf x -> f x
	| SBT (izq, raiz, der) ->
		f raiz && for_all f izq && for_all f der;;

let rec exists f = function
	Leaf x -> f x
	| SBT (izq, raiz, der) ->
		f raiz || exists f izq || exists f der;;

let rec map f tree =
	match tree with
	Leaf x -> Leaf (f x)
	| SBT (izq, x, der) -> SBT (map f izq, f x, map f der);;


let rec mirror = function
	Leaf x -> Leaf x
	| SBT (izq, raiz, der) -> SBT (mirror der, raiz, mirror izq);;


let rec replace_when f t r =
	match t with
	Leaf x ->
		if f x then r else t
	| SBT (left, x, right) ->
		if f x then r
		else SBT (replace_when f left r, x, replace_when f right r);;


let rec cut_above f = function
	Leaf x -> Leaf x
	| SBT (izq, x, der) ->
		if f x then Leaf (f x)
		else SBT (cut_above f izq, x, cut_above f der);;

let rec cut_below f = function
  | Leaf x -> Leaf x
  | SBT (left, x, right) ->
      if f x then Leaf x
      else SBT (cut_below f left, x, cut_below f right);;


let rec to_bin tree =
	match tree with
	Leaf x -> BinTree.leaftree x
	| SBT (izq, raiz, der) -> BinTree.right_replacement (BinTree.left_replacement (BinTree.leaftree raiz) (to_bin izq)) (to_bin der);;

let rec from_bin t =
	if BinTree.is_empty t then raise (Failure "from_bin")
	else match (BinTree.is_empty (BinTree.left_b t), BinTree.is_empty (BinTree.right_b t) )
		with
		(true, true) -> Leaf (BinTree.root t)
		| (false, false) -> SBT (from_bin (BinTree.left_b t), BinTree.root t, from_bin (BinTree.right_b t))
		| (_, _) -> raise (Failure "from_bin");;
