let otro ori des =
    (* 
    Ya se que no es extausivo pero por definición:
     - ori != des
     - ori && des = [1,2,3]
    *)
    match (ori + des) with
        5 -> 1 | 4 -> 2 | 3 -> 3
;;

let move (ori, des) =
    (* 
    Ya se que no es extausivo pero por definición:
     - ori != des
     - ori && des = [1,2,3]
    *)
    let flecha ori des =
    if ori > des then
        match (ori + des) with
        5 -> "   2 <-"
        | 4 -> "<--2---"
        | 3 -> "<- 2   "
    else
        match (ori + des) with
        5 -> "   2 ->"
        | 4 -> "---2-->"
        | 3 -> "-> 2   "
    in
    " 1 " ^ flecha ori des ^ " 3 \n"
;;

let rec hanoi n ori des = 
    (* n número de discos, 1 <= ori <= 3, 1 <= dest <= 3, ori <> des *)
    if n = 0 then "" else
    let otro = otro ori des in
    hanoi (n-1) ori otro ^ move (ori, des) ^ hanoi (n-1) otro des
    
let hanoi n ori des =
    if n = 0 || ori = des then "\n"
    else hanoi n ori des    
       
let print_hanoi n ori des =
    if n < 0 || ori < 1 || ori > 3 || des < 1 || des > 3
       then print_endline  " ** ERROR ** \n"
       else print_endline (" =========== \n" ^ 
                           hanoi n ori des ^
                           " =========== \n")

let crono f x = 
    let t = Sys.time () in
    f x; Sys.time () -. t

(*
let sumar (a,b,c) (d,e,f) = (a+d,b+e,c+f)

let rec nth_hanoi_move n nd ori des =
    let cnt = 0 in
        let rec hanoi n ori des =
            if n = 0 then (1,0,0)
            else if cnt = nd then (1,ori,des)
            else let otro = otro ori des in 
            sumar (hanoi (n-1) ori otro) (hanoi (n-1) otro des)
        in
            hanoi nd ori des
;;
*)
