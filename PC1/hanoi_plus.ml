type move = LtoC | LtoR | CtoL | CtoR | RtoL | RtoC ;;

let hanoi n l l =
  [LtoC; LtoR; CtoL; CtoR; LtoR; CtoL; RtoC; RtoL; CtoL; RtoC]
;;
let tests x =
  if  x = 1 then
  ([LtoC; LtoR; CtoL; CtoR; LtoR; CtoL; RtoC; RtoL; CtoL; RtoC] = hanoi 4 ([1;3], [2; 4], []) ([1;2;4], [3], []))
  else invalid_arg "Test Desconocido";;
;;

(* Definición del tipo de movimientos permitidos *)
type move = LtoC | LtoR | CtoL | CtoR | RtoL | RtoC

(* Excepción para estados no válidos *)
exception Invalid_argument

(* Función auxiliar para verificar si un estado es estable *)
let es_estado_estable n (izq, cen, der) =
  let es_ordenado lst = List.sort compare lst = lst in
  let sin_repetidos lst = List.length lst = List.length (List.sort_uniq compare lst) in
  let todos_discos = List.concat [izq; cen; der] in
  List.for_all es_ordenado [izq; cen; der] &&
  sin_repetidos todos_discos &&
  List.for_all (fun x -> x >= 1 && x <= n) todos_discos

(* Función para mover un disco de un poste a otro *)
let mover origen destino =
  match origen, destino with
  | "L", "C" -> LtoC
  | "L", "R" -> LtoR
  | "C", "L" -> CtoL
  | "C", "R" -> CtoR
  | "R", "L" -> RtoL
  | "R", "C" -> RtoC
  | _ -> raise Invalid_argument

(* Función principal hanoi *)
let hanoi n inicial final =
  (* Verificar estados válidos *)
  if not (es_estado_estable n inicial && es_estado_estable n final) then
    raise Invalid_argument
  else
    (* Definición de la solución recursiva *)
    let rec resolver_hanoi m (ini_izq, ini_cen, ini_der) (fin_izq, fin_cen, fin_der) (aux_izq, aux_cen, aux_der) movimientos origen destino aux =
      if m = 0 then movimientos
      else
        (* Movemos los discos superiores de origen a auxiliar usando destino como auxiliar *)
        let movimientos1 = resolver_hanoi (m - 1) (ini_izq, ini_cen, ini_der) (aux_izq, aux_cen, aux_der) (fin_izq, fin_cen, fin_der) movimientos origen aux destino in
        (* Agregar el movimiento para trasladar el disco superior de origen a destino *)
        let nuevo_mov = mover origen destino in
        let movimientos2 = nuevo_mov :: movimientos1 in
        (* Movemos los discos desde auxiliar a destino usando origen como auxiliar *)
        resolver_hanoi (m - 1) (aux_izq, aux_cen, aux_der) (fin_izq, fin_cen, fin_der) (ini_izq, ini_cen, ini_der) movimientos2 aux destino origen
    in
    resolver_hanoi n inicial final ([], [], []) [] "L" "R" "C"


