type move = LtoC | LtoR | CtoL | CtoR | RtoL | RtoC 

val hanoi : int -> int list * int list * int list -> int list * int list * int list -> move list

