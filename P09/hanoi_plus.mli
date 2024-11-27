val is_stable : int list * int list * int list -> bool 

val all_states: int -> (int list * int list * int list) list

type move = LtoC | LtoR | CtoL | CtoR | RtoL | RtoC

val move : int list * int list * int list -> move -> int list * int list * int list 

val move_sequence : int list * int list * int list -> move list -> int list * int list * int list

