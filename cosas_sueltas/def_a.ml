let pi = 3.1416

let e = 2.7182

let max_int_f = 4611686018427387903.

let per x = 2. *. pi *. x

let area x = pi *. x *. x

let next_char x = char_of_int(int_of_char(x) + 1)

let absf x = if x < 0. then x *. -1. else x

(*let odd = function x -> if x mod 2 = 1 then true else false*) (* Tremenda redundancia *)
let odd x = x mod 2 = 1

let next5mult x = if x mod 5 = 0 then x + 5 else if x < 0 then x - (x mod 5) else x + (5 - (x mod 5))

let is_letter x = (int_of_char x > 64) = (int_of_char x < 123)

let string_of_bool x = if x = true then "verdadero" else "falso"

