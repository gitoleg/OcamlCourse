


type color = string

type fruit = | Apple of color
             | Banana
             | Cherry
             | Dried of fruit


(* Pattern matching *)
let is_banana x = match x with
  | Banana -> true
  | _ -> false

let rec to_string = function
  | Apple c -> c ^ " Apple"
  | Banana -> "Banana"
  | Cherry -> "Cherry"
  | Dried f -> "Dried " ^ to_string f 





