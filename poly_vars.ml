open Printf


type t = [ `A
         | `B of int
         | `C of float ]

let f = function
  | `A -> "a"
  | `B x -> "b " ^ string_of_int x
  | `C x -> "c " ^ string_of_float x 

type cmp = [ `Eq | `Less | `Greater ]
type eq = [ `Eq ]

let to_str : cmp -> string = function
  | `Less -> "<"
  | `Eq -> "="
  | `Greater -> ">"

let eq x y : eq option =
  if x = y then Some `Eq
  else None

let f x y = match eq x y with
  | None -> ""
  | Some x -> to_str (x :> cmp) 


type a = [ `A | `B ]
type b = [ a | `C ]

let f = function
  | `C -> "type b"
  | #a -> "type a"


let f = function
  | `A | `B -> `C
  | x -> x


let x1 = f `A
let x2 = f `E
