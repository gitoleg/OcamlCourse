

type t = string * int * float

type t' = string * int

type age = int

let x = 42

type man = {
  name : string;
  age : age;
}

(* Construction - just a pair, the infered type 
   is string * int  *)
let john = "John", 25

(* Pretty the same - now with a type annotation
   which is just an alias *)
let rick : t' = "Rick", 25

(* An error!!! the position DOES matter *)
let bill : t' = 25, "Bill"

let dave : t = "Dave", 30, 30_000.0

(* ONLY for tuples of two *)
let pair_access pair =
  let x = fst pair in
  let y = snd pair in
  Printf.printf "Guy is %s %d\n" x y 

(* Pattern matching examples.
   Note, the infered type is 
   'a * 'b -> 'a *)
let name = function
  | name, _ -> name

(* The infered type is t' -> string *)
let name (x : t') = match x with
  | name, _ -> name

let salary () =
  let (_, _, s) = dave in
  s



