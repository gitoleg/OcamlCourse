


type shape =
  | Rect of { name : string } 
  | Circle of float 
  | Square of float
  | Triangle of float * float * float

let area = function
  | Rect {name=a}-> Printf.printf "%s\n" a;  52.0
  | Circle r -> Float.pi *. r *. r
  | _ -> 42.0

type coord = float * string
type shape' = coord * shape

type colored_shape = { color : string; shape : shape;  }

let color shape = shape.color
