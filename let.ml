


let x = 1

let y : int =
  let w = x + x in
  let z = 42 in
  x + z + w 

let () = Printf.printf "x: %d, y: %d\n" x y

let add x y = x + y

let z = add x y + 4


let y =
  if 3 > 5 then
    "foo"
  else
    "bar"

