open Printf

let make_iota () = 
  let x = ref 0 in
  let next () = 
    let y = !x in
    x := !x + 1;
    y in
  next

let iota = make_iota() 

let rec foo x =
  if x < 10 then
    begin
      printf "%d\n" (iota ());
      foo (x + 1)
    end

let () = foo 0

module MyRef = struct

  type 'a t = {
    mutable data : 'a;
  }

  let myref x = {data = x }

  let (:=) x y = x.data <- y
  let (!) x = x.data

end


let x = ref None


let a = [|1;2;3;4;5|]
let () = a.(0) <- 42

let to_string s a = match a with
  | [||] -> "empty"
  | [|x|] -> sprintf "single %d" x
  | _ -> sprintf "very big array"


