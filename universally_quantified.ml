open Printf



type 'a t = 
  | List of 'a list
  | Nested of 'a list t 

module Problems = struct 

  (* High order polymorphic functions  *)

  let len f x y = f x + f y

  let len: ('a -> int) -> 'a -> 'b =
    fun f x y -> f x + f y
  (* no 'b: the type is: ('a -> int) -> 'a -> 'a -> int *)

  let x = len List.length [1] ['c']

  (* recursive polymorphic functions *)

  let rec depth = function
    | List _ -> 1
    | Nested x -> 1 + depth x

end

module Solution = struct

  type f = { f: 'a. 'a list -> int }

  let len x = x.f [1;2;3] + x.f ['a'; 'b'; 'c']

  let () = printf "len %d\n" @@ len {f = List.length}

  let rec depth: 'a. 'a t -> int = function
    | List _ -> 1
    | Nested x -> 1 + depth x

end
