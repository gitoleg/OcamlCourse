

module type ListT = sig

  type 'a t

  val empty : 'a t
  val add : 'a -> 'a t -> 'a t
  val head : 'a t -> 'a
end

module ListBasic  = struct 

  type 'a t =
    | Empty
    | Cons of 'a * 'a t 

  let empty = Empty
  let add x t = Cons (x,t)

  let head = function
    | Empty -> failwith "list is empty"
    | Cons (x, _) -> x
end

module MyList = struct
  include ListBasic
  include List

  module Debug = struct
    let print pp xs =
      let open Printf in
      let rec print = function
        | Cons (x, xs) -> printf "%a " pp x; print xs
        | Empty -> printf "\n" in
      print xs 
  end
end

(* Submodule usage  *)
let x = MyList.(add 4 empty)
let pp ch x = Printf.fprintf ch "%d" x
(* let () = MyList.Debug.print pp x *)

