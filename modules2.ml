

module type ListT = sig
  type 'a t  =
    | Empty
    | Cons of 'a * 'a t

  val empty : 'a t
  val add : 'a -> 'a t -> 'a t
  val head : 'a t -> 'a
end

module type ListInterface = sig
  type 'a t
  val empty : 'a t
  val add : 'a -> 'a t -> 'a t
  val head : 'a t -> 'a
end

(* let's define interface one more time  *)
module type ListT = sig

  type 'a t  =
    | Empty
    | Cons of 'a * 'a t

  include ListInterface with type 'a t := 'a t

  module Debug : sig
    val print :
      (out_channel -> 'a -> unit) -> 'a t -> unit
  end

end


module MyList : ListT = struct

  type 'a t =
    | Empty
    | Cons of 'a * 'a t

  let empty = Empty
  let add x t = Cons (x,t)

  let head = function
    | Empty -> failwith "list is empty"
    | Cons (x, _) -> x

  module Debug = struct
    let print pp xs =
      let open Printf in
      let rec print = function
        | Cons (x, xs) -> printf "%a " pp x; print xs
        | Empty -> printf "\n" in
      print xs 
  end

end

(* Abstract interface *)
(* let's define interface again *)
module type ListT = sig
  include ListInterface
end


module StandardList : ListT = struct
  type 'a t = 'a list

  let add x xs = x :: xs
  let empty = []
  let head = List.hd
end


module MyList : ListT = struct

  type 'a t =
    | Empty
    | Cons of 'a * 'a t

  let empty = Empty
  let add x t = Cons (x,t)

  let head = function
    | Empty -> failwith "list is empty"
    | Cons (x, _) -> x
end

let x = MyList.Empty
