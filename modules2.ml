
module type ListInterface = sig
  type 'a t
  type u
  val empty : 'a t
  val add : 'a -> 'a t -> 'a t
  val head : 'a t -> 'a
end

module type ListT = sig

  type 'a t
  type z

  include ListInterface with type 'a t := 'a t and type u := z

  module Debug : sig
    val print :
      (out_channel -> 'a -> unit) -> 'a t -> unit
  end
end


module MyList : ListT = struct

  type 'a t =
    | Empty
    | Cons of 'a * 'a t

  type z

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



(* Type substituting and type constraint  *)
module type Comparable = sig
  type t
  val compare : t -> t -> int
end

module type Printable = sig
  type t
  val print : out_channel -> t -> unit
end


module type CompareableAndPrintable = sig
  include Comparable
  include Printable with type t := t
end


module type Both = sig
  include Comparable with type t = int 
  include Printable with type t := t 
end

module F : Both = struct
  type t = int
  let compare = Int.compare
  let print ch t = Printf.fprintf ch "%d\n" t
end


(* module substitution in module types *)
module type XY = sig
  type x
  type y
end

module type T = sig
  module A : XY
end

module B = struct
  type x = int
  type y = float
end

module type T1 = T with module A = B 


(* module types substitution in module types *)
module type T = sig
  module type A
end

module type B = sig
  type x = int
  type y = float
end

module type T2 = T with module type A = B
