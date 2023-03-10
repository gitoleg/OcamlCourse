
module type ListT = sig

  type 'a t

  val empty : 'a t
  val add : 'a -> 'a t -> 'a t
  val head : 'a t -> 'a
end


module MyList : ListT = struct

  type 'a t =
    | Cons of 'a * 'a t

  let empty = failwith ""
  let add x t = Cons (x,t)

  let head = function  
    | Cons (x, _) -> x


end

(* Basic usage *)
let x = MyList.(Cons (42, Empty))
let x = MyList.empty
let x = MyList.add 4 MyList.empty
let x = MyList.(add 4 empty)


(* Brings all from your module 
   to the function scope *)
let create n =
  let open MyList in
  let rec myadd x xs =
    if x = n then xs
    else myadd (x + 1) (add x xs) in
  myadd 0 empty

(* Brings all from your module 
   to the module scope *)
open MyList



