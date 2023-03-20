

module MyBaseList = struct

  type 'a t =
    | Nil
    | Cons of 'a * 'a t 

  let empty = Nil
  let add x xs = Cons (x,xs)

  let next = function
    | Nil -> None
    | Cons (x,xs) -> Some (x,xs)

end

module MyBaseStack = struct

  type 'a t = 'a list

  let empty = []
  let push x xs = x :: xs
  let pop = function
    | [] -> failwith "stack is empty"
    | x :: xs -> xs

  let top = function
    | [] -> failwith "stack is empty"
    | x :: _ -> x

  let next = function
    | [] -> None
    | x :: xs -> Some (x,xs)
end

module type Base = sig
  type 'a t
  val next : 'a t -> ('a * 'a t) option
end

module Make(B : Base) = struct
  include B

  let rec len xs = match next xs with
    | None -> 0
    | Some (x,xs) -> 1 + len xs

  let rec iter f xs = match next xs with
    | None -> ()
    | Some (x,xs) -> f x; iter f xs
end

module MyList = struct
  include MyBaseList
  include Make(MyBaseList)
end

module MyStack = struct
  include MyBaseStack
  include Make(MyBaseStack)
end


module type T = sig
  type t
  val x : t
end

module A = struct
  type t = int
  let x = 42
end

module B = struct
  type t = float
  let x = 36.6
end

module Pair(X : T)(Y : T) = struct
  type t = X.t * Y.t
  let x = X.x, Y.x
end

module Pair = Pair(A)(struct
    type t = float
    let x = 36.6
  end)

