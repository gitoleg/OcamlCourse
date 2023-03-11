

module MyBaseList = struct

  type 'a t =
    | Nil
    | Cons of 'a * 'a t 

  let empty = Nil
  let add x xs = Cons (x,xs)

  let rec len = function
    | Nil -> 0
    | Cons (_, xs) -> 1 + len xs

  let rec iter f = function
    | Nil -> ()
    | Cons (x,xs) -> f x; iter f xs


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

  let rec len = function
    | [] -> 0
    | _ :: xs -> 1 + len xs

  let rec iter f = function
    | [] -> ()
    | x :: xs -> f x; iter f xs

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
    | Some (_, xs) -> 1 + len xs

  let rec iter f xs = match next xs with
    | None -> ()
    | Some (x, xs) -> f x; iter f xs
end


module MyListT = struct

  type 'a t =
    | Nil
    | Cons of 'a * 'a t 

  let empty = Nil
  let add x xs = Cons (x,xs)

  let next = function
    | Nil -> None
    | Cons (x, xs) -> Some (x, xs)

end

module MyList = Make(MyBaseList)
module MyStack = Make(MyBaseStack)



