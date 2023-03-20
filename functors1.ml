

module type T = sig
  type t
  val x : t
end

module MakePair(A : T)(B : T) : T = struct
  type t = A.t * B.t
  let x = A.x, B.x
end

module Make = functor (A : T)(B : T) -> struct
  type t = A.t * B.t
end

module A = struct
  type t = int
  let x = 42
end

module B = struct
  type t = float
  let x = 32.1
end

module C = MakePair(A)(B)


let () = Printf.printf "%d\n" (fst C.x)
