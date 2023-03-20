open Printf

module type T = sig
  type t
  val default : t
  val print : t -> unit
end

module IntT = struct
  type t = int
  let default = 42
  let print = printf "%d\n" 
end

let printer m =
  let module I = (val m : T) in
  I.print I.default

let print () =
  let m = (module IntT : T) in
  printer m


let int_printer m =
  let module M = (val m : T with type t = int) in
  M.print M.default

let buggy_printer m x =
  let module M = (val m : T) in
  M.print x

(* locally abstract type  *)
let polyprinter (type a)
    (module M : T with type t = a) (x : a) =
  M.print x

let foo: 'a -> 'b = fun x -> x + 1
let foo (type a) (x : a) = x + 1

(* Universally quantified type  *)
let polyprinter: type a.
  (module T with type t = a) -> a -> unit =
  fun m x ->
  let module M = (val m : T with type t = a) in
  M.print x

(* an anonymous module *)
let print_list () =
  let list = [(module IntT : T); (module struct
                type t = float
                let default = 52.4
                let print = printf "%g"
              end)] in
  List.iter printer list


(* "How to" for several locally abstract types*)
module type T2 = sig
  type t
  type u

  val default : t
  val print : t -> u -> t
end


let polyprinter2 (type a b)
    (module M : T2 with type t = a
                    and type u = b)
    (x : a)
    (y : b) =
  M.print x y



let anonymous () =
  let m = (module struct
    type t = int
    let default = 42
    let print = printf "%d"
  end : T with type t = int) in
  polyprinter m 42
