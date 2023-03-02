open Printf

(* unit as an argument *)
let hello () = Printf.printf "Hello, world!\n"

(* single-arg function *)
let sum1 x = x + 2  

(* multi-arg functions *)
let sum2 x y = x + y
let sum3 x y z = x + y + z 

(* anonymous function: a value that is assigned to var sum4 *)
let sum4 = fun x y -> x + y

(* an anonymous function that returns an anonymous function ...  *)
let sum5 = fun x -> fun y -> x + y

(* function application *)
let sum6 x y = x + int_of_float y

(* type annotations *)
let sum7 : int -> int -> string = fun x y -> string_of_int (x + y)
let sum8 (x : int) (y : int) = x + y

(* application *)                               
let x = sum1 4 + sum1 5
let y = sum1 (4 + 5) + sum1 5

(* partial application *)
let add : int -> int -> int = fun x y -> x + y

let add : int -> (int -> int) =
  fun x ->
  fun y -> x + y

let add x y = x + y

let add' = add 42 


(* recursive functions *)
let rec fact x =
  if x = 0 then 1
  else x * fact (x - 1)

let rec pow x y =
  if y = 0 then 1
  else x * pow x (y - 1)

(** tail recursive function - with an optional helper *)
let rec pow' r x y =
  if y = 0 then r
  else pow' (r * x) x (y - 1)

let pow x y = pow' 1 x y


(* one more example of tail recursive function *)
let rec count max = 
  if max = 0 then 0
  else 1 + count (max - 1) 

let rec count n max = 
  if max = 0 then n
  else count (n + 1) (max - 1) 


(* mutually recursive functions *)
let rec even x =
  x = 0 || odd (x - 1)
and odd x =
  x <> 0 && even (x - 1)


(* labeled argument  *)
let fused_add ~acc x y = acc + x * y 

let acc = 0
let a = fused_add ~acc 4 5 
let b = fused_add ~acc:0 4 5


(* argument with default value  *)
let add ?(x=42) y = x + y

let a = add 2
let b = add ~x:4 2


(* optional argument *)
let add ?x y = match x with
  | None -> y
  | Some x -> x + y

let a = add 4
let b = add 4 ~x:2


(* high order function, polymorphic function  *) 
let print_length: ('a -> int) -> 'a -> unit =
  fun f x ->
  printf "the length is %d\n" (f x)

let () = print_length String.length "abc"
let () = print_length (fun (_:int) -> Sys.int_size ) 42

(* let's write something long: with many things inside *)
let has_suffix ~suf str =
  let len = String.length in
  let len_str = len str in
  let len_suf = len suf in
  let start = len_str - len_suf in
  let string_at = String.get str in
  let suffix_at = String.get suf in
  let rec compare idx =
    if idx = len_suf then true
    else
    if string_at (start + idx) <> suffix_at idx then
      false
    else compare (idx + 1) in
  start >= 0 && compare 0


let () = printf "test for has suffix: %b, %b\n"
    (has_suffix ~suf:"world" "Hello, world")
    (has_suffix ~suf:"abc" "")


(* prefix operator  *)
let x = (+) 3 4

(* pipe operator *)
let add x y = x + y
let mul x y = x * y
let print r = printf "the result is %d\n" r

let () = print (mul 42 (add 4 5))
let () = add 4 5 |> mul 42 |> print

(* apply operator *)
let () = print @@ mul 42 @@ add 4 5 

let lead num =
  let max = Int.one lsl (Sys.int_size - 1) in
  let rec count n max =
    if num land max <> 0 then n
    else count (n + 1) (max lsr 1) in
  count 0 max

let () = Printf.printf "%d\n" @@ lead 2
