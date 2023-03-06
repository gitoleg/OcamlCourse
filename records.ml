

type t = {
  name : string;
  age  : int;
  salary : float;
}

let ( + ) x y = x.age + y.age

type u = {
  name : string;
  age  : int;
  salary : float;
}

(* Construction - an ERROR - 
   no default values *)
let rick = {age=27; name="Rick"}

(* Construction *)
let john = {name="John"; age=25;
            salary=20_000.0 }

(* Construction - an order does 
   NOT matter *)
let bill = {age=26; name="Bill";
            salary=20_000.0 }

(* Pattern matching examples *)
let name {name} = name

let salary x = match x with
  | {salary} when salary > 2000.0 -> salary +. 2000.0
  | {salary} when salary > 1000.0 -> salary
  | _ -> x.salary

let is_even x = match x with
  | y when y mod 2 = 0 -> true
  | _ -> false

(* Note, it's an exaustive pattern matching  *)
let salary = function
  | {salary} -> salary

(* bind a new name in pattern matching *)
let salary x = match x with
  | {salary=s} -> s

(* Pattern matching with let-binding *)
let default_name () =
  let {name} = john in
  name

let default_name' () =
  let {name=n} as r = john in
  Printf.printf "%d\n" r.age;
  n

(* the best way for fields access  *)
let age x = x.age

(* copy a record. So-caled functional update *)
let promote x upd = {x with salary = x.salary +. upd}


let exper () =
  let {salary} as r = bill in
  Printf.printf "%f\n" salary;
  r

let foo ({name} as x) =
  Printf.printf "%d\n" x.age;
  name

(* Entering Polymorphism *)

type 'a job = {
  value: 'a;
  length: 'a -> int
}

let job1 =
  {value = 42; length = fun _ -> Sys.int_size; }

let job2 =
  {value = "hello, world"; length = String.length}

let print j =
  Printf.printf "length %d" (j.length j.value)

let () =
  print job1;
  print job2
