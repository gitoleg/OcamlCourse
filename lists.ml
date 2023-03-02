let rec sum xs =
  match xs with
  | [] -> 0
  | x :: xs -> x + sum xs

let is_empty xs =
  match xs with
  | [] -> true
  | _ -> false
(* OR *)
let is_empty xs = xs = []

let f = fun x -> x + 1

let rec map f xs =
  match xs with
  | [] -> []
  | x :: xs -> f x :: map f xs

let rec map2 f xs ys =
  match xs,ys with
  | x :: xs, y :: ys -> f x y :: map2 f xs ys
  | [], [] -> []
  | _ -> invalid_arg "oops"

let start_from_two = function
  | 2 :: _ -> true
  | _ -> false


let rec length xs =
  match xs with
  | [] -> 0
  | _ :: xs -> 1 + length xs


let test_length xs n =
  match length xs with
  | 0 -> false
  | n -> true

let rec length = function
  | [] -> 0
  | x :: xs -> 1 + length xs


let rec fold acc f xs =
  match xs with
  | [] -> acc
  | x :: xs -> fold (f acc x) f xs


let test_fold xs =
  let open Printf in
  let f x acc =
    printf "current %d\n" x;
    List.iter (fun y -> printf "%d " y) acc ;
    printf "\n";
    x :: acc in
  List.fold_right f xs []

