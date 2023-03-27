

module Model1 = struct
  let add x y = x + y
  let div x y = x / y
end

module Model2 = struct
  let add x y = x + y
  let div x y =
    if y = 0 then None
    else Some (x / y)

  (* ugly, not composable *)
  let comp x y =
    let x1 = add x y in
    match div x 42 with
    | None -> None
    | Some x2 -> Some (add x1 x2)
end


module Model3 = struct

  let add x y =
    match x,y with
    | None,_ | _, None -> None
    | Some x, Some y -> Some (x + y)

  let div x y =
    match x,y with
    | None,_ | _, None -> None
    | Some x, Some y -> Some (x + y)

  (* composable, but still ugly *)
  let comp x y = add x y |> div (Some 42)

end

(* try to refactor *)
module Model4 = struct

  let wrap f x y =
    match x, y with
    | None,_ | _, None -> None
    | Some x, Some y -> f x y

  let div x y =
    if y = 0 then None
    else Some (x / y)

  let div x y = wrap div x y

  (* problem with add  - need to wrap output in oirder
     to match the interface. Still ugly. But something we
     do pretty good: now we work with int option, i.e. we
     have the same type for arguments and outputs  *)

  let wrap_out f x y = Some (f x y)
  let add x y = wrap (wrap_out ( + )) x y

end

(* refactoring, a better version *)
module Model5 = struct
  let bind x f =
    match x with
    | None -> None
    | Some x -> f x

  let return x = Some x

  let (>>=) = bind

  let add x y =
    return (x + y)

  let div x y = 
    if y = 0 then None
    else return (x / y)

  let comp x y =
    add x y >>= fun x1 ->
    div x1 42 >>= fun x2 ->
    add x2 1
end
