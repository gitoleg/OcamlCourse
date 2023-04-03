open Printf

class myshape  name =
  let () = printf "pre-init myshape\n" in

  object
    val name = name
    val secret = "secret"

    method print = printf "the name is %s\n" name

    initializer
      printf "init myshape\n"

  end


class mysquare name edge =
  object
    inherit myshape name as super

    val edge = edge
    method print =
      printf "Square with edge %d!\n" edge
  end

class type ['a] mystack_t = object
  val mutable data : 'a list
  method pop : 'a
  method push : 'a -> unit
end

class ['a] mystack : ['a] mystack_t  = object
  val mutable data : 'a list = []

  method pop = match data with
    | [] -> failwith "stack is empty"
    | x :: xs ->
      data <- xs;
      x

  method push x =
    data <- x :: data

end


class int_stack = object(self)
  inherit [int] mystack as super

  method print =
    List.iter (fun x -> printf "%d\n" x) data

  method push x =
    printf "my int stack!\n";
    super#push x
end


class virtual a = object(self)
  method virtual name: string
  method print =
    printf "I am a %s class!\n" self#name 
end

class b = object
  inherit a
  method name = "b"
end 


let () =
  let b = new b in
  b#print

