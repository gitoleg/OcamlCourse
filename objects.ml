open Printf

let opt =
  object(self)
    val mutable data : int option = None
    method is_opt = data = None
    method data = data
    method update x = data <- Some x
  end

let to_str x =
  match x#data with
  | None -> printf "None\n"
  | Some x -> printf "Some %d\n"  x


let circle x =
  object 
    val radius = x
    method radius = radius
    method area = Float.pi *. float(radius) *. float(radius)
  end

let square x =
  object (self : 'self)
    val edge = x   
    method edge = edge
    method area = float(edge * edge)
    method update dx : 'self = {< edge = edge + dx>}
    method twice = self#update edge

  end


let print_area (o : <area : float; ..>) =
  printf "area is %g\n" o#area

let () =
  let s = square 42 in
  let c = circle 34 in
  print_area s;
  print_area c


type area = < area : float>

let xs = [(circle 3 :> area); (square 3 :> area)]


