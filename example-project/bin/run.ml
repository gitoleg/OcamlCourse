open Printf

let () =
  printf "Here what do we know about some guys\n";
  printf "The first one is %a\n"
    Myprinters.pp Mydata.john;
  printf "And the second is %a\n"
    Myprinters.pp Mydata.bill
