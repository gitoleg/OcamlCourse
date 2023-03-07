open Printf

open Mytypes

let pp ch {name; age; salary} =
  fprintf ch "Name %s, age %d, salary %g" 
    name age salary
