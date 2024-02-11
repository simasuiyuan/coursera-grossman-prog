datatype mytype = TwoInts of int * int
                | Str of string
                | Pizza

(* use "3_datatype_bindings.sml"; *)
(* use case expression to access field of mytype *)

fun f x = (* function f with type checking: mytype -> int *) 
  case x of
       Pizza => 3
     | TwoInts(i1, i2) => i1 + i2
     | Str s => String.size s
