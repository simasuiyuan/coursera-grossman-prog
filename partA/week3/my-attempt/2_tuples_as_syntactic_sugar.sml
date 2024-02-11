(* using records to define tuples *)
val record_tuple = {1 = 3, 2 = 20, 3 = 89}
(* type checking for my_tuples: int * int * int *)

val ml_tuple = (3, 20, 89)
(* tuple sytax is just a syntactic sugar that using records to defne tuple*)

val record_ele = #1 record_tuple

val ml_ele = #1 ml_tuple
