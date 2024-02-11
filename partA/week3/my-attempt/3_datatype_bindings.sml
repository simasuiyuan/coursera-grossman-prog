datatype mytype = TwoInts of int * int
                | Str of string
                | Pizza
(* mytype cloud be 3 types of vaues int*int, string or nothing
 * each of them are tagged by TwoInts, Str and Pizza <= which are called
 * `constructor`*)

val a = Str "hi"
val b = Str
val c = Pizza
val d = TwoInts(1+2,3+4)
val e = a
