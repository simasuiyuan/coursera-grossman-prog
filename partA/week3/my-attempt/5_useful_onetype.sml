(* use as Enum (enumerating over non-inclusive options) *)
datatype suit = Club | Diamond | Heart | Spade

(* ML allows variants to carry data *)
datatype rank = Jack | Queen | King | Ace | Num of int

val comb = (Club, Queen) (* type checking: suit * rank *)


(* define stdent id by student number (int) 
 * or full name (string * (string option) * string) *)
 datatype id = StudentNum of int
             | Name of string * (string option) * string

(* above id is a good example of using one-of-each instead of each-of type !*)
(* If student_num is -1, then use the other fields, otherwise ignore other fields *)
(* bad_id = {  *)
(*   student_num : int option, *)
(*   first : string, *)
(*   middle : string option, *)
(*   last : string  *)
(* } *)

datatype exp = Constant of int
           | Negate of exp
           | Add of exp * exp
           | Multiply of exp * exp
           | Largest of exp list

fun eval e = 
  case e of 
       Constant i => i
     | Negate e2 => ~ (eval e2)
     | Add (e1, e2) => (eval e1) + (eval e2)
     | Multiply (e1, e2) => (eval e1) * (eval e2)
     (* | Largest e_list => max(eval e_list *)


val a = Constant 10
val b = Negate a
val c = Add(a, b)
val d = Multiply(a, b)

val res = eval(Add (Constant 19, Negate (Constant 4)))
