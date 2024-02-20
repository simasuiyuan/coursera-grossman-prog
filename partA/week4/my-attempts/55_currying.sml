val sorted3 = fn x => fn y => fn z => z >= y andalso y >= x;
(* int -> int -> int -> bool *)
val first_curr = sorted3 4;
val second_curr = (sorted3 4) 5;
val third_curr = ((sorted3 4) 5) 6;
(* syntatic sugar without brackets *)
val third_curr_new = sorted3 4 5 6;
(* above just show what happend behin the scene (utilizing currying), in actual
 * programming the function no need to define in the way like that*)
fun sorted3_good x y z = z >= y andalso y >= x;
 (* when init this function what actual happend is currying : 
  * type checking: int -> int -> int -> bool  (same as sorted3) *) 

(* Note: difference betweent currying and pass a tuple as one arg to func*)
fun sorted3_tuple (x, y, z) = z >= y andalso y >= x; (* int * int * int -> bool *)
(* Notice the distinction of type checking of these 2,:
 * - sorted3_good: is a syntatic sugar of utilizing currying, is like a chain
 * passing one single arg at a time and pipe to a function/result
 * - sorted3_tuple: only has one layer definition, and it use pattern matching
 * to associate each argument *)


fun curry f x y = f (x, y); 
(* val curry = fn : ('a * 'b -> 'c) -> 'a -> 'b -> 'c *)
fun uncurry f (x, y) = f x y;
(* val uncurry = fn : ('a -> 'b -> 'c) -> 'a * 'b -> 'c *)
(* logic reasoning?: * as and; -> as imply *)
(* 'a and 'b implies 'c *)
(* a -----|     *)
(*        | - c *)
(* b -----|     *)
(* 'a implies 'b implies 'c *)
(* a ---- b ---- c*)

