(* This is a comment. This is our first program. *)

(* these are called sequence of bindings which also caled program *)

val x = 34;
(* (Type-checking) static env: x: int *)
(* (Evaluating) dynamic env: x --> 34 *)

val y = 17;
(* (Type-checking) static env: x: int, y: int *)
(* (Evaluating) dynamic env: x--> 34, y --> 17 *)

val z = (x + y) + (y + 2);
(* (Type-checking) static env: x: int, y: int, z(Type-infer): int *)
(* (Evaluating) dyanamic env: x--> 34, y --> 17, z --> (lookup(x) + lookup(y)) + (lookup(y) + 2)) --> 70 *)
(* this line uses preceding(earlier) bindings of x and y to type-checking and evaluating *)

val q = z + 1;
(* (Type-checking) static env: x: int, y: int, z(Type-infer): int, q: int  *)
(* (Evaluating) dyanamic env: x--> 34, y --> 17, z --> 70, q --> 71 *)


val abs_of_z = if z < 0 then 0 - z else z; (* conditional *)
(* static env: ..., abs_of_z: int *)
(* dynamic env: ..., abs_of_z --> 70 *)

val abs_of_z_simpler = abs z; (* function *)
(* can be written: val abs_of_z_simpler = abs(z) *)




