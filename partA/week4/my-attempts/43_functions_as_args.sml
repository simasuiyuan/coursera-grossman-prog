fun incr_n_times_lame (n, x) = 
  if n=0
  then x
  else 1 + incr_n_times_lame(n-1,x)

(* func as args*)
(* 
 * fun f(g, ...) = ... g(...) ...
 * fun h1 ... = ...
 * fun h2 ... = ...
*)

fun double_n_times_lame (n, x) = (* computes 2^n * x *)
  if n=0
  then x
  else 2 * double_n_times_lame(n-1, x)

fun nth_tail_lame (n,xs) =
   if n=0
   then xs
   else tl (nth_tail_lame(n-1,xs))

(* smarter way => pass similar operations (functions) into a main function
 * (repeatly revoked by all operations) *)

fun n_times(f,n,x) = 
  if n=0
  then x
  else f (n_times(f, n-1, x))

fun incr x = x + 1
fun double x = 2 * x

val x1 = n_times(double, 4, 7)
val x2 = n_times(incr, 4, 7)
val x3 = n_times(tl, 2, [4, 8, 12, 16])

