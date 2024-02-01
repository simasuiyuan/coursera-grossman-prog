(* version 1
fun count_from1(x : int) =
    let
	fun count(from : int, to : int) =
	    if from=to
	    then to::[]
	    else from :: count(from+1, to)
    in
	count(1, x)
    end
*)

(* version 2 *)
(* notice that how the `to` paramter is removed from inner function `count` *)
fun count_from1(x : int) =
    let
	fun count(from : int) =
	    if from=x
	    then x::[]
	    else from :: count(from+1)
    in
	count(1)
    end
