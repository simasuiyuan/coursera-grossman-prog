fun alternate (intList : int list) =
    let fun sum_alternative (intList : int list, sign : bool) =
	    if null intList
	    then 0
	    else if sign
	    then sum_alternative(tl intList, not sign) + hd intList
	    else sum_alternative(tl intList, not sign) - hd intList
    in sum_alternative(intList, true)
    end

fun min_max (intList : int list) =
    let fun min_max_nonempty (xs : int list, min : int, max : int) =
	    if null (tl xs)
	    then (min, max)
	    else let tl_ans = min_max_nonempty(tl xs, min, max)
		     val curr_min = #1 tl_ans
		     val curr_max = #2 tl_ans	
		 in
		     if hd xs < curr_min
		     then val curr_max = hd xs
		     else 
			     
