fun is_older (date1 : (int*int*int), date2: (int*int*int)) =
    let val delta = (#1 date1 - #1 date2) * 366 + (#2 date1 - #2 date2) * 31 + (#3 date1 - #3 date2)
    in if delta < 0
       then true
       else false
    end
									   

fun number_in_month (dates : (int*int*int) list, month : int) =
    if null dates
    then 0
    else
	let
	    fun is_valid(date : int*int*int) =
		if #2 date = month
		then 1
		else 0
	in
	    is_valid(hd(dates)) + number_in_month(tl(dates), month)
	end

fun number_in_months (dates :(int*int*int) list, months : int list) =
    if null months
    then 0
    else number_in_month(dates, hd(months)) + number_in_months(dates, tl(months))
	
fun dates_in_month (dates : (int*int*int) list, month : int) =
    if null dates
    then []
    else
	let val ans = dates_in_month(tl(dates), month)
	in
	    if #2 (hd(dates)) = month
	    then hd(dates) :: ans
	    else ans
	end

fun dates_in_months (dates : (int*int*int) list, months : int list) =
    if null months
    then []
    else
	dates_in_month(dates, hd months) @ dates_in_months(dates, tl months)

fun get_nth (strs : string list, n : int) =
    if n = 1
    then hd strs
    else
	get_nth(tl strs, n-1)

fun date_to_string (date : int*int*int) =
    let val months = ["January", "February", "March", "April","May", "June", "July", "August", "September", "October", "November", "December"]
    in
	get_nth(months, #2 date) ^ " " ^ Int.toString(#3 date) ^ ", " ^ Int.toString(#1 date)
    end

fun number_before_reaching_sum (sum : int, array : int list) =
    if sum <= 0 orelse null array
    then ~1
    else 1 + number_before_reaching_sum(sum - hd(array), tl(array))
	     
fun what_month (day : int) =
    let val days_of_month = [31,28,31,30,31,30,31,31,30,31,30,31]
    in
	1 + number_before_reaching_sum(day, days_of_month)
    end

fun month_range (days1 : int, days2 : int) =
    if days1 > days2
    then []
    else what_month(days1) :: month_range(days1 + 1, days2)

fun oldest (dates : (int*int*int) list) =
    if null dates
    then NONE
    else
	let val tl_ans = oldest(tl dates)
	in if isSome tl_ans andalso is_older(valOf tl_ans, (hd dates))
	   then tl_ans
	   else SOME (hd dates)
	end
