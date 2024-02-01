fun is_older (date1 : int*int*int, date2 : int*int*int) =
    #1 date1 < #1 date2 orelse
    (#1 date1 = #1 date2 andalso #2 date1 < #2 date2) orelse
    (#1 date1 = #1 date2 andalso #2 date1 = #2 date2 andalso #3 date1 < #3 date2)

fun number_in_month (dates : (int*int*int) list, month : int) =
    if null dates
    then 0
    else if #2 (hd dates) = month
    then number_in_month(tl dates, month) + 1
    else number_in_month(tl dates, month)

fun number_in_months (dates : (int*int*int) list, months : int list) =
    if null months
    then 0
    else number_in_month(dates, hd months) + number_in_months(dates, tl months)

fun dates_in_month (dates : (int*int*int) list, month : int) =
    if null dates
    then []
    else if #2 (hd dates) = month
    then (hd dates) :: dates_in_month(tl dates, month)
    else dates_in_month(tl dates, month)

fun dates_in_months (dates : (int*int*int) list, months : int list) =
    if null months
    then []
    else dates_in_month(dates, hd months) @ dates_in_months(dates, tl months)

fun get_nth (strList : string list, n : int) =
    if n = 1
    then hd strList
    else get_nth(tl strList, n-1)

fun date_to_string (date : int*int*int) =
    let val monthStrList = ["January", "February", "March", "April", "May", "June",
			 "July", "August", "September", "October", "November", "December"]
	val year = #1 date
	val month = #2 date
	val day = #3 date
    in get_nth(monthStrList, month) ^ " " ^ Int.toString(day) ^ ", " ^ Int.toString(year)
    end

fun number_before_reaching_sum (sum : int, posIntList : int list) =
    let	fun find_n (acc : int, n : int, remList : int list) =
	    if null remList
	    then n
	    else
		let val newSum = acc + (hd remList)
		in
		    if newSum < sum
		    then find_n(newSum, n+1, tl remList)
		    else n
		end
    in find_n(0, 0, posIntList)
    end

fun what_month (dayOfYear : int) =
    let val monthAcc = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31] (* leap year ignored *)
    in number_before_reaching_sum(dayOfYear, monthAcc) + 1
    end

fun month_range (day1 : int, day2 : int) =
    let fun append_month (startDay : int, endDay : int, monthList : int list) =
	    if startDay > endDay
	    then monthList
	    else append_month(startDay, endDay-1, what_month(endDay)::monthList)
    in append_month(day1, day2, [])
    end

fun oldest (dates : (int*int*int) list) =
    if null dates
    then NONE
    else let
	fun oldest_nonempty (dates : (int*int*int) list) =
	    if null (tl dates)
	    then hd dates
	    else let val oldest_date = oldest_nonempty(tl dates)
		 in
		     if is_older(hd dates, oldest_date)
		     then hd dates
		     else oldest_date
		 end
    in SOME(oldest_nonempty dates)
    end

(* challenge problems *)
fun remove_duplicates (intList : int list) =
    let
	fun exist (x : int, intList : int list) =
	    if null intList
	    then false
	    else (x = (hd intList) orelse exist(x, tl intList))
	fun append_unique (intList : int list, unique_list : int list) =
	    if null intList
	    then unique_list
	    else if exist(hd intList, unique_list)
	    then append_unique(tl intList, unique_list)
	    else append_unique(tl intList, (hd intList)::unique_list)
    in rev(append_unique(intList, []))
    end

fun number_in_months_challenge (dates : (int*int*int) list, months : int list) =
    let val months = remove_duplicates(months)
    in
	if null months
	then 0
	else number_in_month(dates, hd months) + number_in_months(dates, tl months)
    end

fun dates_in_months_challenge (dates : (int*int*int) list, months : int list) =
    let val months = remove_duplicates(months)
    in
	if null months
	then []
	else dates_in_month(dates, hd months) @ dates_in_months(dates, tl months)
    end

fun reasonable_date (date : int*int*int) =
    let
	val year = #1 date
	val month = #2 date
	val day = #3 date
	fun is_leap_year (year : int) =
	    (year mod 400 = 0) orelse ((year mod 100 <> 0) andalso (year mod 4 = 0))
	val daysInMonth = [31, 28 + (if is_leap_year(year) then 1 else 0), 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
	fun get_days_in_month (month : int, daysInMonth : int list) =
	    if month = 1
	    then hd daysInMonth
	    else get_days_in_month(month-1, tl daysInMonth)
    in
	(year > 0) andalso (month >= 1) andalso (month <= 12)
	andalso day >= 1 andalso day <= get_days_in_month(month, daysInMonth)
    end
