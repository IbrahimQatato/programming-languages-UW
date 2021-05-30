(*homework1*)
fun is_older (d1 : int*int*int, d2 : int*int*int) =
    if not(#1 d1 = #1 d2)
    then #1 d1 < #1 d2
    else if not( #2 d1 = #2 d2)
         then #2 d1 < #2 d2
         else #3 d1 < #3 d2
fun number_in_month ( d : (int*int*int) list, n : int)=
    if null d
    then 0
    else
	let
	    val r = number_in_month(tl d , n)
	in
	    if #2( hd d) = n
	    then 1+r 
	    else r
	end
fun number_in_months ( d :( int*int*int) list, n : int list)=
    if null n
    then 0
    else number_in_month(d, hd n)+ number_in_months(d, tl n)
fun dates_in_month ( d : (int*int*int) list, n : int)=
    if null d
    then []
    else
	let
	    val r = dates_in_month(tl d , n)
	in
	    if #2( hd d) = n
	    then hd d ::r 
	    else r
	end
fun dates_in_months ( d : (int*int*int) list, n : int list)=
    if null n
    then []
    else dates_in_month(d, hd n)@ dates_in_months(d, tl n)
fun get_nth ( list : string list, n : int)=
    if n = 1
    then hd list
    else get_nth(tl list, n-1)
val months =  ["January ", "February ", "March ", "April ",
	       "May ", "June ", "July ", "August ", "September ", "October ", "November ", "December "]
fun date_to_string ( date : int*int*int)=
    get_nth(months, #2 date)^Int.toString(#3 date)^", "^Int.toString(#1 date)
fun number_before_reaching_sum (sum : int, list : int list)=
    let	val diff = sum - hd list
    in if diff-(hd(tl list)) <= 0
       then 1
       else 1+ number_before_reaching_sum(diff, tl list)
    end
fun what_month (day : int)=
    let val months = [0, 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30,32]
    in number_before_reaching_sum(day, months)
    end
fun month_range (day1 : int, day2 : int)=
    if day1>day2
    then []
    else what_month(day1):: month_range(day1+1, day2)
fun oldest ( date : (int*int*int) list)=
    if null date
    then NONE
    else let
	     fun find_oldest(dates : (int*int*int) list)=
		 if null (tl dates)
		 then hd dates
		 else let
		          val tl_result = find_oldest(tl dates)
		      in
			  if is_older( hd dates, tl_result)
			  then hd dates
			  else tl_result
		      end
	 in SOME (find_oldest date)
    end
(*challenge problems*)
fun remove_duplicate (month : int, months : int list) =
    if null months
    then []
    else
	if month = hd months
	then remove_duplicate(month, tl months)
	else hd months :: remove_duplicate(month, tl months)
fun remove_duplicates (months : int list) =
    if null months
    then []
    else
	let val r = remove_duplicate(hd months, tl months)
	in
	    hd months :: remove_duplicates(r)
	end
fun number_in_months_challenge (d:(int*int*int) list, n : int list)=
    number_in_months(d, remove_duplicates(n))
fun dates_in_months_challenge (d:(int*int*int) list, n : int list)=
    dates_in_months(d, remove_duplicates(n))
fun reasonable_date (date : (int*int*int))=
    let fun is_leap (y) =
	    y mod 400 = 0 orelse (y mod 4 = 0 andalso y mod 100 <> 0)
	val day = #3 date
	val month = #2 date
	val year = #1 date
	val valids = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
	val leap_valids = [31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
	fun get_nth ( list : int list, n : int)=
	    if n = 1
	    then hd list
	    else get_nth(tl list, n-1)
    in
	year > 0 andalso month<13 andalso month>0 andalso if is_leap(year)
							  then day <= get_nth(leap_valids, month)
							  else day <= get_nth(valids, month)
    end
