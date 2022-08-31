(* A date is represented as a tuple (int*int*int) where
     the first element represent the year, second the 
     month and the third the day. *)

(* Evaluate true only if the first date comes before the second *)
fun is_older (date1 : int*int*int, date2 : int*int*int) =
    let
	val year1 = #1 date1
	val month1 = #2 date1
	val day1 = #3 date1
	val year2 = #1 date2
	val month2 = #2 date2
	val day2 = #3 date2
    in
	if year1 = year2
	then if month1 = month2
	     then  day1 < day2         
	     else month1 < month2	  
	else year1 < year2
    end
	
(* Take a list of dates and return how many dates 
   are in the given month. *)
fun number_in_month (dates : (int*int*int) list, month : int) =
    if null dates
    then 0
    else let val remaining = number_in_month(tl(dates), month)
	 in
	     if #2 (hd(dates)) = month
             then 1 + remaining
             else remaining
	 end

(* Take a list of dates and return how many dates are 
   in the given list of months. *)			     
fun number_in_months (dates : (int*int*int) list, months : int list) =
    if null months
    then 0
    else number_in_month(dates, hd(months)) + number_in_months(dates, tl(months))

(* Take a list of dates and return a list of dates 
   that are in the given month. *)
fun dates_in_month (dates : (int*int*int) list, month : int) =
    if null dates
    then []
    else
	let
	    val date = hd(dates)
	    val anws = dates_in_month(tl(dates), month)
	 in
	     if #2 date = month
	     then date :: anws
	     else anws
	 end

(* Take a list of dates and return a list of dates 
   that are in the given list of months. *)
fun dates_in_months (dates : (int*int*int) list, months : int list) =
    if null months
    then []
    else dates_in_month(dates, hd(months)) @ dates_in_months(dates, tl(months))

(* Return the n-th element of a list of strings. *)
fun get_nth (words : string list, pos : int) =
    if pos = 1
    then hd(words)
    else get_nth(tl(words), pos-1)

(* Format the given date to a string, e.g. "January 15, 1999". *)		
fun date_to_string (date : (int*int*int)) =
    let
	val months = ["January", "February", "March", "April", "May", "June",
		      "July", "August","September", "October", "November","December"]
	val date_day = Int.toString(#3 date)
	val date_month = #2 date
	val date_year = Int.toString(#1 date)
    in
	get_nth(months,date_month)^" "^date_day^", "^date_year
    end


(* Return the first n number of the list that add less than sum. *)
fun number_before_reaching_sum (sum : int, numbers : int list) =
    if sum <= hd(numbers)
    then 0
    else 1 + number_before_reaching_sum(sum-(hd(numbers)), tl(numbers))

(* Take a day (an int between 1 and 365) and return the month 
   that day is in. *)				       
fun what_month (day : int) =
    let val days_in_month = [31,28,31,30,31,30,31,31,30,31,30,31]
    in
	number_before_reaching_sum(day, days_in_month) + 1
    end

(* Takes two days of the year day1 and day2 and returns an int list
[m1,m2,...,mn] where m1 is the month of day1, m2 is the month of day1+1, ..., and 
mn is the month of day day2. *)	
fun month_range (day1 : int, day2 : int) =
    if day1 > day2
    then []
    else what_month(day1) :: month_range(day1 + 1,day2)
	
(* Take a list of dates and return the oldest date in the list. *)
fun oldest (dates : (int*int*int) list) =
    if null dates
    then NONE
    else
	let
	    fun oldest_nonempty (dates : (int*int*int) list, date : (int*int*int)) =
		if null dates
		then SOME date
		else if is_older(date, hd(dates))
		     then oldest_nonempty(tl(dates), date)
		     else oldest_nonempty(tl(dates), hd(dates))
				    
	    val first_date = hd(dates)
	in
	    oldest_nonempty(dates, first_date)
	end

(* Helper functions for challenge problems 11. *)
fun remove (numbers : int list, num : int) = 
    if null numbers
    then []
    else if hd(numbers) = num
         then remove(tl(numbers), num)
         else hd(numbers) :: (remove(tl(numbers), num))
				   
fun create_unique_list (numbers : int list) =
    let
	val answ = remove(tl(numbers), hd(numbers))
    in
	if null answ
	then [hd(numbers)]
	else hd(numbers) :: create_unique_list(answ)
    end
(* End of the helper functions. *)
	
(* Take a list of dates and return how many dates are
   in the given list of months. *)			     
fun number_in_months_challenge (dates : (int*int*int) list, months : int list) =
    if null months
    then 0
    else let val unique_months = create_unique_list(months)
	 in
	     number_in_months(dates, unique_months)
	 end

(* Take a list of dates and return a list of dates 
   that are in the given list of months. *)
fun dates_in_months_challenge (dates : (int*int*int) list, months : int list) =
    if null months
    then []
    else let val unique_months = create_unique_list(months)
	 in
	     dates_in_months(dates, unique_months)
	 end


fun n_th_element (numbers : int list, pos : int) =
    if pos < 1
    then 0
    else if pos = 1
    then hd(numbers)
    else n_th_element (tl(numbers), pos - 1)
(* Return true if the given date is reasonable. A reasonable date has a positive
   year, a month between 1 and 12 and a day between 1 and 31. Leap years are allowed. *)
fun reasonable_date (date : (int*int*int)) =
    let
	val year = #1 date
	val month = #2 date
	val day = #3 date
	val is_leap = year mod 100 <> 0 andalso
		      (year mod 4 = 0 orelse year mod 400 = 0)
			  
	val days_in_months = if is_leap
			     then [31,29,31,30,31,30,31,31,30,31,30,31]
			     else [31,28,31,30,31,30,31,31,30,31,30,31]
	val lower_day = 1
	val max_day = n_th_element(days_in_months, month)

	val year = #1 date
	val month = #2 date
	val day = #3 date
		     
	val is_leap = year mod 100 <> 0 andalso
			(year mod 4 = 0 orelse year mod 400 = 0)	
	val valid_year = year > 0	    
	val valid_month = month >= 1 andalso month <= 12
	val valid_day = day >= lower_day andalso day <= max_day

    in
	valid_year andalso valid_month andalso valid_day
    end
