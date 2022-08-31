fun alternate (loi : int list) =
    let fun alternate_list (loi : int list, pos : int) =
	    if null loi
	    then 0
	    else if(pos mod 2 = 0)
	         then alternate_list(tl(loi), pos + 1) - hd(loi)
	         else hd(loi) + alternate_list(tl(loi), pos + 1)
    in
	alternate_list(loi, 1)
    end

fun min_max (loi : int list) =
    let
	fun helper (loi : int list, min : int, max : int) =
	    if null loi
	    then (min, max)
	    else let
		val num = hd(loi)
		val actual_max = (if max < num then num else max)
		val actual_min = (if min > num then num else min)
	    in
		helper(tl(loi), actual_min, actual_max)
	    end
    in
	helper(loi, hd(loi), hd(loi))
    end

fun cumsum (loi : int list) =
    let
	fun partial_sum (loi : int list, sum : int) =
	    if null loi
	    then []
	    else let val next_sum = sum + hd(loi)
	         in
		     next_sum :: partial_sum(tl(loi), next_sum)
		 end
    in
	partial_sum(loi, 0)
    end

fun greeting (name : string option) = 	
    if isSome name
    then "Hello there, "^valOf(name)^"!"
    else "Hello there, you!"				 

fun repeat (numbers : int list, each_times : int list) =
    if null numbers
    then []
    else
	let
	    fun repeat_number (number : int, times : int) =
		if times < 1
		then []
		else number :: repeat_number(number, times - 1)
    in
	repeat_number(hd(numbers), hd(each_times)) @
	repeat(tl(numbers), tl(each_times))
    end

fun add_opt (num1 : int option, num2 : int option) =
    if isSome num1 andalso isSome num2
    then SOME(valOf(num1) + valOf(num2))
    else NONE

fun add_all_opt (loi : (int option) list) =
    if null loi
    then NONE
    else
	let
	    val anws = add_all_opt(tl(loi))
	in
	    if isSome(hd(loi)) andalso isSome(anws)
	    then SOME(valOf(hd(loi)) + valOf(anws))
	    else if isSome(hd(loi))
	    then hd(loi)
	    else anws
	end

fun any (lob : bool list) =
    if null lob
    then false
    else hd(lob) orelse any(tl(lob))

fun all (lob : bool list) =
    if null lob
    then true
    else hd(lob) andalso all(tl(lob))

fun zip (loi1 : int list, loi2: int list) =
    if null loi1 orelse null loi2
    then []
    else (hd(loi1), hd(loi2)) :: zip(tl(loi1), tl(loi2))

fun zip_recycle (loi1 : int list, loi2: int list) =
    if null loi1 orelse null loi2
    then []
    else
	let
	    fun get_length (loi : int list) =
		if null loi
		then 0
		else 1 + get_length(tl(loi))
				   
	    val len1 = get_length(loi1)
	    val len2 = get_length(loi2)
				 
	    fun helper1 (loi1 : int list, repeated : int list) =
		if null loi1
		then []
		else if null repeated
		then (hd(loi1), hd(loi2)) :: helper1(tl(loi1), tl(loi2))
		else (hd(loi1), hd(repeated)) :: helper1(tl(loi1), tl(repeated))
							
	    fun helper2 (repeated : int list, loi2 : int list) =
		if null loi2
		then []
		else if null repeated
		then (hd(loi1), hd(loi2)) :: helper2(tl(loi1), tl(loi2))
		else (hd(repeated), hd(loi2)) :: helper2(tl(repeated), tl(loi2))
	in
	    if null loi1 andalso null loi2
	    then []
	    else if len1 = len2
	    then (hd(loi1), hd(loi2)) :: zip_recycle(tl(loi1), tl(loi2))
	    else if len1 > len2
	    then helper1(loi1, loi2)
	    else helper2(loi1, loi2)
	end

fun zip_opt (loi1 : int list, loi2 : int list) =
    let
	fun get_length (loi : int list) =
	    if null loi
	    then 0
	    else 1 + get_length(tl(loi))
				   
	val len1 = get_length(loi1)
	val len2 = get_length(loi2)
    in
	if len1 = len2
	then SOME(zip(loi1, loi2))
	else NONE
    end

fun lookup (lot : (string * int) list, str : string) =
    if null lot
    then NONE
    else if #1(hd(lot)) = str
    then SOME(#2(hd(lot)))
    else lookup(tl(lot), str)

fun splitup (loi : int list) =
    let
	fun positives (loi : int list) =
	    if null loi
	    then []
	    else if hd(loi) >= 0
	    then hd(loi) :: positives(tl(loi))
	    else positives(tl(loi))

	fun negatives (loi : int list) =
	    if null loi
	    then []
	    else if hd(loi) < 0
	    then hd(loi) :: negatives(tl(loi))
	    else negatives(tl(loi))
    in
	(positives(loi), negatives(loi))
    end

fun split_at (loi : int list, n : int) =

    let
	fun greater (loi : int list) =
	    if null loi
	    then []
	    else if hd(loi) >= n
	    then hd(loi) :: greater(tl(loi))
	    else greater(tl(loi))
			  
	fun less (loi : int list) =
	    if null loi
	    then []
	    else if hd(loi) < n
	    then hd(loi) :: less(tl(loi))
	    else less(tl(loi))
    in
	(greater(loi), less(loi))
    end

fun is_sorted (loi : int list) =
    if null loi
    then true
    else let
	fun helper (loi : int list) =
	    if null(tl(loi))
	    then true
	    else if hd(loi) > hd(tl(loi))
	    then false
	    else helper(tl(loi))
    in
	helper(loi)
    end

fun is_any_sorted (loi : int list) =
    if null loi
    then true
    else let
	fun is_increasing (loi : int list) =
	    if null(tl(loi))
	    then true
	    else if hd(loi) > hd(tl(loi))
	    then false
	    else is_increasing(tl(loi))

	fun is_decreasing (loi : int list) =
	    if null(tl(loi))
	    then true
	    else if hd(loi) < hd(tl(loi))
	    then false
	    else is_decreasing(tl(loi))
    in
	is_increasing(loi) orelse is_decreasing(loi)
    end

fun sorted_merge (loi1 : int list, loi2 : int list) =
    if null loi1
    then loi2
    else if null loi2
    then loi1
    else if hd loi1 < hd loi2
    then hd(loi1) :: sorted_merge(tl(loi1), loi2)
    else hd(loi2) :: sorted_merge(loi1, tl(loi2))

fun q_sort (loi : int list) =
    if null loi
    then []
    else
	let
	    val threshold = hd(loi)
	    val two_lists = split_at(tl(loi), threshold)
				
	    val list1 = #1 two_lists
	    val list2 = #2 two_lists

	    val sorted1 = q_sort(list1)
	    val sorted2 = q_sort(list2)
	in
	    sorted_merge(threshold :: sorted1, sorted2)
	end

fun divide (loi : int list) =
    if null loi
    then ([], [])
    else if null (tl(loi))
    then ([hd(loi)], [])
    else
	let
	    fun alt (loi : int list, pos : int) =
		if null loi
		then []
		else if pos mod 2 <> 0
		then hd(loi) :: alt(tl(loi), pos + 1)
		else alt(tl(loi), pos + 1)
	in
	    (alt(loi, 1), alt(tl(loi), 1))
	end

fun not_so_quick_sort (loi : int list) =
    if null loi
    then []
    else if null (tl(loi))
    then [hd(loi)]
    else
	let
	    val two_lists = divide(loi)
	    val list1 = #1 two_lists
	    val list2 = #2 two_lists

	    val sorted1 = not_so_quick_sort(list1)
	    val sorted2 = not_so_quick_sort(list2)
	in
	    sorted_merge(sorted1, sorted2)
	end

fun full_divide (k : int, n : int) =
    if k = 0
    then (k, n)
    else
	let
	    fun div_count (num : int, divisor : int, count : int) =
		if num = 0 orelse (num mod divisor <> 0)
		then(count, num)
		else div_count(num div divisor, divisor, count + 1)
	in
	    div_count(n, k, 0)
	end

fun factorize (n : int) =
    if n = 0 orelse n = 1
    then []
    else
	let		      
	    fun count (n : int, divisor : int) =
		if n = 1
		then []
		else
		    let
			val t_n = full_divide(divisor, n)
		    in
			if(#1 t_n) = 0
			then count(#2 t_n, divisor + 1)
			else (divisor, #1 t_n) :: count (#2 t_n, divisor + 1)
		    end
	in
	    count(n, 2)
	end

fun multiply (lof : (int * int) list) =
    if null lof
    then 1
    else
	let
	    fun mult (num : int, count : int) =
		if count = 1
		then num
		else if count = 0
		then 0
		else num* mult(num, count - 1)
	in
	    mult(#1 (hd(lof)), #2 (hd(lof))) * multiply (tl(lof))
	end

	    
(* Challenge (hard): Write a function all_products : (int * int) list -> int list that given a 
factorization list result from factorize creates a list all of possible products produced from 
using some or all of those prime factors no more than the number of times they are available. 
This should end up being a list of all the divisors of the number n that gave rise to the list. 
Example: all_products([(2,2), (5,1)]) = [1,2,4,5,10,20]. 
For extra challenge, your recursive process should return the numbers in this order, as opposed to sorting them afterwards. *)
	    
fun all_products (lof : (int * int) list) =
    if null lof orelse #2 (hd(lof)) = 0
    then []
    else
	let
	    fun pow (num : int, p : int) =
		if p <= 1
		then num
		else num * pow(num, p - 1)
			      
	    fun multiply_times (lof : (int * int) list, count : int) =
		if null lof
		then []
		else if count = #2 (hd lof)
		then pow(#1 (hd(lof)), count) :: multiply_times(tl(lof), 1)
		else pow(#1 (hd(lof)), count) :: multiply_times(lof, count + 1)

	    fun factors (lom : int list) =
		if null lom
		then
		else
	    val multiplied_prod = 1 :: multiply_times(lof, 1)
	in
	    multiplied_prod
	end
