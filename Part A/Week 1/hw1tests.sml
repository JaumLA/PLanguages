use "hw1.sml";

(* Tests for fun is_older. *)
val test1_1 = is_older ((1,2,3),(2,3,4)) = true;
val test1_2 = is_older ((1,2,3),(1,2,3)) = false;
val test1_3 = is_older ((3,2,3),(2,3,4)) = false;
val test1_4 = is_older ((1,2,3),(2,1,4)) = true;
val test1_5 = is_older ((2,4,3),(2,3,4)) = false;

(* Tests for fun number_in_month. *)
val test2_1 = number_in_month([], 3) = 0;

val test2_2 = number_in_month([(3,12,17),(1999,12,3),(1999,2,31),(1850,1,3)], 12) = 2;

val test2_3 = number_in_month([(3,11,17),(1999,1,3),(1999,2,31),(1850,1,3)], 12) = 0;

val test2_4 = number_in_month([(5,8,3)], 8) = 1;

val test2_5 = number_in_month([(3,10,3),(3,10,3),(3,10,3),(3,10,3),(3,10,3)], 10) = 5;

(* Tests for fun number_in_months. *)
val test3_1 = number_in_months([], [3,4,2]) = 0;

val test3_2 = number_in_months([(3,12,17),(1999,12,3),(1999,2,31),(1850,1,3)], [12,5,4,2]) = 3;

val test3_3 = number_in_months([(3,11,17),(1999,1,3),(1999,2,31),(1850,1,3)], [12,10,7,6]) = 0;

val test3_4 = number_in_months([(5,8,3)], [8]) = 1;

val test3_5 = number_in_months([(3,10,3),(3,11,3),(3,8,3),(3,10,3),(3,5,3)], [10,11,8,5]) = 5;

(* Tests for fun dates_in_month. *)
val test4_1 = dates_in_month([(3,4,2)], 4) = [(3,4,2)];

val test4_2 = dates_in_month ([(2012,2,28),(2013,12,1)],2) = [(2012,2,28)];

val test4_2 = dates_in_month ([(2012,2,28),(2013,12,1),(2011,2,9)],2) = [(2012,2,28),(2011,2,9)];

(* Tests for fun dates_in_months. *)
val test5_1 = dates_in_months ([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[2,3,4]) = [(2012,2,28),(2011,3,31),(2011,4,28)];

val test5_2 = dates_in_months ([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[4,7,8,6,9]) = [(2011,4,28)];

val test5_3 = dates_in_months ([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[4]) = [(2011,4,28)];

val test5_4 = dates_in_months ([(2012,5,28),(2013,6,1),(2011,7,31),(2011,8,28)],[2,3,4]) = [];

(* Tests for fun get_nth. *)
val test6_1 = get_nth (["hi", "there", "how", "are", "you"], 2) = "there";
val test6_2 = get_nth (["hi", "there", "how", "are", "you"], 1) = "hi";
val test6_3 = get_nth (["hi", "there", "how", "are", "you"], 5) = "you";
val test6_4 = get_nth (["hi", "there", "how", "are", "you"], 4) = "are";

(* Tests for fun date_to_string. *)
val test7_1 = date_to_string (2013, 6, 1) = "June 1, 2013";
val test7_2 = date_to_string (1945, 12, 31) = "December 31, 1945";
val test7_3 = date_to_string (1999, 1, 15) = "January 15, 1999";
val test7_4 = date_to_string (2018, 8, 23) = "August 23, 2018";

(* Tests for fun number_before_reaching_sum. *)
val test8_1 = number_before_reaching_sum (10, [1,2,3,4,5]) = 3;
val test8_2 = number_before_reaching_sum (10, [5,4,3,2,1]) = 2;
val test8_3 = number_before_reaching_sum (5, [1,2,3,4,5]) = 2;
val test8_4 = number_before_reaching_sum (12, [1,2,3,4,5]) = 4;

(* Tests for fun what_month. *)
val test9_1 = what_month 70 = 3;
val test9_2 = what_month 30 = 1;
val test9_3 = what_month 83 = 3;
val test9_4 = what_month 150 = 5;
val test9_5 = what_month 362 = 12;

(* Tests for fun month_range. *)
val test10_1 = month_range (31, 34) = [1,2,2,2];
val test10_2 = month_range (31, 31) = [1];
val test10_3 = month_range (20, 30) = [1,1,1,1,1,1,1,1,1,1,1];
val test10_4 = month_range (60, 34) = [];
val test10_5 = month_range (360, 365) = [12,12,12,12,12,12];

(* Tests for fun oldest. *)
val test11_1 = oldest([(3,12,17),(1999,12,3),(1999,2,31)]) = SOME (3,12,17);
val test11_2 = oldest([(3,12,17),(3,12,3),(3,2,31)]) = SOME (3,2,31);
val test11_3 = oldest([(1999,12,17),(1999,12,3),(1999,12,31)]) = SOME (1999,12,3);
val test11_3 = oldest([]) = NONE;

(* Tests for fun number_in_months_challenge. *)
val test12_1 = number_in_months_challenge([(3,12,17),(1999,12,3),(1999,2,31),(1850,1,3)], [12,5,4,2]) = 3;

val test12_2 = number_in_months_challenge([(3,12,17),(1999,12,3),(1999,2,31),(1850,1,3)], [12,5,4,4,4,4,2,7,7,7,7,7,12,12,12,12,5,5,5,5]) = 3;

val test12_3 = number_in_months_challenge([(3,9,17),(1999,1,3),(1999,7,31),(1850,1,3)], [12,5,4,2,8,0,11]) = 0;

val test12_4 = number_in_months_challenge([(3,12,17),(1999,12,3),(1999,2,31),(1850,1,3)], []) = 0;

val test12_5 = number_in_months_challenge([(3,12,17),(1999,12,3),(1999,2,31),(1850,1,3)], [12,12,12,12,12,12]) = 2;

(* Tests for fun reasonable_date. *)
val test13_1 = reasonable_date((1999,1,15)) = true;
val test13_2 = reasonable_date((1999,1,32)) = false;
val test13_3 = reasonable_date((1999,1,31)) = true;
val test13_4 = reasonable_date((4,2,29)) = true;
val test13_5 = reasonable_date((1999,2,29)) = false;
val test13_6 = reasonable_date((4,2,30)) = false;
val test13_7 = reasonable_date((~1999,2,29)) = false;
val test13_8 = reasonable_date((1999,~2,29)) = false;
val test13_9 = reasonable_date((0,0,0)) = false;
val test13_10 = reasonable_date((1,1,1)) = true;
