use "hw3.sml";

val test_only_capitals_1 = only_capitals([]) = []
val test_only_capitals_2 = only_capitals(["As"]) = ["As"]
val test_only_capitals_3 = only_capitals(["as"]) = []
val test_only_capitals_4 = only_capitals(["As", "tf", "YtrsaA", "rRt"]) = ["As", "YtrsaA"]

val test_longest_string1_1 = longest_string1([]) = ""
val test_longest_string1_2 = longest_string1(["a"]) = "a"
val test_longest_string1_3 = longest_string1(["a", "ds", "asds"]) = "asds"
val test_longest_string1_4 = longest_string1(["As", "tf", "YtrsaA", "1trsaa"]) = "YtrsaA"
val test_longest_string1_5 = longest_string1 ["a", "asds", "ds"] = "asds"
										     
val test_longest_string2_1 = longest_string2([]) = ""
val test_longest_string2_2 = longest_string2(["a"]) = "a"
val test_longest_string2_3 = longest_string2(["a", "ds", "asds"]) = "asds"
val test_longest_string2_4 = longest_string2(["As", "tf", "YtrsaA", "1trsaa"]) = "1trsaa"
val test_longest_string2_5 = longest_string2 ["a", "asds", "ds"] = "asds"
										     
val test_longest_string3_1 = longest_string3 [] = ""
val test_longest_string3_2 = longest_string3 ["a"] = "a"
val test_longest_string3_3 = longest_string3 ["a", "ds", "asds"] = "asds"
val test_longest_string3_4 = longest_string3 ["As", "tf", "YtrsaA", "1trsaa"] = "YtrsaA"
val test_longest_string3_5 = longest_string3 ["a", "asds", "ds"] = "asds"
val test_longest_string3_6 = longest_string3 ["this","list","has","no","capital","letters"]
								       
val test_longest_string4_1 = longest_string4 [] = ""
val test_longest_string4_2 = longest_string4 ["a"] = "a"
val test_longest_string4_3 = longest_string4 ["a", "ds", "asds"] = "asds"
val test_longest_string4_4 = longest_string4 ["As", "tf", "YtrsaA", "1trsaa"] = "1trsaa"
val test_longest_string4_5 = longest_string4 ["a", "asds", "ds"] = "asds"
val test_longest_string4_6 = longest_string4 ["this","list","has","no","capital","letters"]
					     
val test_longest_capitalized_1 = longest_capitalized [] = ""
val test_longest_capitalized_2 = longest_capitalized ["a"] = ""
val test_longest_capitalized_3 = longest_capitalized ["A"] = "A"
val test_longest_capitalized_4 = longest_capitalized ["A", "s"] = "A"
val test_longest_capitalized_5 = longest_capitalized ["s", "D", "A"] = "D"
val test_longest_capitalized_6 = longest_capitalized ["s", "DD", "A"] = "DD"
val test_longest_capitalized_7 = longest_capitalized ["A","bc","C"] = "A"
val test_longest_capitalized_8 = longest_capitalized ["this","list","has","no","capital","letters"]
									  
val test_rev_string_1 = rev_string "" = ""
val test_rev_string_2 = rev_string "asd" = "dsa"
val test_rev_string_3 = rev_string "ASd" = "dSA"
val test_rev_string_4 = rev_string "preI" = "Ierp"
val test_rev_string_5 = rev_string "a" = "a"
val test_rev_string_6 = rev_string "Eyo" = "oyE"

val test_first_answer_1 = first_answer (fn x => if x > 3 then SOME x else NONE) [1,2,3,4,5] = 4
val test_first_answer_2 = first_answer (fn x => if x = " " then SOME x else NONE) ["","asd"," ","ff","ds"] = " "
(*val test_first_answer_3 = ((first_answer (fn x => if x = 0 then SOME x else NONE) [1,2,3,4,5]) handle NoAnswer => true)*)
val test_first_answer_4 = first_answer (fn x => if x > 3 then SOME x else NONE) [1,2,3,4,5] = 4
												  
val test_all_answers_1 = all_answers (fn x => if x > 6 then SOME [x] else NONE) [] = SOME []
val test_all_answers_2 = all_answers (fn x => if x < 6 then SOME [x] else NONE) [1,2,3,4] = SOME [1,2,3,4]
val test_all_answers_3 = all_answers (fn x => if x > 6 then SOME [x] else NONE) [1] = NONE
val test_all_answers_4 = all_answers (fn x => if x > 2 then SOME [x] else NONE) [1,2,3,4] = NONE
val test_all_answers_5 = all_answers (fn x => if x < 2 then SOME [x] else NONE) [1,2,3,4] = NONE
val test_all_answers_6 = all_answers (fn x => if x = " " then SOME [x] else NONE) ["","asd"," ","ff","ds"] = NONE
val test_all_answers_7 = all_answers (fn x => if x = " " then SOME [x] else NONE) [" "," "," "," "," "] = SOME [" "," "," "," "," "]

val test_count_wildcards_1 = count_wildcards Wildcard = 1
val test_count_wildcards_2 = count_wildcards Wildcard = 1
val test_count_wildcards_3 = count_wildcards Wildcard = 1
val test_count_wildcards_4 = count_wildcards Wildcard = 1

							    
val test_count_wild_and_variable_lengths_1 = count_wild_and_variable_lengths (Variable "") = 0
val test_count_wild_and_variable_lengths_2 = count_wild_and_variable_lengths Wildcard = 1
val test_count_wild_and_variable_lengths_3 = count_wild_and_variable_lengths (Variable "sdew") = 4
val test_count_wild_and_variable_lengths_4 = count_wild_and_variable_lengths Wildcard = 1
val test_count_wild_and_variable_lengths_5 = count_wild_and_variable_lengths (TupleP [Wildcard, ConstP 12, Wildcard]) = 2
val test_count_wild_and_variable_lengths_6 = count_wild_and_variable_lengths (TupleP [Wildcard, Variable "str", Wildcard]) = 5
val test_count_wild_and_variable_lengths_7 = count_wild_and_variable_lengths (TupleP [Wildcard, Variable "str", Wildcard, Variable "str2"]) = 9
val test_count_wild_and_variable_lengths_8 = count_wild_and_variable_lengths (ConstructorP("pattern", (TupleP [Wildcard, ConstP 12, Wildcard]))) = 2
val test_count_wild_and_variable_lengths_9 = count_wild_and_variable_lengths (ConstructorP("pattern", (TupleP [Wildcard, Variable "str", Wildcard]))) = 5

											    
val test_count_some_var_1 = count_some_var ("x", Variable("x")) = 1
val test_count_some_var_2 = count_some_var ("x", (TupleP [Wildcard, ConstP 12, Wildcard])) = 0
val test_count_some_var_3 = count_some_var ("x", (TupleP [Wildcard, Variable "str", Wildcard])) = 0
val test_count_some_var_4 = count_some_var ("x", (TupleP [Wildcard, Variable "x", Wildcard])) = 1
val test_count_some_var_5 = count_some_var ("x", (TupleP [Wildcard, Variable "x", Wildcard, Variable "x"])) = 2
val test_count_some_var_6 = count_some_var ("x", (ConstructorP("pattern", (TupleP [Wildcard, Variable "x", Wildcard])))) = 1
val test_count_some_var_7 = count_some_var ("x", (ConstructorP("x", (TupleP [Wildcard, Variable "x", Wildcard])))) = 1

val test_check_pat_1 = check_pat (Variable("x")) = true
val test_check_pat_2 = check_pat (TupleP [Wildcard, Variable "x", Wildcard]) = true
val test_check_pat_3 = check_pat (TupleP [Wildcard, Variable "x", Variable "y"]) = true
val test_check_pat_4 = check_pat (TupleP [Wildcard, Variable "x", Variable "x"]) = false
val test_check_pat_5 = check_pat (ConstructorP("x", (TupleP [Wildcard, Variable "x", Wildcard]))) = true
val test_check_pat_6 = check_pat (ConstructorP("x", (TupleP [Wildcard, Variable "x", ConstructorP("y", Variable "y")]))) = true
val test_check_pat_7 = check_pat (ConstructorP("x", (TupleP [Wildcard, Variable "x", ConstructorP("y", Variable "x")]))) = false
val test_check_pat_8 = check_pat (ConstructorP("x", (TupleP [Wildcard, Variable "x", ConstructorP("y", TupleP [Variable "y"])]))) = true
val test_check_pat_9 = check_pat (ConstructorP("x", (TupleP [Wildcard, Variable "x", ConstructorP("y", TupleP [Variable "z"])]))) = true
val test_check_pat_10 = check_pat (ConstructorP("x", (TupleP [Wildcard, Variable "x", ConstructorP("y", TupleP [Variable "x"])]))) = false
val test_check_pat_11 = check_pat (ConstructorP("x", (ConstructorP("y", TupleP [Variable "x", Variable "y"])))) = true
val test_check_pat_12 = check_pat (ConstructorP("x", (ConstructorP("y", TupleP [Variable "x", Variable "x"])))) = false
val test_check_pat_13 = check_pat (TupleP [Wildcard, Variable "x", TupleP [Variable "y"]]) = true
val test_check_pat_14 = check_pat (TupleP[TupleP[Variable "x",ConstructorP ("wild",Wildcard)],Variable "x"]) = false
												 
val test_match_1 = match (Const(1), UnitP) = NONE
val test_match_2 = match (Const(1), ConstP 1) = SOME []		     
val test_match_3 = match (Const(1), Variable "s") = SOME [("s", Const(1))]
val test_match_4 = match (Const(1), TupleP [Wildcard]) = NONE
val test_match_5 = match (Const(1), TupleP [ConstP 1]) = NONE
val test_match_6 = match (Tuple [Unit], TupleP [UnitP]) = SOME []
val test_match_7 = match (Tuple [Tuple [Unit]], TupleP [TupleP[UnitP]]) = SOME []
val test_match_8 = match (Tuple [Tuple [Unit]], TupleP [TupleP[UnitP, Variable "x"]]) = NONE
val test_match_9 = match (Tuple [Const(1), Tuple [Unit]], TupleP [ConstP 1, TupleP[UnitP]]) = SOME []
val test_match_10 = match (Tuple [Const(1), Tuple [Unit, Const(2)]], TupleP [ConstP 1, TupleP[UnitP, Variable("s")]]) = SOME [("s", Const(2))]
val test_match_11 = match (Tuple [Const(1), Tuple [Unit, Const(2)]], TupleP [ConstP 2, TupleP[UnitP, Variable("s")]]) = NONE
val test_match_12 = match (Tuple [Const(1), Tuple [Unit, Const(2)]], TupleP [ConstP 1, TupleP[UnitP, Variable("s"), Wildcard]]) = NONE

val test_match_13 = first_match Unit [UnitP] = SOME []
val test_match_14 = first_match Unit [Variable ("s")] = SOME [("s", Unit)]
val test_match_15 = first_match (Tuple [Const(1), Tuple [Unit, Const(2)]]) [(TupleP [ConstP 1, TupleP[UnitP, Variable("s")]])] = SOME [("s", Const(2))]
val test_match_16 = first_match (Tuple [Const(1), Tuple [Unit, Const(2)]]) [(TupleP [ConstP 1, TupleP[UnitP, ConstP 3]])] = NONE

val test_typecheck_patterns_1 = typecheck_patterns([("a","s",IntT)], [ConstP 10, Variable "a"])
val test_typecheck_patterns_2 = typecheck_patterns([("a","s",UnitT)], [ConstP 10, Variable "a", ConstructorP("SOME",Variable "x")])
val test_typecheck_patterns_3 = typecheck_patterns([("a","s",TupleT[IntT, IntT, IntT])], [TupleP[Variable "a", ConstP 10, Wildcard], TupleP[Variable "b", Wildcard, ConstP 11], Wildcard])
val test_typecheck_patterns_4 = typecheck_patterns([], [ConstructorP("Red", UnitP), Wildcard])
val test_typecheck_patterns_5 = typecheck_patterns([], [ConstructorP("Sedan", Variable "a"), ConstructorP("Truck", TupleP[Variable "b", Wildcard]), Wildcard])
val test_typecheck_patterns_6= typecheck_patterns([], [Wildcard, TupleP[Wildcard, TupleP[Wildcard, UnitP]]])
