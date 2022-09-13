use "extra.sml";

val test_pass_or_fail_1 = pass_or_fail { id = 1023, grade = SOME 73 } = fail
val test_pass_or_fail_2 = pass_or_fail { id = 1, grade = SOME 48 } = fail
val test_pass_or_fail_3 = pass_or_fail { id = 10231023, grade = SOME 0 } = fail
val test_pass_or_fail_4 = pass_or_fail { id = 1729, grade = NONE } = fail
val test_pass_or_fail_5 = pass_or_fail { id = 432, grade = SOME 74 } = fail
val test_pass_or_fail_6 = pass_or_fail { id = 2, grade = SOME 75 } = pass
val test_pass_or_fail_7 = pass_or_fail { id = 13, grade = SOME 100 } = pass
val test_pass_or_fail_8 = pass_or_fail { id = 15, grade = SOME 86 } = pass

val test_has_passed_1 = has_passed { id = 1023, grade = SOME 73 } = false
val test_has_passed_2 = has_passed { id = 1, grade = SOME 48 } = false
val test_has_passed_3 = has_passed { id = 10231023, grade = SOME 0 } = false
val test_has_passed_4 = has_passed { id = 1729, grade = NONE } = false
val test_has_passed_5 = has_passed { id = 432, grade = SOME 74 } = false
val test_has_passed_6 = has_passed { id = 2, grade = SOME 75 } = true
val test_has_passed_7 = has_passed { id = 13, grade = SOME 100 } = true
val test_has_passed_8 = has_passed { id = 15, grade = SOME 86 } = true

val test_number_passed_1 = number_passed [{ id = 1, grade = SOME 65 }, { id = 2, grade = SOME 82 },
					  { id = 3, grade = NONE }, { id = 5, grade = SOME 96 }] = 2
val test_number_passed_2 = number_passed [] = 0						  
val test_number_passed_3 = number_passed [{ id = 12, grade = SOME 100 }, { id = 14, grade = SOME 0 },
					  { id = 9, grade = NONE }, { id = 2, grade = NONE }] = 1			    
val test_number_passed_4 = number_passed [{ id = 1, grade = SOME 76 }, { id = 2, grade = SOME 82 },
					  { id = 5, grade = SOME 96 }] = 3

val test_number_misgraded_1 = number_misgraded([(pass, {id=0, grade=SOME 75})]) = 0;
val test_number_misgraded_2 = number_misgraded([(fail, {id=0, grade=SOME 75})]) = 1;
val test_number_misgraded_3 = number_misgraded([(fail, {id=0, grade=SOME 74})]) = 0;
val test_number_misgraded_4 = number_misgraded([(fail, {id=0, grade=SOME 74}),
                                                (fail, {id=1, grade=SOME 90}),
                                                (pass, {id=2, grade=SOME 60})]) = 2;

				  
val test_tree_height_1 = tree_height (node { value = 0, left = node { value = 0,
								      left = node { value = 0, left = leaf, right = leaf }, right = leaf },
					     right = node { value = 0, left = leaf, right = leaf } }) = 3								    
val test_tree_height_2 = tree_height leaf = 0						
val test_tree_height_3 = tree_height (node { value = "abcde", left = leaf, right = leaf }) = 1		 
val test_tree_height_4 = tree_height (node { value = true, left = leaf, right = leaf }) = 1										      
val test_tree_height_5 = tree_height (node { value = 0, left = leaf,
					     right = node { value = 0, left = node { value = 0, left = leaf, right = leaf }, right = leaf } }) = 3

val test_sum_tree_1 = sum_tree (node { value = 1, left = node { value = 2,
								left = node { value = 3, left = leaf, right = leaf }, right = leaf },
				       right = node { value = 4, left = leaf, right = leaf } }) = 10							      
val test_sum_tree_2 = sum_tree leaf = 0
val test_sum_tree_3 = sum_tree (node { value = 1729, left = leaf, right = leaf }) = 1729
val test_sum_tree_4 = sum_tree (node { value = 32, left = leaf,
    right = node { value = ~60, left = node { value = 17, left = leaf, right = leaf }, right = leaf } }) = ~11

val test_gardener_1 = gardener (node { value = leave_me_alone,
				       left = node { value = prune_me, left = node { value = leave_me_alone, left = leaf, right = leaf }, right = leaf },
				       right = node { value = leave_me_alone, left = leaf, right = leaf } }) =
		      node { value = leave_me_alone, left = leaf, right = node { value = leave_me_alone, left = leaf, right = leaf } }
val test_gardener_2 = gardener leaf = leaf
val test_gardener_3 = gardener (node { value = prune_me, left = node { value = prune_me,
								       left = node { value = leave_me_alone, left = leaf, right = leaf }, right = leaf },
				       right = node { value = leave_me_alone, left = leaf, right = leaf } }) = leaf
val test_gardener_4 = gardener (node { value = leave_me_alone,
				       left = node { value = leave_me_alone, left = node { value = leave_me_alone, left = leaf, right = leaf }, right = leaf },
				       right = node { value = leave_me_alone, left = leaf, right = leaf } }) =
		      node { value = leave_me_alone, left = node { value = leave_me_alone,
								   left = node { value = leave_me_alone, left = leaf, right = leaf }, right = leaf },
			     right = node { value = leave_me_alone, left = leaf, right = leaf } }
val test_gardener_5 = gardener (node { value = leave_me_alone, left = node { value = leave_me_alone,
									     left = node { value = prune_me, left = leaf, right = leaf }, right = leaf },
				       right = node { value = prune_me, left = leaf, right = leaf } }) =
		      node { value = leave_me_alone, left = node { value = leave_me_alone, left = leaf, right = leaf }, right = leaf }
val test_gardener_6 = gardener (node { value = prune_me, left = leaf, right = leaf }) = leaf
val test_gardener_7 = gardener (node { value = leave_me_alone, left = leaf, right = leaf }) =
		      node { value = leave_me_alone, left = leaf, right = leaf }
val test_gardener_8 = gardener (node { value = leave_me_alone, left = leaf,
				       right = node { value = prune_me, left = node { value = prune_me, left = leaf, right = leaf }, right = leaf } }) =
		      node { value = leave_me_alone, left = leaf, right = leaf }

val test_nat_to_int_1 = nat_to_int (ZERO) = 0
val test_nat_to_int_2 = nat_to_int (SUCC ZERO) = 1
val test_nat_to_int_3 = nat_to_int (SUCC(SUCC ZERO)) = 2
			   
val test_int_to_nat_1 = int_to_nat (0) = ZERO
val test_int_to_nat_2 = int_to_nat (1) = SUCC ZERO
val test_int_to_nat_3 = int_to_nat (2) = SUCC(SUCC ZERO)

val test_add_1 = add (ZERO, ZERO) = ZERO
val test_add_2 = add (SUCC ZERO, ZERO) = SUCC ZERO
val test_add_3 = add (SUCC ZERO, SUCC ZERO) = SUCC(SUCC ZERO)
val test_add_4 = add (SUCC(SUCC ZERO), SUCC(SUCC ZERO)) = SUCC(SUCC(SUCC(SUCC ZERO)))

val test_sub_1 = sub (ZERO, ZERO) = ZERO
val test_sub_2 = sub (SUCC ZERO, ZERO) = SUCC ZERO
val test_sub_3 = sub (SUCC ZERO, SUCC ZERO) = ZERO
val test_sub_4 = sub (SUCC(SUCC ZERO), SUCC ZERO) = SUCC ZERO

val test_mult_1 = mult (ZERO, ZERO) = ZERO
val test_mult_2 = mult (SUCC ZERO, ZERO) = ZERO
val test_mult_3 = mult (SUCC ZERO, SUCC ZERO) = SUCC ZERO
val test_mult_4 = mult (SUCC(SUCC ZERO), SUCC ZERO) = SUCC(SUCC ZERO)
val test_mult_5 = mult (SUCC(SUCC ZERO), SUCC(SUCC ZERO)) = SUCC(SUCC(SUCC(SUCC ZERO)))
val test_mult_6 = mult (SUCC ZERO, SUCC(SUCC ZERO)) = SUCC ZERO

val test_less_than_1 = less_than (ZERO, ZERO) = false
val test_less_than_2 = less_than (SUCC ZERO, ZERO) = false
val test_less_than_3 = less_than (ZERO, SUCC ZERO) = true
val test_less_than_4 = less_than (SUCC ZERO, SUCC(SUCC ZERO)) = true
val test_less_than_5 = less_than (SUCC(SUCC ZERO), SUCC ZERO) = false

val test_isEmpty_1 = isEmpty (Elems []) = true
val test_isEmpty_2 = isEmpty (Intersection (Elems [], Elems [1, 2, 3])) = true 
val test_isEmpty_3 = isEmpty (Intersection (Range {from = 2, to = 3}, Elems [1,2])) = false 
val test_isEmpty_4 = isEmpty (Range {from = 0, to = 0}) = true
val test_isEmpty_5 = isEmpty (Range {from = 0, to = 5}) = false
val test_isEmpty_6 = isEmpty (Intersection (Range {from=1, to=4}, Range {from=5, to=6})) = true
val test_isEmpty_7 = isEmpty (Intersection (Intersection (Elems [5], Elems [1, 2, 3]), Union (Elems [1,2,3], Elems []))) = true
val test_isEmpty_8 = isEmpty (Union (Elems [2], Range {from = 0, to = 0})) = false
val test_isEmpty_9 = isEmpty (Union (Elems [], Range {from = 0, to = 0})) = true
val test_isEmpty_10 = isEmpty (Union (Elems [], (Intersection (Elems [], Elems [1, 2, 3])))) = true

val test_contains_1 = contains(Elems [], 23) = false
val test_contains_2 = contains(Elems [5], 30) = false
val test_contains_3 = contains(Elems [2, 6], 6) = true
val test_contains_4 = contains(Range {from = 1, to = 4}, 0) = false
val test_contains_5 = contains(Range {from = 1, to = 4}, 6) = false
val test_contains_6 = contains(Range {from = 1, to = 4}, 2) = true
val test_contains_7 = contains(Union (Elems [1,2], (Elems [3,6])), 5) = false
val test_contains_8 = contains(Union (Elems [1,2,5], (Elems [3,6])), 5) = true

