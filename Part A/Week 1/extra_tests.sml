use "extra.sml";

val test_alternate_1 = alternate [1, 2, 3, 4] = ~2;
val test_alternate_2 = alternate [] = 0;
val test_alternate_3 = alternate [~100] = ~100;
val test_alternate_4 = alternate [1, ~2, 3, ~4] = 10;
val test_alternate_5 = alternate [~1, 2, ~3, 4] = ~10;

val test_min_max_1 = min_max [3, 1, 2, 5, 4] = (1, 5);
val test_min_max_2 = min_max [1] = (1, 1);
val test_min_max_3 = min_max [~1000000, 1, 1, 1, 1000000] = (~1000000, 1000000);


val test3_1 = cumsum [1,4,20] = [1,5,25];
val test3_2 = cumsum [1,20,20] = [1,21,41];
val test3_3 = cumsum [1] = [1];

val test_greeting_1 = greeting (SOME "Charilaos") = "Hello there, Charilaos!";
val test_greeting_2 = greeting (SOME "student") = "Hello there, student!";
val test_greeting_3 = greeting NONE = "Hello there, you!";

val test_add_opt_1 = add_opt (SOME 1, SOME 2) = SOME 3;
val test_add_opt_2 = add_opt (SOME 1, NONE) = NONE;
val test_add_opt_3 = add_opt (NONE, SOME 2) = NONE;
val test_add_opt_4 = add_opt (NONE, NONE) = NONE;
val test_add_opt_5 = add_opt (SOME ~123, SOME 15) = SOME ~108;

val test_add_all_opt_1 = add_all_opt [SOME 1, NONE, SOME 3] = SOME 4;
val test_add_all_opt_2 = add_all_opt [] = NONE;
val test_add_all_opt_3 = add_all_opt [NONE, NONE, NONE] = NONE;
val test_add_all_opt_4 = add_all_opt [SOME 123] = SOME 123;
val test_add_all_opt_5 = add_all_opt [NONE, SOME ~1, NONE, NONE] = SOME ~1;

val test_any_1 = any([]) = false;
val test_any_2 = any([false,false,false]) = false;
val test_any_3 = any([true,false,false]) = true;
val test_any_4 = any([false,true,false]) = true;

val test_all_1 = all([]) = true;
val test_all_2 = all([true,true,true]) = true;
val test_all_3 = all([false,true,true]) = false;
val test_all_4 = all([true,false,true]) = false;

val test_zip_1 = zip ([1, 2, 3], [4, 6]) = [(1, 4), (2, 6)];
val test_zip_2 = zip ([], [4, 6]) = [];
val test_zip_3 = zip ([1, 2, 3], []) = [];
val test_zip_4 = zip ([], []) = [];
val test_zip_5 = zip ([1, 2], [4, 6, 8]) = [(1, 4), (2, 6)];
val test_zip_6 = zip ([1, 2, 3], [4, 6, 8]) = [(1, 4), (2, 6), (3, 8)];

val test_zip_recycle_1 = zip_recycle ([1, 2, 3], [4, 6]) = [(1, 4), (2, 6), (3, 4)];
val test_zip_recycle_2 = zip_recycle ([], [4, 6]) = [];
val test_zip_recycle_3 = zip_recycle ([1, 2, 3], []) = [];
val test_zip_recycle_4 = zip_recycle ([], []) = [];
val test_zip_recycle_5 = zip_recycle ([1, 2], [4, 6, 8]) = [(1, 4), (2, 6), (1, 8)];
val test_zip_recycle_6 = zip_recycle ([1, 2, 3], [4, 6, 8]) = [(1, 4), (2, 6), (3, 8)];

val test_zip_opt_1 = zip_opt ([1, 2, 3], [4, 6]) = NONE;
val test_zip_opt_2 = zip_opt ([], [4, 6]) = NONE;
val test_zip_opt_3 = zip_opt ([1, 2, 3], []) = NONE;
val test_zip_opt_4 = zip_opt ([], []) = SOME [];
val test_zip_opt_5 = zip_opt ([1, 2], [4, 6, 8]) = NONE;
val test_zip_opt_6 = zip_opt ([1, 2, 3], [4, 6, 8]) = SOME [(1, 4), (2, 6), (3, 8)];

val test_lookup_1 = lookup ([], "totem") = NONE;
val test_lookup_2 = lookup ([("totem" ,1), ("asd", 2), ("tt", 3)], "totem") = SOME 1;
val test_lookup_3 = lookup ([("tote", 1), ("totem", 2), ("tt", 3)], "totem") = SOME 2;
val test_lookup_4 = lookup ([("totem", 1), ("asd", 2), ("tt", 3)], "") = NONE;
val test_lookup_5 = lookup ([("totem" ,1), ("asd", 2), ("tt", 3)], "tt") = SOME 3;

val test_splitup_1 = splitup ([]) = ([], []);
val test_splitup_2 = splitup ([1, 2, 3, 4, ~1, ~2, ~3]) = ([1, 2, 3, 4], [~1, ~2, ~3]);
val test_splitup_3 = splitup ([1, 2, 3, 4]) = ([1, 2, 3, 4], []);
val test_splitup_4 = splitup ([1, 2, 3, 4, ~1]) = ([1, 2, 3, 4], [~1]);
val test_splitup_5 = splitup ([~1, 2, 3, 4, 1, ~2, ~3]) = ([2, 3, 4, 1], [~1, ~2, ~3]);

val test_split_at_1 = split_at ([], 2) = ([], []);
val test_split_at_2 = split_at ([1, 2, 3, ~2, ~3], 2) = ([2, 3], [1, ~2, ~3]);
val test_split_at_3 = split_at ([1, 2, 3, ~2, ~3], 10) = ([], [1, 2, 3, ~2, ~3]);
val test_split_at_4 = split_at ([~3, ~2, 10, 30, ~10], 4) = ([10, 30], [~3, ~2, ~10]);
val test_split_at_5 = split_at ([4], 1) = ([4], []);

val test_is_sorted_1 = is_sorted ([]) = true;
val test_is_sorted_2 = is_sorted ([1, 2, 3, 4, 5, 6]) = true;
val test_is_sorted_3 = is_sorted ([1]) = true;
val test_is_sorted_4 = is_sorted ([1, 2, 3, 4, 1]) = false;

val test_is_any_sorted_1 = is_any_sorted ([]) = true;
val test_is_any_sorted_2 = is_any_sorted ([1, 2, 3, 4, 5, 6]) = true;
val test_is_any_sorted_3 = is_any_sorted ([1]) = true;
val test_is_any_sorted_4 = is_any_sorted ([1, 2, 3, 4, 1]) = false;
val test_is_any_sorted_5 = is_any_sorted ([4, 3, 2, 1]) = true;
val test_is_any_sorted_6 = is_any_sorted ([4, 3, 2, 4, 1]) = false;
val test_is_any_sorted_7 = is_any_sorted ([2, 1]) = true;

val test_sorted_merge_1 = sorted_merge ([1, 4, 7], [5, 8, 9]) = [1, 4, 5, 7, 8, 9];
val test_sorted_merge_2 = sorted_merge ([1], [5]) = [1, 5];
val test_sorted_merge_3 = sorted_merge ([], [5, 8, 9]) = [5, 8, 9];
val test_sorted_merge_4 = sorted_merge ([], []) = [];
val test_sorted_merge_5 = sorted_merge ([5, 8, 9], [1, 4, 7]) = [1, 4, 5, 7, 8, 9];

val test_q_sort_1 = q_sort ([]) = [];
val test_q_sort_2 = q_sort ([1]) = [1];
val test_q_sort_3 = q_sort ([1, 2, 3, 4, 5]) = [1, 2, 3, 4, 5];
val test_q_sort_4 = q_sort ([4, 2, 3, 1, 5]) = [1, 2, 3, 4, 5];
val test_q_sort_5 = q_sort ([5, 4, 3, 2, 1]) = [1, 2, 3, 4, 5];
val test_q_sort_6 = q_sort ([1, 4, 3, 3]) = [1, 3, 3, 4];
val test_q_sort_7 = q_sort ([1, 4, 3, 3, 5, 10, 9, 7, 8, 6, 2]) = [1, 2, 3, 3, 4, 5, 6, 7, 8, 9, 10];

val test_divide_1 = divide ([]) = ([], []);
val test_divide_2 = divide ([1]) = ([1], []);
val test_divide_3 = divide ([1,2,3,4,5]) = ([1,3,5], [2,4]);
val test_divide_4 = divide ([1,2,3,4]) = ([1,3], [2,4]);
val test_divide_5 = divide ([1,2]) = ([1], [2]);

val test_not_so_quick_sort_1 = not_so_quick_sort ([]) = [];
val test_not_so_quick_sort_2 = not_so_quick_sort ([1]) = [1];
val test_not_so_quick_sort_3 = not_so_quick_sort ([1, 2, 3, 4, 5]) = [1, 2, 3, 4, 5];
val test_not_so_quick_sort_4 = not_so_quick_sort ([4, 2, 3, 1, 5]) = [1, 2, 3, 4, 5];
val test_not_so_quick_sort_5 = not_so_quick_sort ([5, 4, 3, 2, 1]) = [1, 2, 3, 4, 5];
val test_not_so_quick_sort_6 = not_so_quick_sort ([1, 4, 3, 3]) = [1, 3, 3, 4];
val test_not_so_quick_sort_7 = not_so_quick_sort ([1, 4, 3, 3, 5, 10, 9, 7, 8, 6, 2]) = [1, 2, 3, 3, 4, 5, 6, 7, 8, 9, 10];

val test_full_divide_1 = full_divide(2, 40) = (3, 5);
val test_full_divide_2 = full_divide(2, 27) = (0, 27);
val test_full_divide_3 = full_divide(3, 27) = (3, 1);
val test_full_divide_4 = full_divide(1, 0) = (0, 0);
val test_full_divide_5 = full_divide(0, 10) = (0, 10);
val test_full_divide_6 = full_divide(2, 2) = (1, 1);

val test_factorize_1 = factorize(1) = [];
val test_factorize_2 = factorize(0) = [];
val test_factorize_3 = factorize(20) = [(2,2), (5,1)];
val test_factorize_4 = factorize(36) = [(2,2), (3,2)];
val test_factorize_5 = factorize(35) = [(5,1), (7,1)];
val test_factorize_6 = factorize(100) = [(2,2), (5,2)];

val test_multiply_1 = multiply([]) = 1;
val test_multiply_2 = multiply([(0, 0)]) = 0;
val test_multiply_3 = multiply([(1, 1)]) = 1;
val test_multiply_4 = multiply([(1, 0)]) = 0;
val test_multiply_5 = multiply([(0, 1)]) = 0;
val test_multiply_6 = multiply([(2, 2), (5, 1)]) = 20;
val test_multiply_7 = multiply([(2, 2), (3, 2)]) = 36;
val test_multiply_8 = multiply([(5, 1), (7, 1)]) = 35;
val test_multiply_9 = multiply([(2, 2), (5, 2)]) = 100;

val test_all_products_1 = all_products ([]) = [];
val test_all_products_2 = all_products ([(1, 0)]) = [];
val test_all_products_3 = all_products ([(0, 1)]) = [0];
val test_all_products_4 = all_products ([(1, 1)]) = [1];
val test_all_products_5 = all_products ([(2,3), (3,2)]) = [1, 2, 3, 4, 6, 8, 9,
12, 18, 24, 36, 72];
val test_all_products_6 = all_products ([(2,2), (5,1)]) = [1, 2, 4, 5, 10, 20];
val test_all_products_7 = all_products ([(2,2), (3,2)]) = [1, 2, 3, 4, 6, 9, 12,
18, 36];
val test_all_products_8 = all_products ([(5,1), (7,1)]) = [1, 5, 7];
val test_all_products_9 = all_products ([(2,2), (5,2)]) = [1, 2, 4, 5, 10, 20, 25, 50, 100];

