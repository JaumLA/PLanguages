(* (int list) (int list) --> int list *)
(* Take the list ys and append all its elements to the final of the list
xs *)
fun append (xs : int list, ys : int list) =
    if null xs
    then ys
    else (hd xs) :: append(tl(xs),ys);

fun firsts (xs : (int * int) list) =
    if null xs
    then []
    else #1 (hd xs) :: firsts((tl xs));

(* tests *)
val x = [1,2,3,4,5];
val y = [6,7,8,9,10];
val pair_list = [(3,4),(5,6)];

append(x,y);
firsts pair_list;
