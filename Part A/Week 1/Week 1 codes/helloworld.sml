(* The first program and the first comment, yey!!! *)

val x = 34;
(* dynamic environment: x --> 34 *)

val y = 17;
(* dynamic environment: x --> 34, y --> 17 *)

val z = (x + y) + (y + 2);
(* dynamic environment: x --> 34, y --> 17, z --> 70 *)

val q = z + 1;

val abs_of_z = if z < 0 then 0 - z else z;

val simple_abs = abs z; (* calling a function. *)

(* 
Less than comparions:
    Syntax: e1 < e2 where e1 and e2 are subexpressions.
    
Type-checking: e1 and e2 must have type int, and the the entire expression is always bool.

Evaluation rules: if e1 is less than e2, then it evaluate true, else 
*)
