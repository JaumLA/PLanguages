(* Definitions for problems 1-4. *)
type student_id = int
type grade = int (* must be in 0 to 100 range *)
type final_grade = { id : student_id, grade : grade option }
datatype pass_fail = pass | fail

fun pass_or_fail (studentGrade : final_grade) =
    case studentGrade of
	{id = _, grade = SOME g} => if g >= 75 then pass else fail
      | {id = _, grade = NONE} => fail

fun has_passed (studentGrade : final_grade) =
    case pass_or_fail(studentGrade) of
	pass => true
      | fail => false

fun number_passed (gradeList : final_grade list) =
    case gradeList of
	[] => 0
      | first :: restList => if has_passed(first)
			     then 1 + number_passed(restList)
			     else number_passed(restList)

fun number_misgraded (passGradeList : (pass_fail * final_grade) list) =
    case passGradeList of
	[] => 0
      | (p, g) :: restList => if p <> pass_or_fail(g)
			      then 1 + number_misgraded(restList)
			      else number_misgraded(restList)

(* Problems 5-7 use these type definitions. *)
datatype 'a tree = leaf 
                 | node of { value : 'a, left : 'a tree, right : 'a tree }
datatype flag = leave_me_alone | prune_me

fun tree_height (t : 'a tree) =
    case t of
	leaf => 0
      | node {value = _, left = l, right = r} => let val sumL = 1 + tree_height(l)
						     val sumR = 1 + tree_height(r)
						 in
						     if sumL > sumR
						     then sumL
						     else sumR
						 end

fun sum_tree (t : int tree) =
    case t of
	leaf => 0
      | node {value = i, left = l, right = r} => i + sum_tree(l) + sum_tree(r)

fun gardener (flagTree : flag tree) =
    case flagTree of
	leaf => leaf
      | node {value = prune_me, left = _, right = _} => leaf
      | node {value = v, left = l, right = r} => node{value=v, left=gardener(l), right=gardener(r)}

						     
datatype nat = ZERO | SUCC of nat

fun is_positive (num : nat) =
    case num of
	ZERO => true
      | _ => false

exception Negative
		 
fun pred (num : nat) =
    case num of
	ZERO => raise Negative
      | SUCC s => s

fun nat_to_int (num : nat) =
    case num of
	ZERO => 0
      | SUCC s => 1 + nat_to_int(s)

fun int_to_nat (num : int) =
    case num of
	0 => ZERO
      | _ => SUCC(int_to_nat(num - 1))

fun add (num1 : nat, num2 : nat) =
    case num1 of
	ZERO => num2
      | SUCC s => SUCC(add(s, num2))

fun sub (num1 : nat, num2 : nat) =
    case num2 of
	ZERO => num1
      | SUCC s => sub(pred(num1), pred(num2))

fun mult (num1 : nat, num2 : nat) =
    case (num1, num2) of
	(ZERO, _) => ZERO
      | (_, ZERO) => ZERO
      | (SUCC ZERO, _) => num1
      | (_, SUCC ZERO) => num1
      | (_, SUCC s) => add(num1, mult(num1, s))

fun less_than (num1 : nat, num2 : nat) =
    case (num1, num2) of
	(ZERO, ZERO) => false
      | (ZERO, _) => true
      | (_, ZERO) => false
      | (_, _) => less_than(pred(num1), pred(num2))

(* 17-19 *)
datatype intSet = 
	 Elems of int list (*list of integers, possibly with duplicates to be ignored*)
	 | Range of { from : int, to : int }  (* integers from one number to another *)
	 | Union of intSet * intSet (* union of the two sets *)
	 | Intersection of intSet * intSet (* intersection of the two sets *)

fun intersect_sets (set1 : intSet, set2 : intSet) =
    case set1 of
	Elems => 

	
fun isEmpty (set : intSet) =
    case set of
	Elems [] => true
      | Range {from = f, to = t} => f = 0 andalso t = 0
      | Union (i1, i2) => isEmpty(i1) andalso isEmpty(i2)
      | Intersection (i1, i2) => 
