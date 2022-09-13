(* Definitions for problems 1-4. *)
type student_id = int
type grade = int (* must be in 0 to 100 range *)
type final_grade = { id : student_id, grade : grade option }
datatype pass_fail = pass | fail

(* takes a final_grade and returns pass if the grade field contains SOME i for an i >= 75, else fail. *)
fun pass_or_fail (studentGrade : final_grade) =
    case studentGrade of
	{id = _, grade = SOME g} => if g >= 75 then pass else fail
      | {id = _, grade = NONE} => fail

(* takes a final_grade and returns true if the grade i >= 75, else false. *)
fun has_passed (studentGrade : final_grade) =
    case pass_or_fail(studentGrade) of
	pass => true
      | fail => false

(* takes a list of final_grade and return how many list elements have passing grades, grade >= 75. *)
fun number_passed (gradeList : final_grade list) =
    case gradeList of
	[] => 0
      | first :: restList => if has_passed(first)
			     then 1 + number_passed(restList)
			     else number_passed(restList)

(* takes a list of pairs (pass_fail, final_grade) and returns the number of 
   elements mislabeled where mislabeling means a pair (pass, x), where has_passed x
   is false or (fail, x) where has_passed x is true. *)
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

(* takes a 'a tree and returns its height. The height of a tree is 
   the length of the longest path to a leaf. *)
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

(* takes a int tree and return the sum of the nodes. *)
fun sum_tree (t : int tree) =
    case t of
	leaf => 0
      | node {value = i, left = l, right = r} => i + sum_tree(l) + sum_tree(r)

(* takes a flag tree and returns a flag tree with all nodes with flag
   prune_me are replaced with a leaf. *)
fun gardener (flagTree : flag tree) =
    case flagTree of
	leaf => leaf
      | node {value = prune_me, left = _, right = _} => leaf
      | node {value = v, left = l, right = r} => node{value=v, left=gardener(l), right=gardener(r)}

						     
datatype nat = ZERO | SUCC of nat

(* given a nat type number returns whether that number is positive. *)
fun is_positive (num : nat) =
    case num of
	ZERO => true
      | _ => false

exception Negative

(* takes a nat type and returns its predecessor. *)
fun pred (num : nat) =
    case num of
	ZERO => raise Negative
      | SUCC s => s

(* takes a nat type and returns the corresponding int. 
   Example: nat_to_int SUCC(SUCC(ZERO)) = 2 *)
fun nat_to_int (num : nat) =
    case num of
	ZERO => 0
      | SUCC s => 1 + nat_to_int(s)

(* takes an int and returns its nat representation. *)
fun int_to_nat (num : int) =
    case num of
	0 => ZERO
      | _ => SUCC(int_to_nat(num - 1))

(* add two nat types. *)
fun add (num1 : nat, num2 : nat) =
    case num1 of
	ZERO => num2
      | SUCC s => SUCC(add(s, num2))

(* subtract two nat types. *)
fun sub (num1 : nat, num2 : nat) =
    case num2 of
	ZERO => num1
      | SUCC s => sub(pred(num1), pred(num2))

(* multiply two nat types. *)
fun mult (num1 : nat, num2 : nat) =
    case (num1, num2) of
	(ZERO, _) => ZERO
      | (_, ZERO) => ZERO
      | (SUCC ZERO, _) => num1
      | (_, SUCC ZERO) => num1
      | (_, SUCC s) => add(num1, mult(num1, s))

(* takes two nat types, n1 and n2, and returns true if the first argument
   is less than the second. *)
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
	
(* take an intSet type and return true if the set is empty. *)
fun isEmpty (set : intSet) =
  let

    (* given two lists return true if there is at least one element in common
       in both lists. *) 
    fun share_element (s1 : int list, s2 : int list) = 
        let
          fun helper (num : int, listtocomp : int list) =
            case listtocomp of
                 [] => false
               | x :: rest => num = x orelse helper(num, rest)

        in
          case (s1, s2) of
            ([], _) => false
          | (_, []) => false
          | (ele1 :: rest1, _) => helper(ele1, s2) orelse share_element(rest1, s2)
        end
        
    (* helper function to check for empty intersection intSet. Expected
    * Intersection type for first argument. *)
    fun checkInter (pair : intSet * intSet) =
        case pair of
            (Elems s1, Elems s2) => not (share_element(s1, s2)) 

            (* check when the elements of Elems are out of range, take the
            * negation of that. *)
          | (Elems (ele1 :: rest), Range {from = f, to = t}) => not ((ele1 > t orelse ele1 < f)
                                                                orelse checkInter(Elems rest, Range {from = f, to = t}))
          | (Range r, Elems l) => checkInter(Elems l, Range r)

          (* check the cases when there is no intersection *)
          | (Range {from=f1, to=t1}, Range {from=f2, to=t2}) => if f1 > f2 
                                                             then checkInter(Range {from=f2,to=t2}, Range {from=f1,to=t1}) 
                                                             else (f1 < f2 andalso t1 < f2) orelse (f1 > t2 andalso t1 > t2)
          | (Intersection i, s) => checkInter(i) orelse isEmpty(s)
          | (s, Intersection i) => checkInter(i) orelse isEmpty(s)
          | (Union (u1, u2), s) => isEmpty(u1) andalso isEmpty(u2) orelse isEmpty(s)
          | (s, Union(u1, u2)) =>isEmpty(u1) andalso isEmpty(u2) orelse isEmpty(s)

    in
        case set of
            Elems [] => true
          | Range {from = f, to = t} => f = 0 andalso t = 0
          | Union (i1, i2) => isEmpty(i1) andalso isEmpty(i2)
          | Intersection i => checkInter(i)
          | _ => false
    end

(* take a an intSet type and an int, if the intSet has the int value
  returns true, else false. *)
fun contains (set : intSet, num : int) =
  case set of
       Elems [] => false
     | Elems (l :: rest) => if l = num then true else contains(Elems rest, num)
     | Range {from = low, to = high} => low <= num andalso high >= num
     | Union (s1, s2) => contains(s1, num) orelse contains(s2, num)
     | Intersection (s1, s2) => contains(s1, num) andalso contains(s2, num)
