(* Coursera Programming Languages, Homework 3, Provided Code *)

exception NoAnswer

datatype pattern = Wildcard
		 | Variable of string
		 | UnitP
		 | ConstP of int
		 | TupleP of pattern list
		 | ConstructorP of string * pattern

datatype valu = Const of int
	      | Unit
	      | Tuple of valu list
	      | Constructor of string * valu

fun g f1 f2 p =
    let 
	val r = g f1 f2 
    in
	case p of
	    Wildcard          => f1 ()
	  | Variable x        => f2 x
	  | TupleP ps         => List.foldl (fn (p,i) => (r p) + i) 0 ps
	  | ConstructorP(_,p) => r p
	  | _                 => 0
    end

(**** for the challenge problem only ****)

datatype typ = Anything
	     | UnitT
	     | IntT
	     | TupleT of typ list
	     | Datatype of string

(**** you can put all your code here ****)

fun only_capitals (listOfString : string list) =
    List.filter(fn s => Char.isUpper(String.sub(s, 0))) listOfString

fun longest_string1 (listOfString : string list) =
    List.foldl (fn (s1, s2) => if String.size s1 > String.size s2 then s1 else s2) "" listOfString

fun longest_string2 (listOfString : string list) =
    List.foldl (fn (s1, s2) => if String.size s1 >= String.size s2 then s1 else s2) "" listOfString

fun longest_string_helper ineq listOfString =
    List.foldl (fn (s1, s2) => if ineq(String.size s1, String.size s2) then s1 else s2) "" listOfString
	
val longest_string3 = longest_string_helper (fn (n1, n2) =>  n1 > n2)

val longest_string4 = longest_string_helper (fn (n1, n2) =>  n1 >= n2)

val longest_capitalized = longest_string3 o only_capitals

fun rev_string s = (String.implode o List.rev o String.explode) s

fun first_answer f values =
    case values of
	[] => raise NoAnswer
      | v :: rest => case f v of
			    SOME anws => anws
			  | NONE => first_answer f rest

fun all_answers f values =
    let fun all_accumu elements accumu =
	    case elements of 
		[] => SOME accumu
	      | v :: rest => ( all_accumu rest (accumu @ (first_answer f [v])) ) handle NoAnswer => NONE
    in
	all_accumu values []
    end

fun count_wildcards p = (g (fn x => 1) (fn v => 0) p)

fun count_wild_and_variable_lengths p = (g (fn x => 1) (fn v => String.size v) p)

fun count_wildcards p = (g (fn x => 1) (fn v => 0) p)

fun count_some_var pairStrPatt =
    case pairStrPatt of
	(s, p) => g (fn x => 0) (fn x => if x = s then 1 else 0) p
		    
fun check_pat p =
    let
	fun all p =
	    case p of
		Variable x        => [x]
	      | TupleP ps         => (List.foldl (fn (p, a) => all p @ a) [] ps)
	      | ConstructorP(_,p) => all p
	      | _                 => []

	fun is_all_distinct strList =
	    case strList of
		[] => true
	      | element :: rest => not (List.exists (fn x => x = element) rest) andalso (is_all_distinct rest)
    in
	is_all_distinct(all p)
    end

fun match vp =
    case vp of
	(_, Wildcard) => SOME []
      | (v, Variable s) => SOME [(s, v)]
      | (Unit, UnitP) => SOME []
      | (Const c, ConstP cp) => if c = cp then SOME [] else NONE
      | (Tuple vaList, TupleP pattList) => if List.length vaList = List.length pattList
					   then all_answers (fn (v, p) => match(v, p)) (ListPair.zip (vaList, pattList))
					   else NONE
      | (Constructor (s1, v), ConstructorP (s2, p)) => if s1 = s2 then match(v, p) else NONE
      | _ => NONE

fun first_match v pattList = ( SOME(first_answer (fn p => match (v, p)) pattList) ) handle NoAnswer => NONE
    
fun typecheck_patterns (tList : (string * string * typ) list, pList : pattern list) =
    let
	fun convert_patt p =
	    case p of
		Wildcard => Anything
	      | Variable s => Anything
	      | UnitP => UnitT
	      | ConstP x => IntT
	      | TupleP pattList => TupleT (List.map convert_patt pattList)
	      | ConstructorP (s, patt) => Datatype s

	fun check_type t pTypList =
	    case (t, pTypList) of
		(_, []) => SOME t
	      | (_, Anything :: rest) => check_type t rest
	      | (TupleT tList, (TupleT pList) :: rest) => if List.exists (fn x => x = NONE) (List.map (fn ty => check_type ty pList) tList)
							  then NONE
							  else check_type t rest
	      | (_, p :: _) => if t = p then SOME t else NONE
						   
	val pattToTyp = List.map convert_patt pList
	val checkedList = List.map (fn (s1, s2, t) => check_type t pattToTyp) tList
    in
	case checkedList of
	    [] => NONE
	  | x :: xs => x
    end		 

