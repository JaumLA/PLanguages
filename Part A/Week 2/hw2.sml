(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string (s1 : string, s2 : string) =
    s1 = s2


(* takes a string and a string list return NONE if the string is not in the 
   list, else return SOME lst where lst is identical to the argument list except the string
   is not in it. You may assume the string is in the list at most once. *)
fun all_except_option (listOfString : string list, str : string) =
    case listOfString of
	[] => NONE
      | element :: restList => if same_string(element, str)
			       then SOME restList
			       else case all_except_option(restList, str) of
					SOME lst => SOME(element :: lst)
				      | NONE => NONE

(* takes a string and a string list return NONE if the string is not in the 
   list, else return SOME lst where lst is identical to the argument list except the string
   is not in it. You may assume the string is in the list at most once. *)
fun all_except_option (listOfString : string list, str : string) =
    case listOfString of
	[] => NONE
      | element :: restList => if same_string(element, str)
			       then SOME restList
			       else case all_except_option(restList, str) of
					SOME lst => SOME(element :: lst)
				      | NONE => NONE

(* takes a string list list (a list of list of strings, the
   substitutions) and a string s and returns a string list. The result 
   has all the strings of the list(s) that contains s, 
   but s itself should not be in the result. *)
fun get_substitutions1 (substitutionList : (string list) list, substitute : string) =
    case substitutionList of
	[] => []
      | listOfString :: restSubstitution => case all_except_option(listOfString, substitute) of
						NONE => get_substitutions1(restSubstitution, substitute)
					      | SOME ListWithout => ListWithout @ get_substitutions1(restSubstitution, substitute)

(* is like get_substitutions1 except it uses a tail-recursive
   local helper function. *)
fun get_substitutions2 (substitutionList : (string list) list, substitute : string) =
    let
	fun get_sub_tail (subList : (string list) list, result : string list) =
	    case subList of
		[] => result
	      | listOfString :: restSub => case all_except_option(listOfString, substitute) of
					       NONE => get_sub_tail(restSub, result)
					     | SOME ListWithout => get_sub_tail(restSub, (result @ ListWithout))
    in
	get_sub_tail(substitutionList, [])
    end


(* takes a string list list of substitutions and a full name 
   of type {first:string,middle:string,last:string} 
   and returns a list of full names (type {first:string,middle:string,last:string} list). 
   The result is all the full names you can produce by substituting for the first 
   name (and only the first name) using substitutions. *)
fun similar_names (substitutionList : (string list) list, {first=f, last=l, middle=m}) =
    let
	fun all_names (allFirstList : string list) =
	    case allFirstList of
		[] => []
	      | sub :: subFirst => {first=sub,last=l,middle=m} :: all_names(subFirst)

	val listExcluded = get_substitutions1(substitutionList, f)
    in
	{first=f,last=l,middle=m} :: all_names(listExcluded)
    end

					   
(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

(* put your solutions for problem 2 here *)

(* take a type card and the color of the card based on its suit. *)
fun card_color (c : suit * rank) =
    case c of
	(Spades, _) => Black
      | (Clubs, _) => Black
      | (Diamonds, _) => Red
      | (Hearts, _) => Red

(* take a type card and return its value. The ace has a value of 11,
   the queens, joker and king has a value of 10, else return the value of the
   number. *)
fun card_value (c : suit * rank) =
    case c of
	(_, Num r) => r
      | (_, Ace) => 11
      | (_, _) => 10

		      

(*  takes a list of cards cs, a card c, and an exception e. It returns a
    list that has all the elements of cs except c. If c is in the list 
    more than once, remove only the first one.
    If c is not in the list, raise the exception e
*)
fun remove_card (cardList : card list, c : card, e : exn) =
  case cardList of
	[] => raise e
      | element :: restList => if c = element
			       then restList
			       else element :: remove_card(restList, c, e)

(* takes a list of cards and returns true if all the cards in the
    list are the same color.*)
fun all_same_color (cardList : card list) =
    case cardList of
	[] => true
      | element :: [] => true
      | element :: secondElem :: restList => card_color(element) = card_color(secondElem) andalso all_same_color(secondElem :: restList)

(* takes a list of cards and returns the sum of their values. Use a locally
   defined helper function that is tail recursive. *)
fun sum_cards (cardList : card list) =
    let
	fun sum_cards_tail (cardList : card list, sum : int) =
	    case cardList of
		[] => sum
	      | element :: restList => sum_cards_tail(restList, sum + card_value(element))
						     
    in
	sum_cards_tail(cardList, 0)
    end

(* Let s be the sum of the values of the held-cards. 
   If s is greater than goal, the preliminary score is three 
   times (s−goal), else the preliminary score is (goal − s). 
   The score is the preliminary score unless all the held-cards are
   the same color, in which case the score is the preliminary 
   score divided by 2 (and rounded down as usual with integer division.
   
  takes a card list (the held-cards) and an int (the goal) and computes
  the score.  *)
fun score (cardList : card list, goal : int) =
    let
	val isAllSameColor = all_same_color(cardList)
	val handSum = sum_cards(cardList)
	val preScore = if handSum > goal then 3 * (handSum - goal) else goal - handSum
    in
	case isAllSameColor of
	    true => preScore div 2
	  | false => preScore
    end

(* It takes a card list (the card-list) a move list (what the player “does”
   at each point), and an int (the goal) and returns the score at the end of the 
   game after processing (some or all of) the moves in the move list in order. 
   • The game starts with the held-cards being the empty list.
   • The game ends if there are no more moves. (The player chose to stop since the move list is empty.)
   • If the player discards some card c, play continues (i.e., make a recursive call) with the held-cards not having c and the card-list unchanged. If c is not in the held-cards, raise the IllegalMove exception.
   • If the player draws and the card-list is (already) empty, the game is over. Else if drawing causes the sum of the held-cards to exceed the goal, the game is over (after drawing). Else play continues with a larger held-cards and a smaller card-list.
   *)
fun officiate (cards : card list, moves : move list, goal : int) =
    let
	fun execute_rounds (cards:card list, moves:move list, hand:card list) =
	    case (cards, moves, (sum_cards(hand) > goal)) of
		([], _, _) => score(hand, goal)
		| (_, [], _) => score(hand, goal)
		| (_, _, true) => score(hand, goal)
		| (_, (Discard c)::restM, _) => execute_rounds(cards, restM, remove_card(hand, c, IllegalMove))
		| ((firstCard::restCards), (Draw)::restM, _) => execute_rounds(restCards, restM, firstCard::hand)
    in
	execute_rounds(cards, moves, [])
    end

	
(* Challenges *)

fun score_challenge (cardList : card list, goal : int) =
    let
	fun count_aces (cardList : card list) =
	    case cardList of
		[] => 0
	      | element :: rest => if card_value(element) = 11
				   then 1 + count_aces(rest)
				   else count_aces(rest)

	val numAces = count_aces(cardList)
	val preHandSum = sum_cards(cardList) - (11*numAces)

	fun one_or_eleven (aces : int, sum : int) =
	    case aces of
		0 => sum
	      | _ => if sum + 11 <= goal
		     then one_or_eleven(aces - 1, sum + 11)
		     else one_or_eleven(aces - 1, sum + 1)
	
	val isAllSameColor = all_same_color(cardList)
	val handSum = one_or_eleven(numAces, preHandSum)
	val preScore = if handSum > goal then 3 * (handSum - goal) else goal - handSum
    in
	case isAllSameColor of
	    true => preScore div 2
	  | false => preScore
    end

fun officiate_challenge (cards : card list, moves : move list, goal : int) =
    let
	fun execute_rounds (cards:card list, moves:move list, hand:card list) =
	    case (cards, moves, (sum_cards(hand) > goal)) of
		([], _, _) => score_challenge(hand, goal)
	      | (_, [], _) => score_challenge(hand, goal)
	      | (_, _, true) => score_challenge(hand, goal)
	      | (_, (Discard c)::restM, _) => execute_rounds(cards, restM, remove_card(hand, c, IllegalMove))
	      | ((firstCard::restCards), (Draw)::restM, _) => execute_rounds(restCards, restM, firstCard::hand)
    in
	execute_rounds(cards, moves, [])
    end

fun careful_player (cardList : card list, goal : int) =
    let
	fun is_possible_get_zero (hand : card list, handWithout : card list, c : card) =
	    case handWithout of
		[] => []
	      | x :: rest => if score(c::remove_card(hand, x, IllegalMove), goal) = 0
			     then [Discard x]
			     else is_possible_get_zero(hand, rest, c)
			  
	fun accumulate_moves (cardList : card list, hand : card list) =
	    case (cardList, score(hand, goal), goal > sum_cards(hand) + 10) of
		([], _, _) => []
	      | (firstCard :: rest, _, true) => Draw :: accumulate_moves(rest, firstCard :: hand)
	      | (_, 0, _) => []
	      | (firstCard :: rest, _, _) => case is_possible_get_zero(hand, hand, firstCard) of
						 [] => []
					       | c :: _ => [c, Draw]
    in
	accumulate_moves (cardList, [])
    end
