(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)
fun all_except_option (word , ls) =
    let fun aux (word, ls)=
	    case ls of
		[] =>[]
	      | x::y  => if (same_string(word, x))
			 then aux(word, y)
			 else  x::aux(word,y)
    in
	let val result = aux(word, ls)
	in
	    if ls = result
	    then NONE
	    else SOME result
	end
    end
fun get_substitutions1 (lsls, name)=
    case lsls of
	[] => []
      | x::y =>let val matches = all_except_option(name, x)
	       in
		   case matches of
		       NONE => get_substitutions1(y, name)
		    | SOME t => t @ get_substitutions1(y, name)
		end				  
fun get_substitutions2 (lsls, name)=
    let fun aux(lsls, name, acc)=
	     case lsls of
		 [] => acc
	       | x::y =>let val matches = all_except_option(name, x)
			in
			    case matches of
				NONE =>aux(y,name,acc)
			      | SOME t =>aux(y,name, acc@t)
			end
    in
	aux(lsls, name, [])
    end
fun similar_names (lsls, {first=f, middle=m, last=l})=
    let val matches =f::get_substitutions2(lsls,f)
	fun helper(m, l, ls)=
	    case ls of
		[] => []
	      | x::y => {first = x, middle = m, last = l}::helper(m,l,y)
    in
	helper(m,l,matches)
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
fun card_color (s, r)=
    case s of
	Clubs => Black
      | Spades => Black
      | _ => Red
fun card_value (s, r)=
    case r of
	Num x => x
      | Ace => 11
      | _ => 10 
fun remove_card (cs, c, e)=
    case cs of
	[] => raise e
       |x::y => if x = c
		then y
		else x::remove_card(y, c, e)
fun all_same_color (cs)=
    case cs of
	[] => true
      | x::[] => true
      | x::(y::z) => if card_color(x)=card_color(y)
		     then all_same_color(y::z)
		     else false
fun sum_cards (hcs)=
    let fun aux (hcs,acc)=
	case hcs of
	    []=> acc
	  | x::y => aux(y,card_value(x)+acc)
    in
	aux(hcs, 0)
    end
fun score (hcs, goal)=
    let val sum= sum_cards(hcs)
    in let val result = if sum> goal
		    then 3*(sum-goal)
		    else goal-sum
       in
	    if all_same_color(hcs)
	    then result div 2
	    else result
       end
    end
fun officiate (cs, moves, goal)=
    let fun helper (cs, moves, goal, held)=
	    case moves of
		[] => (held, goal)
	      | Discard c::moves' => helper(cs,moves',goal,remove_card(held, c, IllegalMove))
	      | Draw::moves' => case cs of
			    [] => (held, goal)
			  | x::y =>let val nextHand = sum_cards(x::held)
				   in if nextHand>goal
				      then (x::held, goal)
				      else helper(y,moves', goal, x::held)
				   end
    in
	score(helper(cs,moves,goal,[]))
    end

fun best_move (next,goal, held)=
    let fun aux (n, goal, h1, h2)=
	    case h2 of
		[] => NONE
	      | a::b => if score(n::remove_card(h1,a,IllegalMove),goal) = 0
			then SOME a
			else aux(n, goal, h1, b)
    in
	aux(next, goal, held, held)
    end
				 
fun careful_player (cs, goal)=
    let fun helper (cs,goal,held)=
	    let val sum = sum_cards(held)
		val score = score(held,goal)
	    in
		if goal-sum>10
		then case cs of
			 [] => [Draw]
			| c::cs' => Draw::helper(cs',goal,c::held)
		else
		    if score = 0
		    then
			[]
		    else
			case cs of
			    [] => []
			  | c::cs' => case best_move(c,goal,held) of
					  NONE => []
					| SOME e => Discard e:: [Draw]
	    end
    in
	helper(cs,goal,[])
    end
	
