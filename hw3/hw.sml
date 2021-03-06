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


(**** you can put all your code here ****)
fun only_capitals ls =
    List.filter(fn x => Char.isUpper (String.sub(x, 0)))  ls
fun longest_string1 slist =
    List.foldl (fn (s,init) =>if String.size s > String.size init
			      then s
			      else init) ""  slist
fun longest_string2 slist =
    List.foldl (fn (s,init) =>if String.size s >= String.size init
			      then s
			      else init) ""  slist
fun longest_string_helper f slist =
    List.foldl (fn(x,y) => if f (String.size x, String.size y) then x else y)
	       ""  slist 
val longest_string3 = longest_string_helper (fn(x,y)=> x>y)
val longest_string4 = longest_string_helper (fn(x,y)=> x>=y)
fun longest_capitalized slist = longest_string3(only_capitals slist)
fun rev_string s = (String.implode o List.rev o String.explode) s
fun first_answer f ls = 
    case ls of
	[] => raise NoAnswer
      | x::xs' => case f x of
		      SOME e => e
		    | NONE => first_answer f xs'
fun all_answers f ls =
    let fun helper (f, ls, acc) =
	    case ls of
		[] => acc
	      | x::xs' => case f x of
			      SOME e => helper(f, xs', SOME( (valOf acc) @ e))
			    | NONE => helper(f, [], NONE)
    in
	helper(f,ls, SOME [])
    end
fun count_wildcards p = g (fn ()=> 1) (fn x => 0) p
fun count_wild_and_variable_lengths p = g (fn ()=> 1) (fn x => String.size x) p
fun count_some_var (s,p) = g (fn ()=> 0) (fn x => if x=s then 1 else 0) p
fun check_pat p =
    let fun helper p =
	    case p of
		Variable x => [x]
	      | TupleP ps  => List.foldl (fn(x,acc)=> acc@(helper x)) [] ps
	      | ConstructorP(_,p) => helper p
	      | _ => []
	fun check ls =
	    case ls of
		[] => true
	      | x::[] => true 
	      | x::xs' => not (List.exists (fn y => x=y) xs')
			  andalso check xs'
    in
	check(helper p)
    end
fun match (v, p)=
    case (v,p) of
	(_,Wildcard) => SOME []
      | (v,Variable s) => SOME[(s,v)]
      | (Unit, UnitP) => SOME[] 
      | (Const y, ConstP x) => if x=y
			       then SOME[]
			       else NONE
      | (Tuple vs, TupleP ps) => if List.length vs = List.length ps
				 then all_answers (match) (ListPair.zip(vs,ps))
				 else NONE
      | (Constructor (str1,v),ConstructorP (str2,p)) => if str1=str2
							then match(v,p)
							else NONE
      | (_,_) => NONE
fun first_match v ps =
    let fun f v p = match (v, p)
    in
	SOME (first_answer (f v) ps) handle NoAnswer => NONE
    end

(**** for the challenge problem only ****)

datatype typ = Anything
	     | UnitT
	     | IntT
	     | TupleT of typ list
	     | Datatype of string
(*****           *****)
			       
fun possible_patterns (ts, ps)=
    let fun helper ts p =
	    case p of
		Wildcard =>SOME (Anything)
	      | Variable _ => SOME (Anything)
	      | UnitP => SOME (UnitT)
	      | ConstP _ => SOME (IntT)
	      | TupleP ps =>(case all_answers (helper ts) ps of
				 SOME x => SOME (TupleT x)
			      | NONE => NONE )
	      | ConstructorP (s,innerP) => (if List.exists (fn (con, data,t) => con=s) ts andalso isSome(helper ts innerP)
					    then SOME (Datatype s)
					    else NONE)
    in
	
	all_answers (helper ts) ps(*then (if Some) fold those answers using the function below*)
    end
					 (*
fun typecheck_pattern (t, p)=
    case (t,p) of
	(SOME (Anything),_) => SOME tnext
      | (SOME (UnitT),UnitT) => SOME tnext
      | (SOME (IntT),IntT) => SOME tnext
      | (SOME (TupleT x),TupleT y) => if List.length x = List.length y
				      then all_answers (typecheck_patterns) (ListPair.zip(x,y))
				      else NONE
      | (SOME (Datatype x),Datatype y) => if y=x
					  then SOME tnext
					  else NONE) ts
as far as I went before I got extrememly frustrated, don't know the answer*)
