(* Homework2 Simple Test *)
(* These are basic test cases. Passing these tests does not guarantee that your code will pass the actual homework grader *)
(* To run the test, add a new line to the top of this file: use "homeworkname.sml"; *)
(* All the tests should evaluate to true. For example, the REPL should say: val test1 = true : bool *)
use "hw2.sml";
val test1 = all_except_option ("string", ["a", "string"]) = SOME ["a"]
val test1_1 = all_except_option ("string", []) = NONE
val test1_2 = all_except_option ("string", ["string","b","x"])= SOME(["b","x"])
						     
val test2 = get_substitutions1 ([["foo","far" ],["there","foo", "fighters"]], "foo") = ["far","there", "fighters"]
val test2_1 = get_substitutions1([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]],"Fred") = ["Fredrick","Freddie","F"] 
val test3 = get_substitutions2 ([["foo"],["there"]], "foo") = []
val test4 = similar_names ([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]], {first="Fred", middle="W", last="Smith"})=
	    [{first="Fred", last="Smith", middle="W"}, {first="Fredrick", last="Smith", middle="W"},
	     {first="Freddie", last="Smith", middle="W"}, {first="F", last="Smith", middle="W"}]
val test5 = card_color (Clubs, Num 2) = Black
val test5_1 = card_color (Diamonds, Num 2) = Red
val test6 = card_value (Clubs, Num 2) = 2
val test6_1 = card_value (Clubs, Ace) = 11
val test6_2 = card_value (Clubs, Queen) = 10
val test7 = remove_card ([(Hearts, Ace)], (Hearts, Ace), IllegalMove) = []
val test7_1 = (remove_card ([(Hearts,Num 9)], (Hearts, Ace), IllegalMove) handle
	       IllegalMove => [(Hearts, Ace)]) = [(Hearts, Ace)]
val test7_2 = remove_card ([(Hearts, Ace),(Hearts,Num 9)], (Hearts, Ace), IllegalMove) = [(Hearts,Num 9)]				     
val test7_3 = remove_card ([(Hearts,Num 9),(Hearts, Ace),(Diamonds, Queen)], (Hearts, Ace), IllegalMove) = [(Hearts,Num 9),(Diamonds,Queen)]
val test8 = all_same_color [(Hearts, Ace), (Diamonds, Ace)] = true
val test8_1 = all_same_color [(Hearts, Ace), (Diamonds, Ace),(Clubs, Ace)] = false
val test9 = sum_cards [(Clubs, Num 2),(Clubs, Num 2)] = 4
val test10 = score ([(Hearts, Num 2),(Diamonds, Num 4)],10) = 2
val test11 = officiate ([(Hearts, Num 2),(Clubs, Num 4)],[Draw], 15) = 6
val test12 = officiate ([(Clubs,Ace),(Spades,Ace),(Clubs,Ace),(Spades,Ace)],
                        [Draw,Draw,Draw,Draw,Draw],
                        42)
             = 3
val test13 = ((officiate([(Clubs,Jack),(Spades,Num(8))],
                         [Draw,Discard(Hearts,Jack)],
                         42);
               false) 
              handle IllegalMove => true)
