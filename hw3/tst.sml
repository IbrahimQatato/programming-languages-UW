use "hw.sml";
val test1 = only_capitals ["A","B","C"] = ["A","B","C"]
val test2 = longest_string1 ["A","bc","C"] = "bc"
val test3 = longest_string2 ["A","bc","Cb"] = "Cb"
val test4a = longest_string3 ["A","bc","C"] = "bc"
val test4b = longest_string4 ["A","B","C"] = "C"
val test5 = longest_capitalized ["A","bc","C"] = "A"
val test6 = rev_string "abc" = "cba"
val test7 = first_answer (fn x => if x > 3 then SOME x else NONE) [1,2,3,4,5] = 4
val test8 = all_answers (fn x => if x = 1 then SOME [x] else NONE) [2,3,4,5,6,7] = NONE
val test8_1 = all_answers (fn x => if x = 1 then SOME [x] else NONE) [] = SOME[]
val test9a = count_wildcards Wildcard = 1
val test9a_1 = count_wildcards (TupleP [Wildcard, Wildcard, Wildcard]) = 3
val test9a_2 = count_wildcards (ConstructorP ("test",TupleP [Wildcard, Wildcard, Wildcard])) = 3
val test9b = count_wild_and_variable_lengths (Variable("a")) = 1
val test9b_1 = count_wild_and_variable_lengths (Variable("abc")) = 3
val test9c = count_some_var ("x", Variable("x")) = 1
val test9c_1 = count_some_var ("x",TupleP [Variable("x"), Variable("x"), Variable("y")]) = 2
val test10 = check_pat (TupleP [Variable("x"), Variable("x"), Variable("y")]) = false
val test10_1 = check_pat (TupleP[Variable "x",Variable "x"]) = false
val test11 = match (Const(1), UnitP) = NONE
val test11_1 = match (Const(1), ConstP 1) = SOME[]
val test11_2 = match (Const 2, Variable "a") = SOME[("a", Const 2)]
val test11_3 = match (Tuple [Unit, Const 3, Const 4], TupleP [Variable("x"), Variable("x"), Variable("y")]) = SOME[("x", Unit),("x", Const 3), ("y", Const 4)]
val test11_4 = match (Constructor ("s",Const 2), ConstructorP ("s",Variable "a")) = SOME[("a", Const 2)]
val test12 = first_match Unit [UnitP] = SOME []
