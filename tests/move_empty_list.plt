:- begin_tests(move_empty_list).

:- include(refactor_common).

/* $move_empty_list$
diff -ruN move_empty_list.pl -
--- move_empty_list.pl (source)
+++ move_empty_list.pl (target)
@@ -1 +1 @@
-f([A|T], B, [/**/]).
+f([], [A|T], [/**/]).
*/

test(move_empty_list) :-
    execute_test(move_empty_list, replace_term(f(X,B,C), f(B,X,C), B=[]),[file(move_empty_list)]).

:- end_tests(move_empty_list).
