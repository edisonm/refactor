:- begin_tests(repl_empty_list).

:- include(refactor_common).

/* $repl_empty_list$
diff -ruN repl_empty_list.pl -
--- repl_empty_list.pl (source)
+++ repl_empty_list.pl (target)
@@ -1,2 +1,2 @@
-f([], [P]).
-f([], [P|[]]).
+f([], [x(P)]).
+f([], [x(P)|[]]).
*/

test(repl_empty_list) :-
    execute_test(repl_empty_list, replace_term(f(A,B),f(A,C),(B=[M], C=[x(M)])),[file(repl_empty_list)]).

:- end_tests(repl_empty_list).
