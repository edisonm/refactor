:- begin_tests(repl_operator).

:- include(refactor_common).

/* $repl_operator$
diff -ruN repl_operator.pl -
--- repl_operator.pl (source)
+++ repl_operator.pl (target)
@@ -1,4 +1,4 @@
 :- module(repl_operator, [repl_operator/1]).
 
 repl_operator(X) :-
-    X is 1 +  2 +   3 +    4.
+    X is ((1 ^  2) ^   3) ^    4.
*/

test(repl_operator) :-
    execute_test(repl_operator, replace_term(A+B, A^B),[files(repl_operator)]).

:- end_tests(repl_operator).
