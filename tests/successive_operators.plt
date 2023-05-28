:- begin_tests(successive_operators).

:- include(refactor_common).

/* $successive_operators$
diff -ruN successive_operators.pl -
--- successive_operators.pl (source)
+++ successive_operators.pl (target)
@@ -1,2 +1,2 @@
-a :- [k:p].
-a :- _{k:p}.
+a :- [k: #(p)].
+a :- _{k: #(p)}.
*/

test(successive_operators) :-
    execute_test(successive_operators, replace_term(p,#(p)),[file(successive_operators)]).

:- end_tests(successive_operators).
