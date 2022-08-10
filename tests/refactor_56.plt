:- begin_tests(refactor_56).

:- include(refactor_common).

/* $operators$
diff -ruN operators.pl -
--- operators.pl (source)
+++ operators.pl (target)
@@ -1 +1,2 @@
-f([a- ~t, b]).
+f([ a- ~t,
+    b]).
*/

test(operators) :-
    execute_test(operators, replace_term(f(A),f('$LISTB,NL'(A))),[file(operators)]).

:- end_tests(refactor_56).
