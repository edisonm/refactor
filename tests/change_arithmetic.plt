:- begin_tests(change_arithmetic).

:- include(refactor_common).

:- use_module(change_arithmetic).

/* $change_arithmetic$
diff -ruN change_arithmetic.pl -
--- change_arithmetic.pl (source)
+++ change_arithmetic.pl (target)
@@ -1,4 +1,4 @@
 :- module(change_arithmetic, [change_arithmetic/1]).
 
 change_arithmetic(X) :-
-    X is 2+6.
+    X is 2+1*6.
*/

test(change_arithmetic) :-
    execute_test(change_arithmetic, replace_term(A+B, A+(1*B))).

:- end_tests(change_arithmetic).
