:- begin_tests(refactor).

:- include(refactor_common).

:- use_module(ex23).

/* $ex23$
diff -ruN ex23.pl -
--- ex23.pl (source)
+++ ex23.pl (target)
@@ -1,4 +1,4 @@
 :- module(ex23, [ex23/1]).
 
 ex23(X) :-
-    X is 2+6.
+    X is 2+1*6.
*/

test(ex23) :-
    execute_test(ex23, replace_term(A+B, A+(1*B))).

:- end_tests(refactor).
