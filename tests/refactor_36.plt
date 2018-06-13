:- begin_tests(refactor_36).

:- include(refactor_common).

:- use_module(opex2).

/* $opex2$
diff -ruN opex2.pl -
--- opex2.pl (source)
+++ opex2.pl (target)
@@ -7,5 +7,5 @@
 A myis B :- display(A myis B), nl.
 
 opex2(A, B) :-
-    A myis B.
+    A myis2 B.
 
*/
test(opex2) :-
    execute_test(opex2, opex2,
                 replace(goal, myis(A, B), myis2(A, B), true), []).

:- end_tests(refactor_36).
