:- begin_tests(refactor).

:- include(refactor_common).

/* $opex1_1$
diff -ruN opex1.pl -
--- opex1.pl (source)
+++ opex1.pl (target)
@@ -7,5 +7,5 @@
 A myis B :- display(A myis B), nl.
 
 opex1(A, B) :-
-    A myis B.
+    p(A, B).
 
*/

test(opex1_1) :-
    execute_test(opex1_1, replace_goal(myis(A, B), p(A, B)), [files(opex1)]).

/* $opex1_2$
diff -ruN opex1.pl -
--- opex1.pl (source)
+++ opex1.pl (target)
@@ -7,5 +7,5 @@
 A myis B :- display(A myis B), nl.
 
 opex1(A, B) :-
-    A myis B.
+    A myis2 B.
 
*/

test(opex1_2) :-
    execute_test(opex1_2, replace_goal(myis(A, B), myis2(A, B)), [files(opex1)]).

:- end_tests(refactor).
