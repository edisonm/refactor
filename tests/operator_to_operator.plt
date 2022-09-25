:- begin_tests(operator_to_operator).

:- include(refactor_common).

/* $operator_to_operator$
diff -ruN operator1.pl -
--- operator1.pl (source)
+++ operator1.pl (target)
@@ -7,5 +7,5 @@
 A myis B :- display(A myis B), nl.
 
 operator1(A, B) :-
-    A myis B.
+    A myis2 B.
 
*/

test(operator_to_operator) :-
    execute_test(operator_to_operator, replace_goal(myis(A, B), myis2(A, B)), [files(operator1)]).

:- end_tests(operator_to_operator).
