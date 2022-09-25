:- begin_tests(operator_to_pred).

:- include(refactor_common).

/* $operator_to_pred$
diff -ruN operator1.pl -
--- operator1.pl (source)
+++ operator1.pl (target)
@@ -7,5 +7,5 @@
 A myis B :- display(A myis B), nl.
 
 operator1(A, B) :-
-    A myis B.
+    p(A, B).
 
*/

test(operator_to_pred) :-
    execute_test(operator_to_pred, replace_goal(myis(A, B), p(A, B)), [files(operator1)]).

:- end_tests(operator_to_pred).
