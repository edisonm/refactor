:- begin_tests(change_operator).

:- include(refactor_common).

/* $change_operator_1$
diff -ruN change_operator.pl -
--- change_operator.pl (source)
+++ change_operator.pl (target)
@@ -1,4 +1,4 @@
 :- module(change_operator, [change_operator/2]).
 
 change_operator(A, (B, c)) :-
-    A + B  : (A -> B).
+    (A + B)  ^ (A -> B).
*/

test(change_operator_1) :-
    execute_test(change_operator_1, replace_term(A:B,A^B), [files(change_operator)]).

/* $change_operator_2$
diff -ruN change_operator.pl -
--- change_operator.pl (source)
+++ change_operator.pl (target)
@@ -1,4 +1,4 @@
 :- module(change_operator, [change_operator/2]).
 
 change_operator(A, (B, c)) :-
-    A + B  : (A -> B).
+    A + B  *-> (A -> B).
*/

test(change_operator_2) :-
    execute_test(change_operator_2, replace_term((A:B),A*->B),[files(change_operator)]).

:- end_tests(change_operator).
