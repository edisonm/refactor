:- begin_tests(refactor).

:- include(refactor_common).

:- use_module(ex27).

/* $ex27$
diff -ruN ex27.pl -
--- ex27.pl (source)
+++ ex27.pl (target)
@@ -6,8 +6,8 @@
 
 ex27 :-
     % test1
-    X = (5,2), % test2
-    b(X).
+    % test2
+    b((5,2)).
 
 ex27_2 :-
     @@(_A, _B).
*/

test(ex27) :-
    execute_test(ex27, replace_term((ex27:- (A=V,Body)), (ex27 :- (Body1@@Body)@@(A=V,Body)),
                                    substitute_value(A, V, Body, Body1))).

/* $ex27_2$
diff -ruN ex27.pl -
--- ex27.pl (source)
+++ ex27.pl (target)
@@ -9,7 +9,6 @@
     X = (5,2), % test2
     b(X).
 
-ex27_2 :-
-    @@(_A, _B).
+_B@@_A.
 
 @@(_, _).
*/

test(ex27_2) :-
    execute_test(ex27, ex27_2, replace_sentence((ex27_2:- @@(A,B)), (\\(@@(B,A)))), []).

:- end_tests(refactor).
